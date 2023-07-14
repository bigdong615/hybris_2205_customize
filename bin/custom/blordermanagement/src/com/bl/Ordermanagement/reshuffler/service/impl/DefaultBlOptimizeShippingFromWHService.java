package com.bl.Ordermanagement.reshuffler.service.impl;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.reshuffler.service.BlOptimizeShippingFromWHService;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.product.service.BlProductService;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import com.google.common.collect.Sets;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.tx.Transaction;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is used to optimize 'ship from' WH for the consignments
 * @author Moumita
 */
public class DefaultBlOptimizeShippingFromWHService implements BlOptimizeShippingFromWHService
{
  private static final Logger LOG = Logger.getLogger(DefaultBlOptimizeShippingFromWHService.class);

  private BlOrderDao orderDao;
  private BaseStoreService baseStoreService;
  private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;
  private BlCommerceStockService blCommerceStockService;
  private BlProductService blProductService;
  private BlAssignSerialService blAssignSerialService;
  private ModelService modelService;
  private BlStockLevelDao blStockLevelDao;
  private BlAllocationService blAllocationService;
  private BlConsignmentEntryService blConsignmentEntryService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void optimizeShipFormWHForOrders(final Date currentDate) {
    final List<AbstractOrderModel> ordersToBeProcessed = getOrderDao()
        .getOrdersToOptimizeShipFromWH(currentDate);
    if(CollectionUtils.isNotEmpty(ordersToBeProcessed)) {
      optimizeShipFromWHForOrders(ordersToBeProcessed);
    }
  }

  private void optimizeShipFromWHForOrders(final List<AbstractOrderModel> ordersToBeProcessed) {
    final BaseStoreModel baseStoreModel = getBaseStoreService()
        .getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
    //Get all warehouses
    final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();
    ordersToBeProcessed.forEach(order -> {
      Transaction tx = Transaction.current();
      tx.enableDelayedStore(false);
      boolean fulfillmentCompleted = false;
      try {
        tx.begin();
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "Optimizing ship from WH for the order {} ", order.getCode());
        //It gets the preferred warehouse
        final WarehouseModel location = getBlDeliveryStateSourcingLocationFilter()
            .applyFilter(order);
        fulfillmentCompleted = fulfillFromWH(location, order, warehouses);
      } catch (final Exception ex) {
        BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Exception occurred while optimizing ship from WH for the order {} through BlReshufflerJob {} ",
            order.getCode(), ex);
        BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while optimizing ship from WH for the order", ex);
      } finally {
        if(fulfillmentCompleted){
          tx.commit();
          BlLogger.logFormatMessageInfo(LOG, Level.INFO,
              "Optimizing ship from WH is successful for the order {} ", order.getCode());
        }else{
          tx.rollback();
          BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
              "Optimizing ship from WH for is not successful for the order {} ", order.getCode());
        }
      }
    });
  }

  /**
   * {@inheritDoc}
   * @param warehouses list of warehouse
   * @param preferredWH the preferred warehouse
   * @return the warehouse model
   */
  public WarehouseModel getAnotherWarehouse(final Collection<WarehouseModel> warehouses,
      final WarehouseModel preferredWH) {
    final Optional<WarehouseModel> warehouse = warehouses.stream()
        .filter(warehouseModel -> !(warehouseModel.getCode()
            .equals(preferredWH.getCode()))).findFirst();
    return warehouse.isPresent() ? warehouse.get() : null;
  }

  /**
   * {@inheritDoc}
   * @param order the order
   * @param otherWH the other warehouse
   * @return list of product code
   */
  public Set<String> modifyProductCodes(final AbstractOrderModel order,
      final WarehouseModel otherWH) {
    final Set<String> products = new HashSet<>();
    final Optional<ConsignmentModel> consignment = order.getConsignments().stream()
        .filter(consignmentModel ->
            consignmentModel.getWarehouse().getCode().equals(otherWH.getCode())).findAny();
    if (consignment.isPresent()) {
      consignment.get().getConsignmentEntries().forEach(consignmentEntryModel ->
          consignmentEntryModel.getSerialProducts().forEach(serialProduct -> {
            if (serialProduct instanceof BlSerialProductModel) {
              products.add(((BlSerialProductModel) serialProduct).getBlProduct().getCode());
            }
          }));
    }
    return products;
  }

  /**
   * {@inheritDoc}
   * @param order the order
   * @param modifiedProductCodes the list of product code
   * @return list of order entry model
   */
  public Collection<AbstractOrderEntryModel> getOrderEntries(final AbstractOrderModel order,
      final Set<String> modifiedProductCodes) {
    return order.getEntries().stream()
        .filter(entry -> modifiedProductCodes.contains(entry.getProduct()
            .getCode())).collect(Collectors.toList());
  }

  /**
   * It fulfills the products present in the order from a single WH if stock is there
   * @param location
   * @param order
   * @param warehouses
   * @return boolean
   */
  private boolean fulfillFromWH(final WarehouseModel location, final AbstractOrderModel order, final List<WarehouseModel> warehouses) {
    final WarehouseModel anotherWH = getAnotherWarehouse(warehouses, location);
    final boolean noSplitting = checkFulfillmentFromSingleWH(order, anotherWH, location);
    if(!noSplitting) {
      return checkFulfillmentFromSingleWH(order, location, anotherWH);
    }
    return true;
  }

  /**
   * It checks whether all the products of the order can be fulfilled from a single warehouse
   * @param order the order
   * @param warehouse the warehouse
   * @param preferredWH the preferred warehouse
   * @return boolean
   */
  private boolean checkFulfillmentFromSingleWH(final AbstractOrderModel order, final WarehouseModel warehouse,
      final WarehouseModel preferredWH) {
    final Set<String> modifiedProductCodes = modifyProductCodes(order, warehouse);
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "list of products {} to fulfill from preferred warehouse {} for the order {}",
        modifiedProductCodes.toString(), preferredWH.getCode(), order.getCode());
    if (CollectionUtils.isNotEmpty(modifiedProductCodes)) {
    final Collection<StockLevelModel> stockLevels = getStocks(modifiedProductCodes, preferredWH,
        order);
    final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
        .groupBySkuProductWithAvailability(stockLevels);
      if (MapUtils.isNotEmpty(availabilityMap) && isSourcingNoSplittingPossible(
          modifiedProductCodes,
          availabilityMap, order, warehouse)) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "all the products can be fulfilled from this warehouse {} for the order {}",
            preferredWH.getCode(), order.getCode());
        final Collection<AbstractOrderEntryModel> orderEntries = getOrderEntries(order,
            modifiedProductCodes);
        final SourcingContext context = createSourcingContext(orderEntries);
        createSourcingLocation(availabilityMap, preferredWH,
            context);
        assignSerialFromLocation(context, warehouse);
        deleteOtherConsignmentIfAny(order, warehouse);
        createConsignment(order, context, preferredWH);
        return true;
      }
    }
    return false;
  }

  /**
   * It deletes the consignment if all the products can be fulfilled from the other warehouse
   * @param order the order
   * @param anotherWH another warehouse
   */
  public void deleteOtherConsignmentIfAny(final AbstractOrderModel order,
      final WarehouseModel anotherWH) {
    final ConsignmentModel consignmentModel = getConsignment(anotherWH, order);
    final Set<String> productCodes = new HashSet<>();
    if(Objects.nonNull(consignmentModel)) {
      final Iterator<ConsignmentEntryModel> consignmentEntries = consignmentModel.getConsignmentEntries().iterator();
      while (consignmentEntries.hasNext()) {
        final ConsignmentEntryModel consignmentEntryModel = consignmentEntries.next();
        consignmentEntryModel.getSerialProducts().forEach(serialProduct -> {
          if (serialProduct instanceof BlSerialProductModel) {
            productCodes.add(((BlSerialProductModel) serialProduct).getCode());
          }
          final AbstractOrderEntryModel orderEntry = consignmentEntryModel.getOrderEntry();
          final List<BlProductModel> serialProducts = consignmentEntryModel.getSerialProducts();
          final List<BlProductModel> serialProductList = orderEntry.getSerialProducts();
          final List<BlProductModel> updatedSerialProducts = new ArrayList<>(serialProductList);
          updatedSerialProducts.removeAll(serialProducts);
          orderEntry.setSerialProducts(updatedSerialProducts);
          BlLogger.logFormatMessageInfo(LOG, Level.INFO,
              "It updates the serial product list {} for the order entry {} of the order {} ",
              serialProductList.toString(), orderEntry, order.getCode());
          getModelService().save(orderEntry);
        });
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "Consignment Entry to be deleted {} ", consignmentEntryModel);
        getModelService().remove(consignmentEntryModel);
      }
      if (CollectionUtils.isNotEmpty(productCodes)) {
        final Collection<StockLevelModel> stocksWithTrueStatus = getBlStockLevelDao()
            .findSerialStockLevelsForDateAndCodes(productCodes,
                consignmentModel.getOptimizedShippingStartDate(),
                consignmentModel.getOptimizedShippingEndDate(), Boolean.TRUE);
        final Collection<StockLevelModel> stockWithFalseStatus = getBlStockLevelDao()
                .findSerialStockLevelsForDateAndCodes(productCodes,
                        consignmentModel.getOptimizedShippingEndDate(),
                        consignmentModel.getOptimizedShippingEndDate(), Boolean.FALSE);

        final Collection<StockLevelModel> stocks = Stream
                .concat(stocksWithTrueStatus.stream(), stockWithFalseStatus.stream())
                .collect(Collectors.toList());

        stocks.forEach(stock -> {
          if(null != stock.getOrder() && stock.getDate().equals(consignmentModel.getOptimizedShippingStartDate()) && stock.getOrder().split(",").length > 1){
            stock.setReservedStatus(false);
            String[] orders = stock.getOrder().split(",");
            List<String> arr_new = Arrays.asList(orders);
            List<String> updateOrders = arr_new.stream().filter(lst -> !lst.equals(consignmentModel.getOrder().getCode())).collect(Collectors.toList());
            stock.setOrder(String.join(",",updateOrders));
          }
          else if(null != stock.getOrder() && stock.getDate().equals(consignmentModel.getOptimizedShippingEndDate()) && stock.getOrder().split(",").length > 1){
            stock.setReservedStatus(true);
            String[] orders = stock.getOrder().split(",");
            List<String> arr_new = Arrays.asList(orders);
            List<String> updateOrders = arr_new.stream().filter(lst -> !lst.equals(consignmentModel.getOrder().getCode())).collect(Collectors.toList());
            stock.setOrder(String.join(",",updateOrders));
          }
          else {
            stock.setReservedStatus(false);
            stock.setOrder(null);
          }
          BlLogger.logFormatMessageInfo(LOG, Level.INFO,
              "Stock status is changed to {} for the serial product {} ", stock.getReservedStatus(),
              stock.getSerialProductCode());
        });
        this.getModelService().saveAll(stocks);
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "Consignment to be deleted {} ", consignmentModel.getCode());
        getModelService().remove(consignmentModel);
        getModelService().refresh(order);
      }
    }
  }


  /**
   * It assigns the serial as per serial priority rules
   * @param context
   * @param warehouseModel
   */
  private void assignSerialFromLocation(final SourcingContext context, final WarehouseModel warehouseModel) {
    final List<AtomicBoolean> allEntrySourceComplete = new ArrayList<>();
    SourcingResult result = new SourcingResult();
    final SourcingLocation finalSourcingLocation = context.getSourcingLocations().iterator().next();
    context.getOrderEntries().forEach(entry -> {
      List<BlProductModel> entries = new ArrayList<>();
      entries = entry.getSerialProducts().stream()
        .filter(serialProduct ->
            serialProduct instanceof BlSerialProductModel
                && ((BlSerialProductModel) serialProduct).getWarehouseLocation()
                  .getCode().equals(warehouseModel.getCode())).collect(Collectors.toList());
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "The number of products {} to consider from the other warehouse {} ",
          entries.size(), warehouseModel.getCode());
      Long quantity = Long.valueOf(entries.size());
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "The quantity {} to be fulfilled for the order entry {} ",
            quantity, entry);
      getBlAssignSerialService()
          .fulfillEachEntry(context, result, finalSourcingLocation, entry, allEntrySourceComplete,
              quantity);
    });
  }

  /**
   * {@inheritDoc}
   * @param order the order
   * @param context the context
   * @param warehouseModel the warehouse
   */
  public void createConsignment(final AbstractOrderModel order, final SourcingContext context,
      final WarehouseModel warehouseModel) {
    final Set<SourcingResult> sourcingResults = context.getResult().getResults();
    final SourcingResult result =
        CollectionUtils.isNotEmpty(sourcingResults) ? sourcingResults.iterator().next() :
            new SourcingResult();
    final ConsignmentModel consignment = getConsignment(warehouseModel, order);
    if (Objects.isNull(consignment)) {
      final ConsignmentModel newConsignment = getBlAllocationService().createConsignment(order,
          BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + order.getCode()
              + BlOrdermanagementConstants.UNDER_SCORE
              + order.getConsignments().size(),
          result);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "A new consignment has been created {} ", newConsignment.getCode());
    } else {
      final List<AbstractOrderEntryModel> orderEntries = new ArrayList<>();
      context.getOrderEntries().forEach(orderEntry ->
          consignment.getConsignmentEntries().forEach(consignmentEntryModel -> {
            if (consignmentEntryModel.getOrderEntry().equals(orderEntry)) {
              updateConsignmentEntry(consignmentEntryModel, result, orderEntry);
              orderEntries.add(orderEntry);
              BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                  "This consignment entry already exists {} of the consignment {}", consignmentEntryModel.getPk(), consignment.getCode());
            }
          }));
      final List<AbstractOrderEntryModel> contextOrderEntries = new ArrayList<>(context.getOrderEntries());
      contextOrderEntries.removeAll(orderEntries);
      context.setOrderEntries(contextOrderEntries);
      final SourcingResult sourcingResult = context.getResult().getResults().iterator().next();
      final Set<ConsignmentEntryModel> entries = context.getOrderEntries().stream()
          .map(entryModel ->
              getBlAllocationService()
                  .createConsignmentEntry(entryModel, Long.valueOf(result.getSerialProductMap()
                          .get(entryModel.getEntryNumber()).size()), consignment,
                      sourcingResult)
          ).collect(Collectors.toSet());
      entries.forEach( consignmentEntryModel -> {
        final Set<BlSerialProductModel> serialProductModels =
            null == result.getSerialProductMap() ? new HashSet<>() : result.getSerialProductMap()
                .get(consignmentEntryModel.getOrderEntry().getEntryNumber());
        reserveStocksForSerialProductsThroughReshuffler(serialProductModels, consignmentEntryModel);
      });
      entries.addAll(consignment.getConsignmentEntries());
      consignment.setConsignmentEntries(entries);
      consignment.setStatus(ConsignmentStatus.READY);
      getModelService().save(consignment);
    }
  }

  /**
   * {@inheritDoc}
   * @param productCodes the product codes
   * @param location the warehouse
   * @param order the order
   * @return list of stock level model
   */
  public Collection<StockLevelModel> getStocks(final Set<String> productCodes,
      final WarehouseModel location,
      final AbstractOrderModel order) {
    final ConsignmentModel consignment = getConsignment(location, order);
    final Date shippingStartDate =
        Objects.nonNull(consignment) ? consignment.getOptimizedShippingStartDate() :
            order.getActualRentalStartDate();
    final Date shippingEndDate =
        Objects.nonNull(consignment) ? consignment.getOptimizedShippingEndDate() :
            order.getActualRentalEndDate();
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Stocks to be fetched from start {} to end date {} for the warehouse {} of the order {} for the products {} ",
        shippingStartDate, shippingEndDate, location, order.getCode(), productCodes);
    return getBlCommerceStockService()
        .getStockForProductCodesAndDate(productCodes,
            location, shippingStartDate, shippingEndDate);
  }

  /**
   * {@inheritDoc}
   * @param entry the entry
   * @param result the sourcing result
   * @param orderEntry the order entry
   */
  public void updateConsignmentEntry(final ConsignmentEntryModel entry,
      final SourcingResult result,
      final AbstractOrderEntryModel orderEntry) {
    final List<BlProductModel> consignmentEntrySerialProducts =
        null == entry.getSerialProducts() ? new ArrayList<>() : entry.getSerialProducts();
    final List<BlProductModel> associatedSerialProducts = new ArrayList<>(consignmentEntrySerialProducts);

    final Set<BlSerialProductModel> serialProductModels =
        null == result.getSerialProductMap() ? new HashSet<>() : result.getSerialProductMap()
            .get(orderEntry.getEntryNumber());

    if (CollectionUtils.isNotEmpty(serialProductModels)) {

      associatedSerialProducts.addAll(serialProductModels);

      final Set<BlSerialProductModel> serialProducts = new HashSet<>();
      associatedSerialProducts.forEach(serial -> {
        if(serial instanceof BlSerialProductModel) {
          serialProducts.add((BlSerialProductModel) serial);
        }
      });
      orderEntry.setSerialProducts(new ArrayList<>(serialProducts));
      entry.setQuantity(Long.valueOf(serialProducts.size()));
      entry.setSerialProducts(
          new ArrayList<>(serialProducts));   //setting serial products from result
      getBlConsignmentEntryService().setItemsMap(entry, serialProducts);
      getBlAllocationService().setSerialCodesToBillingCharges(entry, serialProducts);
      getModelService().save(entry);
      reserveStocksForSerialProductsThroughReshuffler(serialProductModels, entry);
    }
  }

  /**
   * {@inheritDoc}
   * @param serialProductModels set of serial product model
   * @param entry the consignment entry
   */
  public void reserveStocksForSerialProductsThroughReshuffler(final Set<BlSerialProductModel> serialProductModels,
      final ConsignmentEntryModel entry) {
    final Set<String> allocatedProductCodes = new HashSet<>();
    allocatedProductCodes
        .addAll(serialProductModels.stream().map(BlSerialProductModel::getCode).collect(
            Collectors.toSet()));
    final Collection<StockLevelModel> serialStocks = blStockLevelDao
        .findSerialStockLevelsForDateAndCodes(allocatedProductCodes, entry.getConsignment().getOptimizedShippingStartDate(),
            entry.getConsignment().getOptimizedShippingEndDate(), Boolean.FALSE);
    if (CollectionUtils.isNotEmpty(serialStocks) && serialStocks.stream()
        .allMatch(stock -> allocatedProductCodes.contains(stock.getSerialProductCode()))) {
      serialStocks.forEach(stock -> {
        if(stock.getDate().equals(entry.getConsignment().getOptimizedShippingStartDate()) && StringUtils.isNotBlank(stock.getOrder())){
          stock.setReservedStatus(true);
          stock.setOrder(stock.getOrder()+ "," + entry.getOrderEntry().getOrder().getCode());
        }
        else if(stock.getDate().equals(entry.getConsignment().getOptimizedShippingEndDate()) && StringUtils.isNotBlank(stock.getOrder())){
          stock.setReservedStatus(true);
          stock.setOrder(stock.getOrder()+ "," + entry.getOrderEntry().getOrder().getCode());
        }
        else {
          stock.setReservedStatus(true);
          stock.setOrder(entry.getOrderEntry().getOrder().getCode());
        }
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "Stock status is changed to {} for the serial product {} ", stock.getReservedStatus(),
            stock.getSerialProductCode());
      });
      this.getModelService().saveAll(serialStocks);
    }
  }

  /**
   * {@inheritDoc}
   * @param location the warehouse
   * @param order the order
   * @return the consignment model
   */
  public ConsignmentModel getConsignment(final WarehouseModel location,
      final AbstractOrderModel order) {
    final Optional<ConsignmentModel> consignment = order.getConsignments().stream()
        .filter(consignmentModel ->
            consignmentModel.getWarehouse().getCode().equals(location.getCode())).findFirst();
    if (consignment.isPresent()) {
      return consignment.get();
    }
    return null;
  }

  /**
   * {@inheritDoc}
   * @param skuProduct the product
   * @param availabilityMap the availability map
   * @return the available quantity
   */
  public Long getAvailabilityForProduct(final String skuProduct, final
  Map<String, List<StockLevelModel>> availabilityMap) {
    Long stockLevel = 0L;
    if (MapUtils.isNotEmpty(availabilityMap)) {
      final List<StockLevelModel> stockLevelList =
          Objects.isNull(availabilityMap.get(skuProduct)) ? Collections.emptyList() :
              availabilityMap.get(skuProduct);
      final Set<String> serialProductCodes = stockLevelList.stream()
          .map(StockLevelModel::getSerialProductCode).collect(Collectors.toSet());

      stockLevel = Long.valueOf(serialProductCodes.size());
    }
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "available quantity {} for the product {} ", stockLevel, skuProduct);
    return stockLevel;
  }

  /**
   * It checks if the whole order fulfillment from a single warehouse if possible or not
   * @param modifiedProductCodes list of product code
   * @param availabilityMap the availability map
   * @param order the order
   * @param warehouseModel the warehouse
   * @return boolean
   */
  private boolean isSourcingNoSplittingPossible(final Set<String> modifiedProductCodes, final
  Map<String, List<StockLevelModel>> availabilityMap, final AbstractOrderModel order,
      final WarehouseModel warehouseModel) {
    return modifiedProductCodes.stream().allMatch(skuProduct -> {
      final Optional<AbstractOrderEntryModel> orderEntry = order.getEntries().stream()
          .filter(entry ->
              entry.getProduct().getCode()
                  .equals(skuProduct)).findFirst();
      final AbstractOrderEntryModel orderEntryModel = orderEntry.get();
      final Long availableQty = getBlProductService().isAquatechProduct(orderEntryModel.getProduct()) ? orderEntryModel
          .getQuantity() : getAvailabilityForProduct(skuProduct, availabilityMap);
      final List<BlProductModel> entries = orderEntryModel.getSerialProducts().stream()
          .filter(serialProduct ->
              serialProduct instanceof BlSerialProductModel
                  && ((BlSerialProductModel) serialProduct).getWarehouseLocation()
                  .getCode().equals(warehouseModel.getCode())).collect(Collectors.toList());
      return entries.size() <= availableQty;
    });
  }

  /**
   * {@inheritDoc}
   * @param availabilityMap the availability map
   * @param warehouseModel the warehouse model
   * @param context the context
   * @return Sourcing location
   */
  public SourcingLocation createSourcingLocation(
      final Map<String, List<StockLevelModel>> availabilityMap,
      final WarehouseModel warehouseModel, final SourcingContext context) {
    final SourcingLocation sourcingLocation = new SourcingLocation();
    sourcingLocation.setWarehouse(warehouseModel);
    sourcingLocation.setContext(context);
    final Set<SourcingLocation> sourcingLocations = new HashSet<>();
    sourcingLocations.add(sourcingLocation);
    context.setSourcingLocations(sourcingLocations);
    sourcingLocation.setAvailabilityMap(availabilityMap);
    return sourcingLocation;
  }

  /**
   * {@inheritDoc}
   * @param orderEntries the order entries
   * @return sourcing context
   */
  public SourcingContext createSourcingContext(
      final Collection<AbstractOrderEntryModel> orderEntries) {
    final SourcingContext context = new SourcingContext();
    context.setOrderEntries(orderEntries);
    final SourcingResults result = new SourcingResults();
    result.setResults(Sets.newHashSet());
    result.setComplete(Boolean.FALSE);
    context.setResult(result);
    return context;
  }

  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(final BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }

  public BaseStoreService getBaseStoreService() {
    return baseStoreService;
  }

  public void setBaseStoreService(BaseStoreService baseStoreService) {
    this.baseStoreService = baseStoreService;
  }

  public BlDeliveryStateSourcingLocationFilter getBlDeliveryStateSourcingLocationFilter() {
    return blDeliveryStateSourcingLocationFilter;
  }

  public void setBlDeliveryStateSourcingLocationFilter(
      BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter) {
    this.blDeliveryStateSourcingLocationFilter = blDeliveryStateSourcingLocationFilter;
  }

  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }


  /**
   * @return the blProductService
   */
  public BlProductService getBlProductService()
  {
    return blProductService;
  }

  /**
   * @param blProductService the blProductService to set
   */
  public void setBlProductService(BlProductService blProductService)
  {
    this.blProductService = blProductService;
  }

  public BlAssignSerialService getBlAssignSerialService() {
    return blAssignSerialService;
  }

  public void setBlAssignSerialService(
      BlAssignSerialService blAssignSerialService) {
    this.blAssignSerialService = blAssignSerialService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

  public BlAllocationService getBlAllocationService() {
    return blAllocationService;
  }

  public void setBlAllocationService(
      BlAllocationService blAllocationService) {
    this.blAllocationService = blAllocationService;
  }

  public BlConsignmentEntryService getBlConsignmentEntryService() {
    return blConsignmentEntryService;
  }

  public void setBlConsignmentEntryService(
      BlConsignmentEntryService blConsignmentEntryService) {
    this.blConsignmentEntryService = blConsignmentEntryService;
  }
}
