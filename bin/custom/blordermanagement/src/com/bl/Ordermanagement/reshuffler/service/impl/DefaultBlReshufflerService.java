package com.bl.Ordermanagement.reshuffler.service.impl;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.reshuffler.service.BlReshufflerService;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
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
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
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
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is to allocate the unallocated products
 *
 * @author Moumita
 */
public class DefaultBlReshufflerService implements BlReshufflerService {

  private static final Logger LOG = Logger.getLogger(DefaultBlReshufflerService.class);
  private BlOrderDao orderDao;
  private BlCommerceStockService blCommerceStockService;
  private BaseStoreService baseStoreService;
  private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;
  private ModelService modelService;
  private BlAssignSerialService blAssignSerialService;
  private BlAllocationService blAllocationService;
  private BlStockLevelDao blStockLevelDao;
  private BlConsignmentEntryService blConsignmentEntryService;
  private BlProductService blProductService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void processIncompleteOrders() {
    final Date currentDate = Date
        .from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
    final Date endDate = DateUtils.addDays(currentDate, 2);
    for (Date startDate = currentDate; startDate.before(endDate); startDate = DateUtils.addDays(startDate, 1)) {
      processOrdersByDay(startDate, startDate.equals(currentDate));
    }
  }

  /**
   * It processed the orders day wise
   * @param currentDate the date
   * @param isPresentDay
   */
  public void processOrdersByDay(final Date currentDate, final boolean isPresentDay) {
    final List<AbstractOrderModel> ordersToBeProcessed = getOrderDao()
        .getIncompleteOrdersToBeProcessed(currentDate);
    if(CollectionUtils.isNotEmpty(ordersToBeProcessed)) {
      //Sorted by delivery mode
      final Set<AbstractOrderModel> removeDuplicateOrders = new HashSet<>(ordersToBeProcessed);
      final List<AbstractOrderModel> ordersSortedByDeliveryMode = removeDuplicateOrders.stream()
          .filter(order -> order.getDeliveryMode() instanceof BlPickUpZoneDeliveryModeModel)
          .collect(Collectors.toList());
      final List<AbstractOrderModel> remainingOrders = new ArrayList<>(removeDuplicateOrders);
      remainingOrders.removeAll(ordersSortedByDeliveryMode);
      //Sort the orders by order total price
      final List<AbstractOrderModel> ordersSortedByTotalPrice = remainingOrders.stream()
          .sorted(Comparator.comparing(AbstractOrderModel::getTotalPrice).reversed())
          .collect(Collectors.toList());
      final List<AbstractOrderModel> finalSortedOrders = new ArrayList<>();
      finalSortedOrders.addAll(ordersSortedByDeliveryMode);
      finalSortedOrders.addAll(ordersSortedByTotalPrice);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "List of orders to fulfill {} for the day {} ", finalSortedOrders.toString(), currentDate);
      final BaseStoreModel baseStoreModel = getBaseStoreService()
          .getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
      //Get all warehouses
      final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();
      //It filters the orders which needs to be processed and ignore the orders which contains the SKU (when total number
      // of sku needed for orders, will ship on same day, is not sufficient to fulfill from main and buffer inventory
      final Map<AbstractOrderModel, Set<String>> filteredOrders = filterOrdersForProcessing(
          finalSortedOrders,
          warehouses, currentDate, isPresentDay);
         processOrders(filteredOrders, warehouses);
    }
  }

  /**
   * It processes the orders which can be fulfilled
   *
   * @param filteredOrders the orders to be fulfilled
   * @param warehouses     the warehouse
   */
  private void processOrders(final Map<AbstractOrderModel, Set<String>> filteredOrders,
      final List<WarehouseModel> warehouses) {
      filteredOrders.entrySet().forEach(entry -> {
        Transaction tx = Transaction.current();
        tx.enableDelayedStore(false);
        boolean fulfillmentCompleted = false;
        try {
        tx.begin();
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "Processing the order {} ", entry.getKey().getCode());
        //It gets the preferred warehouse
        final WarehouseModel location = getBlDeliveryStateSourcingLocationFilter()
            .applyFilter(entry.getKey());
        fulfillmentCompleted = fulfillFromWH(location, entry, warehouses);
        } catch (final Exception ex) {
          BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Exception occurred while fulfilling the order {} through BlReshufflerJob {} ",
              entry.getKey().getCode(), ex);
          ex.printStackTrace();
        } finally {
          if(fulfillmentCompleted){
            tx.commit();
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                "Order fulfillment is successful for the order {} ", entry.getKey().getCode());
          }else{
            tx.rollback();
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                "Order fulfillment is not successful for the order {} ", entry.getKey().getCode());
          }
        }
      });
  }

  /**
   * It gets the other warehouse which is not preferred as per state mapping of delivery address
   *
   * @param warehouses list of warehouse
   * @param preferredWH the preferred warehouse
   * @return the warehouse model
   */
  private WarehouseModel getAnotherWarehouse(final Collection<WarehouseModel> warehouses,
      final WarehouseModel preferredWH) {
    final Optional<WarehouseModel> warehouse = warehouses.stream()
        .filter(warehouseModel -> !(warehouseModel.getCode()
            .equals(preferredWH.getCode()))).findFirst();
    return warehouse.isPresent() ? warehouse.get() : null;
  }

  /**
   * It fulfills the orders from warehouses as per the availability
   *
   * @param location the preferred warehouse
   * @param entry the map with orders and associated unallocated products
   * @param warehouses list of warehouses
   */
  private boolean fulfillFromWH(final WarehouseModel location,
      final Entry<AbstractOrderModel, Set<String>> entry,
      final List<WarehouseModel> warehouses) {
    final AbstractOrderModel order = entry.getKey();
    final Set<String> productCodes = entry.getValue();
    final WarehouseModel anotherWH = getAnotherWarehouse(warehouses, location);
    final Set<String> modifiedProductCodes = modifyProductCodes(order, anotherWH);
    boolean noSplitting = false;
    modifiedProductCodes.addAll(productCodes);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO,
				"list of products {} to fulfill from preferred warehouse {} for the order {}",
				modifiedProductCodes.toString(), location.getCode(), order.getCode());
    final Collection<StockLevelModel> stockLevels = getStocks(modifiedProductCodes, location,
        entry.getKey());
    final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
        .groupBySkuProductWithAvailability(stockLevels);
    noSplitting = MapUtils.isNotEmpty(availabilityMap) ? isSourcingNoSplittingPossible(modifiedProductCodes,
        availabilityMap, order, anotherWH) : false;
    if (noSplitting) {
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"all the products can be fulfilled from this warehouse {} for the order {}",
					location.getCode(), order.getCode());
      final Collection<AbstractOrderEntryModel> orderEntries = getOrderEntries(order,
          modifiedProductCodes);
      final SourcingContext context = createSourcingContext(orderEntries);
      createSourcingLocation(availabilityMap, location,
          context);
      assignSerialFromLocation(context, true, anotherWH);
      deleteOtherConsignmentIfAny(order, anotherWH);
      createConsignment(order, context, location);
      setOrderStatus(order);
    } else {
      final Set<String> modifiedProductForOtherWH = modifyProductCodes(order, location);
      modifiedProductForOtherWH.addAll(productCodes);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"list of products {} to fulfill from this warehouse {} for the order {}",
					modifiedProductForOtherWH.toString(), anotherWH, order.getCode());
      final Collection<StockLevelModel> stockLevelsFromOtherWh = getStocks(
          modifiedProductForOtherWH,
          anotherWH, entry.getKey());
      final Map<String, List<StockLevelModel>> availabilityMapForOtherWH = getBlCommerceStockService()
          .groupBySkuProductWithAvailability(stockLevelsFromOtherWh);
      noSplitting = MapUtils.isNotEmpty(availabilityMapForOtherWH) ? isSourcingNoSplittingPossible(modifiedProductForOtherWH,
          availabilityMapForOtherWH, order, location) : false;
      if (noSplitting) {
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"all the products can be fulfilled from this warehouse {} for the order {}",
						anotherWH, order.getCode());
        final Collection<AbstractOrderEntryModel> orderEntries = getOrderEntries(order,
            modifiedProductForOtherWH);
        final SourcingContext context = createSourcingContext(orderEntries);
        createSourcingLocation(availabilityMapForOtherWH,
            anotherWH, context);
        assignSerialFromLocation(context, true, location);
        deleteOtherConsignmentIfAny(order, location);
        createConsignment(order, context, anotherWH);
        setOrderStatus(order);
      }
    }
    if (!noSplitting) {
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"all the products can not be fulfilled from single warehouse for the order {}",
					order.getCode());
      fulfillFromMultipleWarehouses(entry, location, anotherWH, productCodes);
    }
    return true;
  }

  /**
   * It sets the order status to Ready if all the unallocated products are fulfilled
   * through reshuffler job
   * @param order the order
   */
  private void setOrderStatus(final AbstractOrderModel order) {
    final boolean allQuantityFulfilled = order.getEntries().stream().allMatch(entry -> {
        setUnallocatedQtyToZero(entry);
       return entry.getQuantity() == entry.getSerialProducts().size();
    });
    if(allQuantityFulfilled) {
      order.setStatus(OrderStatus.READY);
      getModelService().save(order);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "All the unallocated products are fulfilled for the order {}, hence the status is set to {} ",
          order.getCode(), order.getStatus().getCode());
    }
  }

  /**
   * It sets unallocated quantity to zero if all the unallocated products are fulfilled
   * @param entry the order entry
   */
  private void setUnallocatedQtyToZero(final AbstractOrderEntryModel entry) {
    if(entry.getUnAllocatedQuantity() > 0 && entry.getQuantity() == entry.getSerialProducts().size()) {
      entry.setUnAllocatedQuantity(0L);
      getModelService().save(entry);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "All the products are fulfilled of this entry {} for the order {} ",
          entry, entry.getOrder().getCode());
    }
  }

  /**
	 * It deletes the consignment if all the products can be fulfilled from the other warehouse
   * @param order the order
   * @param anotherWH another warehouse
   */
  private void deleteOtherConsignmentIfAny(final AbstractOrderModel order,
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
            "consignment to delete", consignmentEntryModel);
        getModelService().remove(consignmentEntryModel);
      }
			if (CollectionUtils.isNotEmpty(productCodes)) {
				final Collection<StockLevelModel> stocks = getBlStockLevelDao()
						.findSerialStockLevelsForDateAndCodes(productCodes,
								consignmentModel.getOptimizedShippingStartDate(),
								consignmentModel.getOptimizedShippingEndDate(), Boolean.TRUE);
				stocks.forEach(stock -> {
				  stock.setReservedStatus(false);
				  stock.setOrder(null);
          BlLogger.logFormatMessageInfo(LOG, Level.INFO,
              "Stock status is changed to {} for the serial product {} ", stock.getReservedStatus(),
              stock.getSerialProductCode());
        });
				this.getModelService().saveAll(stocks);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"consignment to delete", consignmentModel.getCode());
				getModelService().remove(consignmentModel);
				getModelService().refresh(order);
			}
		}
  }

	/**
	 * It creates sourcing location
	 * @param availabilityMap the availability map
	 * @param warehouseModel the warehouse model
	 * @param context the context
	 * @return Sourcing location
	 */
  private SourcingLocation createSourcingLocation(
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
	 * It assigns the serial as per serial priority rules
	 * @param context
	 * @param fulfilledFromSingleWH
	 * @param warehouseModel
	 */
  private void assignSerialFromLocation(final SourcingContext context,
      final boolean fulfilledFromSingleWH, final WarehouseModel warehouseModel) {
    final List<AtomicBoolean> allEntrySourceComplete = new ArrayList<>();
    SourcingResult result = new SourcingResult();
    final SourcingLocation finalSourcingLocation = context.getSourcingLocations().iterator().next();
    context.getOrderEntries().forEach(entry -> {
      List<BlProductModel> entries = new ArrayList<>();
      if (fulfilledFromSingleWH) {
        entries = entry.getSerialProducts().stream()
            .filter(serialProduct ->
                serialProduct instanceof BlSerialProductModel
                    && ((BlSerialProductModel) serialProduct).getWarehouseLocation()
                    .getCode().equals(warehouseModel.getCode())).collect(Collectors.toList());
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"The number of products {} to consider from the other warehouse {} ",
						entries.size(), warehouseModel.getCode());
      }
      Long quantity = 0L;
      final Long splitConsignmentQuantity =
          Objects.nonNull(entry.getSplitConsignmentQuantity()) ? entry.getSplitConsignmentQuantity()
              : 0L;
      if (splitConsignmentQuantity > 0L) {
        quantity = entry.getSplitConsignmentQuantity();
        entry.setSplitConsignmentQuantity(0L);
        getModelService().save(entry);

      } else {
        quantity = entry.getUnAllocatedQuantity() + entries.size();
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"The quantity {} to be fulfilled for the order entry {} ",
						quantity, entry);
      }
      getBlAssignSerialService()
          .fulfillEachEntry(context, result, finalSourcingLocation, entry, allEntrySourceComplete,
              quantity);
    });
  }

	/**
	 * It gets the order entries
	 * @param order the order
	 * @param modifiedProductCodes the list of product code
	 * @return list of order entry model
	 */
  private Collection<AbstractOrderEntryModel> getOrderEntries(final AbstractOrderModel order,
      final Set<String> modifiedProductCodes) {
    return order.getEntries().stream()
        .filter(entry -> modifiedProductCodes.contains(entry.getProduct()
            .getCode())).collect(Collectors.toList());
  }

	/**
	 * It creates sourcing context
	 * @param orderEntries the order entries
	 * @return sourcing context
	 */
  private SourcingContext createSourcingContext(
      final Collection<AbstractOrderEntryModel> orderEntries) {
    final SourcingContext context = new SourcingContext();
    context.setOrderEntries(orderEntries);
    final SourcingResults result = new SourcingResults();
    result.setResults(Sets.newHashSet());
    result.setComplete(Boolean.FALSE);
    context.setResult(result);
    return context;
  }

	/**
	 * It fulfills the order from multiple warehouses
	 * @param entry map with order entry and associated unallocated products
	 * @param location the preferred warehouse
	 * @param anotherWH the other warehouse
	 * @param productCodes list of product code
	 */
  private void fulfillFromMultipleWarehouses(final Entry<AbstractOrderModel,
      Set<String>> entry, final WarehouseModel location, final WarehouseModel anotherWH,
      final Set<String> productCodes) {
    final Map<WarehouseModel, List<String>> warehouseWithProducts = new HashMap<>();
    final AbstractOrderModel order = entry.getKey();
    final Collection<StockLevelModel> stockLevels = getStocks(productCodes, location, order);
    final Collection<StockLevelModel> stockLevelsFromOtherWH = getStocks(productCodes, anotherWH,
        order);
    final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
        .groupBySkuProductWithAvailability(stockLevels);
    final Map<String, List<StockLevelModel>> availabilityMapForOtherWH = getBlCommerceStockService()
        .groupBySkuProductWithAvailability(stockLevelsFromOtherWH);
    final AtomicBoolean allEntriesCanBeFulfilled = new AtomicBoolean(Boolean.TRUE);
    if (CollectionUtils.isNotEmpty(stockLevels) && CollectionUtils
        .isNotEmpty(stockLevelsFromOtherWH)) {
      final List<String> products = new ArrayList<>();
      final List<String> productsFromOtherWH = new ArrayList<>();
      entry.getValue().forEach(skuProduct ->
        populateProductFulfillDetailsForBothWH(availabilityMap, skuProduct, order, products,
            availabilityMapForOtherWH, productsFromOtherWH, allEntriesCanBeFulfilled));
      warehouseWithProducts.put(location, products);
      warehouseWithProducts.put(anotherWH, productsFromOtherWH);
    } else if (CollectionUtils.isNotEmpty(stockLevels)) {
      final List<String> products = new ArrayList<>();
      entry.getValue().forEach(skuProduct ->
        populateProductFulfillDetails(availabilityMap, skuProduct, order, products,
            allEntriesCanBeFulfilled));
      warehouseWithProducts.put(location, products);
    } else if (CollectionUtils.isNotEmpty(stockLevelsFromOtherWH)) {
      final List<String> productsFromOtherWH = new ArrayList<>();
      entry.getValue().forEach(skuProduct ->
        populateProductFulfillDetails(availabilityMapForOtherWH, skuProduct, order,
            productsFromOtherWH, allEntriesCanBeFulfilled));
      warehouseWithProducts.put(anotherWH, productsFromOtherWH);
    } else {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "This product {} of the order {} does not have enough stock to fulfill ", entry.getValue(), order.getCode());
    }
    if (allEntriesCanBeFulfilled.get()) {
      warehouseWithProducts.entrySet().forEach(entryPerWH -> {
        final Set<String> products = new HashSet<>();
        entryPerWH.getValue().forEach(products::add);
        if (entryPerWH.getKey().equals(location)) {
          final Collection<AbstractOrderEntryModel> entries = getOrderEntries(order, products);
          final SourcingContext context = createSourcingContext(entries);
          final SourcingLocation sourcingLocation = createSourcingLocation(availabilityMap,
              location, context);
          assignSerialFromLocation(context, false, anotherWH);
          createConsignment(order, context, location);
          setOrderStatus(order);
        } else {
          if (entryPerWH.getKey().equals(anotherWH)) {
            final Collection<AbstractOrderEntryModel> orderEntries = getOrderEntries(order,
                products);
            final SourcingContext context = createSourcingContext(orderEntries);
            final SourcingLocation sourcingLocation = createSourcingLocation(
                availabilityMapForOtherWH, anotherWH, context);
            assignSerialFromLocation(context, false, location);
            //create consignment
            createConsignment(order, context, anotherWH);
            setOrderStatus(order);
          }
        }
      });
    }
  }

	/**
	 * It populates the data when the order will be fulfilled from both of the warehouses
	 * @param availabilityMap the availability map
	 * @param skuProduct the sku product code
	 * @param order the order
	 * @param products list of product code
	 * @param availabilityMapForOtherWH the availability map from the other warehouse
	 * @param productsFromOtherWH the products from other warehouse
	 * @param allEntriesCanBeFulfilled the flag indicates whether all unallocated products of an prder
	 *                                 can be fulfilled or not
	 */
  private void populateProductFulfillDetailsForBothWH(
      final Map<String, List<StockLevelModel>> availabilityMap, final String skuProduct,
      final AbstractOrderModel order, final List<String> products,
      final Map<String, List<StockLevelModel>> availabilityMapForOtherWH,
      final List<String> productsFromOtherWH, AtomicBoolean allEntriesCanBeFulfilled) {
    final Long availableQty = getAvailabilityForProduct(skuProduct, availabilityMap);
    final Optional<AbstractOrderEntryModel> orderEntryModel = order.getEntries().stream()
        .filter(orderEntry ->
            orderEntry.getProduct().getCode()
                .equals(skuProduct)).findFirst();
    final Long availableQtyFromOtherWH = getAvailabilityForProduct(skuProduct,
        availabilityMapForOtherWH);
    if (orderEntryModel.isPresent()) {
      final AbstractOrderEntryModel orderEntry = orderEntryModel.get();
      final Long unallocatedQty = orderEntry.getUnAllocatedQuantity();
      if (unallocatedQty <= availableQty) {
        products.add(orderEntry.getProduct().getCode());
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"all quantity {} to fulfill from the preferred warehouse for the order entry {} ",
						availableQty, orderEntry);
      } else if (unallocatedQty <= availableQtyFromOtherWH) {
        productsFromOtherWH.add(orderEntry.getProduct().getCode());
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"all quantity {} to fulfill from the preferred warehouse for the order entry {} ",
						availableQtyFromOtherWH, orderEntry);
      } else if (unallocatedQty <= availableQty + availableQtyFromOtherWH) {
        products.add(orderEntry.getProduct().getCode());
        orderEntry.setSplitConsignmentQuantity(availableQty);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"{} quantity to fulfill from the preferred warehouse for the order entry {} ",
						availableQty, orderEntry);
        orderEntry.setUnAllocatedQuantity(unallocatedQty - availableQty);
        productsFromOtherWH.add(orderEntry.getProduct().getCode());
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"{} quantity to fulfill from the other warehouse for the order entry {} ",
						orderEntry.getUnAllocatedQuantity() - availableQty, orderEntry);
        getModelService().save(orderEntry);
      } else {
        allEntriesCanBeFulfilled.set(Boolean.FALSE);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"This entry {} of the order {} does not have enough stock to fulfill ", orderEntry, order.getCode());
      }
    }
  }

	/**
	 * It populates the data when the order will be fulfilled from both of the warehouses
	 * @param availabilityMap the availability map
	 * @param skuProduct the sku product code
	 * @param order the order
	 * @param allEntriesCanBeFulfilled the flag indicates whether all unallocated products of an prder
	 *                                 can be fulfilled or not
	 */
  private void populateProductFulfillDetails(
      final Map<String, List<StockLevelModel>> availabilityMap, String skuProduct,
      final AbstractOrderModel order, final List<String> productWithQty,
      AtomicBoolean allEntriesCanBeFulfilled) {
    final Long availableQty = getAvailabilityForProduct(skuProduct, availabilityMap);
    final Optional<AbstractOrderEntryModel> orderEntryModel = order.getEntries().stream()
        .filter(orderEntry ->
            orderEntry.getProduct().getCode()
                .equals(skuProduct)).findFirst();
    if (orderEntryModel.isPresent()) {
      if (orderEntryModel.get().getUnAllocatedQuantity() <= availableQty) {
        productWithQty.add(orderEntryModel.get().getProduct().getCode());
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"all quantity {} to fulfill from the preferred warehouse for the order entry {} ",
						availableQty, orderEntryModel);
      } else {
        allEntriesCanBeFulfilled.set(Boolean.FALSE);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"This entry {} of the order {} does not have enough stock to fulfill ", orderEntryModel, order.getCode());
      }
    }
  }

	/**
	 * It gets the products from the consignment of the other warehouse
	 * @param order the order
	 * @param otherWH the other warehouse
	 * @return list of product code
	 */
  private Set<String> modifyProductCodes(final AbstractOrderModel order,
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
	 * It creates a consignment
	 * @param order the order
	 * @param context the context
	 * @param warehouseModel the warehouse
	 */
  private void createConsignment(final AbstractOrderModel order, final SourcingContext context,
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
								"This consignment entry already exists {} of the consignment {}", consignmentEntryModel, consignment.getCode());
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
	 * It updates the consignment entry which is already created
	 * @param entry the entry
	 * @param result the sourcing result
	 * @param orderEntry the order entry
	 */
  private void updateConsignmentEntry(final ConsignmentEntryModel entry,
      final SourcingResult result,
      final AbstractOrderEntryModel orderEntry) {
    final List<BlProductModel> consignmentEntrySerialProducts =
        null != entry.getSerialProducts() ? entry.getSerialProducts() : new ArrayList<>();
    final List<BlProductModel> associatedSerialProducts = new ArrayList<>(consignmentEntrySerialProducts);

    final Set<BlSerialProductModel> serialProductModels =
        null != result.getSerialProductMap() ? result.getSerialProductMap()
            .get(orderEntry.getEntryNumber()) : new HashSet<>();

    if (CollectionUtils.isNotEmpty(serialProductModels)) {

      associatedSerialProducts.addAll(serialProductModels);
      entry.setSerialProducts(
          associatedSerialProducts);   //setting serial products from result
      entry.setQuantity(Long.valueOf(associatedSerialProducts.size()));

      final Set<BlSerialProductModel> serialProducts = new HashSet<>();
          associatedSerialProducts.forEach(serial -> {
          if(serial instanceof BlSerialProductModel) {
            serialProducts.add((BlSerialProductModel) serial);
          }
      });
      getBlConsignmentEntryService().setItemsMap(entry, serialProducts);
      getBlAllocationService().setSerialCodesToBillingCharges(entry, serialProducts);
      getModelService().save(entry);
      reserveStocksForSerialProductsThroughReshuffler(serialProductModels, entry);
    }
  }

  /**
   * It reserves the stocks for the assigned serial products
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
        stock.setReservedStatus(true);
        stock.setOrder(entry.getOrderEntry().getOrder().getCode());
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "Stock status is changed to {} for the serial product {} ", stock.getReservedStatus(),
            stock.getSerialProductCode());
      });
      this.getModelService().saveAll(serialStocks);
    }
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
      return orderEntryModel.getUnAllocatedQuantity() + entries.size() <= availableQty;
    });
  }

	/**
	 * It gets the availability for a product
	 * @param skuProduct the product
	 * @param availabilityMap the availability map
	 * @return the available quantity
	 */
  private Long getAvailabilityForProduct(final String skuProduct, final
  Map<String, List<StockLevelModel>> availabilityMap) {
    Long stockLevel = 0L;
    if (MapUtils.isNotEmpty(availabilityMap)) {
      final List<StockLevelModel> stockLevelList =
          Objects.nonNull(availabilityMap.get(skuProduct)) ?
              availabilityMap.get(skuProduct) : Collections.emptyList();
      final Set<String> serialProductCodes = stockLevelList.stream()
          .map(StockLevelModel::getSerialProductCode).collect(Collectors.toSet());

      stockLevel = Long.valueOf(serialProductCodes.size());
    }
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "available quantity {} for the product {} ", stockLevel, skuProduct);
    return stockLevel;
  }

  private Collection<StockLevelModel> getStocks(final Set<String> productCodes,
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

  private ConsignmentModel getConsignment(final WarehouseModel location,
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
	 * It filters the order which can not be fulfilled due to unavailability
	 * @param todayOrdersToBeProcessed the list of orders
	 * @param warehouses the list of warehouse
	 * @param currentDate the current date
	 * @param isPresentDay
   * @return order with associated unallocated products
	 */
  private Map<AbstractOrderModel, Set<String>> filterOrdersForProcessing(
      final List<AbstractOrderModel> todayOrdersToBeProcessed,
      final List<WarehouseModel> warehouses, final Date currentDate, final boolean isPresentDay) {
    //This map will contain order id with unallocated products
    final Map<AbstractOrderModel, Set<String>> ordersWithUnallocatedProducts = new LinkedHashMap<>();
    //This map will contain unallocated products with quantity
    final Map<String, Long> unallocatedProductWithQty = new HashMap<>();
    todayOrdersToBeProcessed.stream().forEach(order -> {
      //List of unallocated products
      final Set<String> unallocatedProducts = new HashSet<>();
      order.getEntries().forEach(entry -> {
        final Long unallocatedQty = entry.getUnAllocatedQuantity();
        if (Objects.nonNull(unallocatedQty) && unallocatedQty > 0) {
          final String productCode = entry.getProduct().getCode();
          unallocatedProductWithQty
              .put(productCode, (Objects.nonNull(unallocatedProductWithQty.get(productCode)) ?
                  unallocatedProductWithQty.get(productCode) : 0) + entry.getUnAllocatedQuantity());
          unallocatedProducts.add(productCode);
        }
      });
      ordersWithUnallocatedProducts.put(order, unallocatedProducts);
    });
    // List of unallocated products code
    final List<String> unallocatedProductList = unallocatedProductWithQty.keySet()
        .stream().collect(Collectors.toList());
    if(CollectionUtils.isEmpty(unallocatedProductList)) {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "unallocated quantity is not set for the order {} ", todayOrdersToBeProcessed);
      return Collections.emptyMap();
    }
    //It finds stock for unallocated products for the day when the products will ship out
    final Map<String, Long> getStockForUnallocatedProducts = getBlCommerceStockService().
        getStockForUnallocatedProduct(unallocatedProductList, warehouses, currentDate, currentDate);
    //The products which are not available to fulfill
    final List<String> unavailableProducts = new ArrayList<>();
    unallocatedProductWithQty.entrySet().forEach(entry -> {
      final Optional<Entry<String, Long>> unmatchedProducts = getStockForUnallocatedProducts
          .entrySet().stream()
          .filter(entryForUnallocatedProducts ->
              (entryForUnallocatedProducts.getKey().equals(entry.getKey()) &&
                  entryForUnallocatedProducts.getValue() < (entry.getValue()))).findFirst();
      if (unmatchedProducts.isPresent()) {
        unavailableProducts.add(unmatchedProducts.get().getKey());
      }
    });
    final Set<String> productsWithStocks = getStockForUnallocatedProducts.entrySet().stream().map(Entry::getKey).collect(Collectors.toSet());
    unallocatedProductList.removeAll(productsWithStocks);
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "List of products with 0 stocks {} ", unallocatedProductList.toString());
    unavailableProducts.addAll(unallocatedProductList);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO,
				"List of products that can not be fulfilled {} ", unavailableProducts.toString());
		if(CollectionUtils.isNotEmpty(unavailableProducts)) {
		  if(isPresentDay) {
        final List<AbstractOrderModel> orderModelList = getOrderDao()
            .getOrdersOfUnavailableSoftAssignedSerials(currentDate, unavailableProducts);
        final Set<String> orderCodes = orderModelList.stream().map(AbstractOrderModel::getCode)
            .collect(Collectors.toSet());
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "List of orders which have unallocated products to be shipped in same day {} ",
            orderCodes);
        if (CollectionUtils.isNotEmpty(orderCodes)) {
          final List<StockLevelModel> stocks = getBlStockLevelDao()
              .getStocksOfSoftAssignedSerialsOfOrders(orderCodes);
          final Set<String> ordersNotToBeFulfilled = stocks.stream().map(StockLevelModel::getOrder)
              .collect(Collectors.toSet());
          ordersNotToBeFulfilled.forEach(orderCode -> {
            final AbstractOrderModel orderModel = getOrderDao().getOrderByCode(orderCode);
            orderModel.setStatus(OrderStatus.RECEIVED_MANUAL_REVIEW);
            orderModel.setManualReviewStatusByReshuffler(true);
          });
          getModelService().saveAll();
          BlLogger.logFormatMessageInfo(LOG, Level.INFO,
              "List of orders that have been set in manual review status {} ",
              ordersNotToBeFulfilled);
        }
      }
      ordersWithUnallocatedProducts.entrySet().removeIf(entry -> !(Collections
          .disjoint(entry.getValue(), unavailableProducts)));
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "List of orders that can be fulfilled {} ", ordersWithUnallocatedProducts);
    }
		//The orders which will not be considered in reshuffler job, will be in manual review only
    return ordersWithUnallocatedProducts;
  }

  public BlOrderDao getOrderDao() {
    return orderDao;
  }

  public void setOrderDao(final BlOrderDao orderDao) {
    this.orderDao = orderDao;
  }

  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
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

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public BlAssignSerialService getBlAssignSerialService() {
    return blAssignSerialService;
  }

  public void setBlAssignSerialService(
      BlAssignSerialService blAssignSerialService) {
    this.blAssignSerialService = blAssignSerialService;
  }

  public BlAllocationService getBlAllocationService() {
    return blAllocationService;
  }

  public void setBlAllocationService(
      BlAllocationService blAllocationService) {
    this.blAllocationService = blAllocationService;
  }

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
  }

  public BlConsignmentEntryService getBlConsignmentEntryService() {
    return blConsignmentEntryService;
  }

  public void setBlConsignmentEntryService(
      BlConsignmentEntryService blConsignmentEntryService) {
    this.blConsignmentEntryService = blConsignmentEntryService;
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

}
