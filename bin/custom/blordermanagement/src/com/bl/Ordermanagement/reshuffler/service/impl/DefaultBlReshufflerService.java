package com.bl.Ordermanagement.reshuffler.service.impl;

import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.reshuffler.service.BlOptimizeShippingFromWHService;
import com.bl.Ordermanagement.reshuffler.service.BlReshufflerService;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlPickUpZoneDeliveryModeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.tx.Transaction;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
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
  private BlStockLevelDao blStockLevelDao;
  private BlProductService blProductService;
  private BlOptimizeShippingFromWHService blOptimizeShippingFromWHService;

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
      if(startDate.equals(currentDate)) {
        getBlOptimizeShippingFromWHService()
            .optimizeShipFormWHForOrders(startDate);
      }
    }
  }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processSerialsInLateOrders() {
        final Date currentDate = Date
                .from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());
        final Date endDate = DateUtils.addDays(currentDate, 2);
        for (Date startDate = currentDate; startDate.before(endDate); startDate = DateUtils.addDays(startDate, 1)) {
            processOrdersSoonToBeTransitByDay(startDate, startDate.equals(currentDate));
            if(startDate.equals(currentDate)) {
                getBlOptimizeShippingFromWHService()
                        .optimizeShipFormWHForOrders(startDate);
            }
        }
    }

  /**
   * It processed the orders day wise
   * @param currentDate the date
   * @param isPresentDay is present day
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


    public void processOrdersSoonToBeTransitByDay(final Date currentDate, final boolean isPresentDay) {
        final List<AbstractOrderModel> ordersToBeProcessed = getOrderDao()
                .getOrdersToBeShippedSoon(currentDate);
        if(CollectionUtils.isNotEmpty(ordersToBeProcessed)) {
            //Sorted by delivery mode
            final Set<AbstractOrderModel> removeDuplicateOrders = new HashSet<>(ordersToBeProcessed);
            final List<AbstractOrderModel> remainingOrders = new ArrayList<>(removeDuplicateOrders);
            //Sort the orders by order total price
            final List<AbstractOrderModel> ordersSortedByTotalPrice = remainingOrders.stream()
                    .sorted(Comparator.comparing(AbstractOrderModel::getTotalPrice).reversed())
                    .collect(Collectors.toList());
          final List<AbstractOrderModel> finalSortedOrders = new ArrayList<>(ordersSortedByTotalPrice);
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                    "List of orders to fulfill {} for the day {} ", finalSortedOrders.toString(), currentDate);
            final BaseStoreModel baseStoreModel = getBaseStoreService()
                    .getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
            //Get all warehouses
            final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();
            //It filters the orders which needs to be processed and ignore the orders which contains the SKU (when total number
            // of sku needed for orders, will ship on same day, is not sufficient to fulfill from main and buffer inventory
            final Map<AbstractOrderModel, Set<String>> filteredOrders = filterOrdersForProcessingLateSerials(
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
          BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while fulfilling the order", ex);
        } finally {
          if(fulfillmentCompleted){
            tx.commit();
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                "Order fulfillment is successful for the order {} ", entry.getKey().getCode());
          }else{
            tx.rollback();
            BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
                "Order fulfillment is not successful for the order {} ", entry.getKey().getCode());
          }
        }
      });
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
    final WarehouseModel anotherWH = getBlOptimizeShippingFromWHService().getAnotherWarehouse(warehouses, location);
    final boolean noSplitting = checkFulfillmentFromSingleWH(order, anotherWH, location, productCodes);
    if(!noSplitting) {
      if(!checkFulfillmentFromSingleWH(order, location, anotherWH, productCodes)) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "all the products can not be fulfilled from single warehouse for the order {}",
            order.getCode());
        return fulfillFromMultipleWarehouses(entry, location, anotherWH, productCodes);
      } else {
        setOrderStatus(order);
        return true;
      }
    } else {
      setOrderStatus(order);
      return true;
    }
  }

  /**
   * It checks whether all the products of the order can be fulfilled from a single warehouse
   * @param order the order
   * @param warehouse the warehouse
   * @param preferredWH the preferred warehouse
   * @return boolean
   */
  private boolean checkFulfillmentFromSingleWH(final AbstractOrderModel order, final WarehouseModel warehouse,
      final WarehouseModel preferredWH, final Set<String> productCodes) {
    final Set<String> modifiedProductCodes = getBlOptimizeShippingFromWHService().modifyProductCodes(order, warehouse);
    modifiedProductCodes.addAll(productCodes);
    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "list of products {} to fulfill from preferred warehouse {} for the order {}",
        modifiedProductCodes.toString(), preferredWH.getCode(), order.getCode());
    if (CollectionUtils.isNotEmpty(modifiedProductCodes)) {
      final Collection<StockLevelModel> stockLevels = getBlOptimizeShippingFromWHService()
          .getStocks(modifiedProductCodes, preferredWH,
              order);
      final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
          .groupBySkuProductWithAvailability(stockLevels);
      if (MapUtils.isNotEmpty(availabilityMap) && isSourcingNoSplittingPossible(
          modifiedProductCodes,
          availabilityMap, order, warehouse)) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "all the products can be fulfilled from this warehouse {} for the order {}",
            preferredWH.getCode(), order.getCode());
        final Collection<AbstractOrderEntryModel> orderEntries = getBlOptimizeShippingFromWHService()
            .getOrderEntries(order,
                modifiedProductCodes);
        final SourcingContext context = getBlOptimizeShippingFromWHService()
            .createSourcingContext(orderEntries);
        getBlOptimizeShippingFromWHService().createSourcingLocation(availabilityMap, preferredWH,
            context);
        assignSerialFromLocation(context, true, warehouse);
        getBlOptimizeShippingFromWHService().deleteOtherConsignmentIfAny(order, warehouse);
        getBlOptimizeShippingFromWHService().createConsignment(order, context, preferredWH);
        return true;
      }
    }
    return false;
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
      order.setStatus(OrderStatus.PENDING);
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
	 * It fulfills the order from multiple warehouses
	 * @param entry map with order entry and associated unallocated products
	 * @param location the preferred warehouse
	 * @param anotherWH the other warehouse
	 * @param productCodes list of product code
	 */
  private boolean fulfillFromMultipleWarehouses(final Entry<AbstractOrderModel,
      Set<String>> entry, final WarehouseModel location, final WarehouseModel anotherWH,
      final Set<String> productCodes) {
    final Map<WarehouseModel, List<String>> warehouseWithProducts = new HashMap<>();
    final AbstractOrderModel order = entry.getKey();
    final Collection<StockLevelModel> stockLevels = getBlOptimizeShippingFromWHService().getStocks(productCodes, location, order);
    final Collection<StockLevelModel> stockLevelsFromOtherWH = getBlOptimizeShippingFromWHService().getStocks(productCodes, anotherWH,
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
          final Collection<AbstractOrderEntryModel> entries = getBlOptimizeShippingFromWHService().getOrderEntries(order, products);
          final SourcingContext context = getBlOptimizeShippingFromWHService().createSourcingContext(entries);
          final SourcingLocation sourcingLocation = getBlOptimizeShippingFromWHService().createSourcingLocation(availabilityMap,
              location, context);
          assignSerialFromLocation(context, false, anotherWH);
          getBlOptimizeShippingFromWHService().createConsignment(order, context, location);
          setOrderStatus(order);
        } else {
          if (entryPerWH.getKey().equals(anotherWH)) {
            final Collection<AbstractOrderEntryModel> orderEntries = getBlOptimizeShippingFromWHService().getOrderEntries(order,
                products);
            final SourcingContext context = getBlOptimizeShippingFromWHService().createSourcingContext(orderEntries);
            final SourcingLocation sourcingLocation = getBlOptimizeShippingFromWHService().createSourcingLocation(
                availabilityMapForOtherWH, anotherWH, context);
            assignSerialFromLocation(context, false, location);
            //create consignment
            getBlOptimizeShippingFromWHService().createConsignment(order, context, anotherWH);
            setOrderStatus(order);
          }
        }
      });
    }
    return allUnallocatedProductsFulfilled(order);
  }

  /**
   * It checks whether all the unallocated products fulfilled or not
   * @param order
   * @return
   */
  private boolean allUnallocatedProductsFulfilled(final AbstractOrderModel order ) {
     return order.getEntries().stream().allMatch(entry ->
         entry.getQuantity() == entry.getSerialProducts().size());
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
    final Long availableQty = getBlOptimizeShippingFromWHService().getAvailabilityForProduct(skuProduct, availabilityMap);
    final Optional<AbstractOrderEntryModel> orderEntryModel = order.getEntries().stream()
        .filter(orderEntry ->
            orderEntry.getProduct().getCode()
                .equals(skuProduct)).findFirst();
    final Long availableQtyFromOtherWH = getBlOptimizeShippingFromWHService().getAvailabilityForProduct(skuProduct,
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
    final Long availableQty = getBlOptimizeShippingFromWHService().getAvailabilityForProduct(skuProduct, availabilityMap);
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
          .getQuantity() : getBlOptimizeShippingFromWHService().getAvailabilityForProduct(skuProduct, availabilityMap);
      final List<BlProductModel> entries = orderEntryModel.getSerialProducts().stream()
          .filter(serialProduct ->
              serialProduct instanceof BlSerialProductModel
                  && ((BlSerialProductModel) serialProduct).getWarehouseLocation()
                  .getCode().equals(warehouseModel.getCode())).collect(Collectors.toList());
      return orderEntryModel.getUnAllocatedQuantity() + entries.size() <= availableQty;
    });
  }

	/**
	 * It filters the order which can not be fulfilled due to unavailability
	 * @param todayOrdersToBeProcessed the list of orders
	 * @param warehouses the list of warehouse
	 * @param currentDate the current date
	 * @param isPresentDay
   * @return order with associated unallocated products
	 */
  private Map<AbstractOrderModel, Set<String>> filterOrdersForProcessing(final List<AbstractOrderModel> todayOrdersToBeProcessed,
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


    private Map<AbstractOrderModel, Set<String>> filterOrdersForProcessingLateSerials(final List<AbstractOrderModel> todayOrdersToBeProcessed,
                                                                           final List<WarehouseModel> warehouses, final Date currentDate, final boolean isPresentDay) {

    Map<AbstractOrderModel, Set<String>> mapOfLateOrders = new HashMap<>();
    Set<String> productSet = new HashSet<>();
    for (AbstractOrderModel order: todayOrdersToBeProcessed) {
            for(AbstractOrderEntryModel entryModel: order.getEntries())
            {
                if(CollectionUtils.isNotEmpty(entryModel.getSerialProducts())) {
                  for (BlProductModel serialProductModel : entryModel.getSerialProducts()) {
                    if (serialProductModel instanceof BlSerialProductModel) {
                      OrderModel orderModel = ((BlSerialProductModel)serialProductModel).getAssociatedOrder();
                      if (Objects.nonNull(orderModel) && OrderStatus.LATE.equals(orderModel.getStatus())) {
                        entryModel.setSerialProducts(Collections.emptyList());
                        modelService.save(entryModel);
                        mapOfLateOrders.put(order, productSet);
                      }
                    }
                  }
                }
            }
        }
    return mapOfLateOrders;
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

  public BlStockLevelDao getBlStockLevelDao() {
    return blStockLevelDao;
  }

  public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
    this.blStockLevelDao = blStockLevelDao;
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

  public BlOptimizeShippingFromWHService getBlOptimizeShippingFromWHService() {
    return blOptimizeShippingFromWHService;
  }

  public void setBlOptimizeShippingFromWHService(
      BlOptimizeShippingFromWHService blOptimizeShippingFromWHService) {
    this.blOptimizeShippingFromWHService = blOptimizeShippingFromWHService;
  }

}
