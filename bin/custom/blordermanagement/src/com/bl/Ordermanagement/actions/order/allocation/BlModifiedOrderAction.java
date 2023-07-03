package com.bl.Ordermanagement.actions.order.allocation;

import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.reshuffler.service.BlOptimizeShippingFromWHService;
import com.bl.Ordermanagement.reshuffler.service.BlReshufflerService;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class BlModifiedOrderAction extends AbstractProceduralAction<OrderProcessModel> {

  private static final Logger LOG = Logger.getLogger(BlModifiedOrderAction.class);

  private BaseStoreService baseStoreService;
  private BlStockLevelDao blStockLevelDao;
  private BlReshufflerService blReshufflerService;
  private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;
  private BlOptimizeShippingFromWHService blOptimizeShippingFromWHService;
  private BlCommerceStockService blCommerceStockService;
  private BlProductService blProductService;
  private BlAssignSerialService blAssignSerialService;

  @Override
  public void executeAction(OrderProcessModel process)
      throws RetryLaterException, Exception {
    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Process: {} in step {}",
        process.getCode(), getClass().getSimpleName());
    final OrderModel order = process.getOrder();

    Set<String> productSet = new HashSet<>();
    order.getConsignments().forEach(consignmentModel -> {
      Set<String> olderProductCode = new HashSet<>();
      consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
        consignmentEntryModel.getSerialProducts().forEach(blProductModel -> {
          if(blProductModel instanceof BlSerialProductModel){
            olderProductCode.add(blProductModel.getCode());
          }
        });
        consignmentEntryModel.setConsignmentEntryStatus(new HashMap<>());
        consignmentEntryModel.setItems(new HashMap<>());
        consignmentEntryModel.setSerialProducts(Collections.emptyList());
      });
      releaseStockForGivenSerial(olderProductCode,consignmentModel.getOptimizedShippingStartDate(),consignmentModel.getOptimizedShippingEndDate());
    });

    order.getEntries().forEach(entryModel -> {
      entryModel.setSerialProducts(Collections.emptyList());
      entryModel.setUnAllocatedQuantity(entryModel.getQuantity());
      productSet.add(entryModel.getProduct().getCode());
      modelService.save(entryModel);
     });

    processOrder(order,productSet);
  }

  public void processOrder(OrderModel order ,Set<String> productSet){
    final BaseStoreModel baseStoreModel = getBaseStoreService()
        .getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
    //Get all warehouses
    final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();

    boolean fulfillmentCompleted = false;

    final WarehouseModel location = getBlDeliveryStateSourcingLocationFilter().applyFilter(order);
    fulfillmentCompleted = fulfillFromWH(location, order,productSet, warehouses);
  }

  private boolean fulfillFromWH(final WarehouseModel location,
      final AbstractOrderModel order,final Set<String> productCodes,
      final List<WarehouseModel> warehouses){

    final WarehouseModel anotherWH = getBlOptimizeShippingFromWHService().getAnotherWarehouse(warehouses, location);
    final boolean noSplitting = checkFulfillmentFromSingleWH(order, anotherWH, location, productCodes);
    if(!noSplitting) {
      if(!checkFulfillmentFromSingleWH(order, location, anotherWH, productCodes)) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "all the products can not be fulfilled from single warehouse for the order {}",
            order.getCode());
        return fulfillFromMultipleWarehouses(order, location, anotherWH, productCodes);
      } else {
        setOrderStatus(order);
        return true;
      }
    } else {
      setOrderStatus(order);
      return true;
    }
  }

  private boolean checkFulfillmentFromSingleWH(final AbstractOrderModel order, final WarehouseModel warehouse,
      final WarehouseModel preferredWH, final Set<String> productCodes) {

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "3. list of products {} to fulfill from preferred warehouse {} for the order {}",
        productCodes.toString(), preferredWH.getCode(), order.getCode());
    if (CollectionUtils.isNotEmpty(productCodes)) {
      final Collection<StockLevelModel> stockLevels = getBlOptimizeShippingFromWHService()
          .getStocks(productCodes, preferredWH,
              order);
      final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
          .groupBySkuProductWithAvailability(stockLevels);
      if (MapUtils.isNotEmpty(availabilityMap) && isSourcingNoSplittingPossible(
          productCodes,
          availabilityMap, order, warehouse)) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
            "all the products can be fulfilled from this warehouse {} for the order {}",
            preferredWH.getCode(), order.getCode());
        /*final Collection<AbstractOrderEntryModel> orderEntries = getBlOptimizeShippingFromWHService()
            .getOrderEntries(order,
                productCodes);*/
        final SourcingContext context = getBlOptimizeShippingFromWHService()
            .createSourcingContext(order.getEntries());
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

  private boolean fulfillFromMultipleWarehouses(final AbstractOrderModel order, final WarehouseModel location, final WarehouseModel anotherWH,
      final Set<String> productCodes) {
    final Map<WarehouseModel, List<String>> warehouseWithProducts = new HashMap<>();
   // final AbstractOrderModel order = entry.getKey();
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
      productCodes.forEach(skuProduct ->
          populateProductFulfillDetailsForBothWH(availabilityMap, skuProduct, order, products,
              availabilityMapForOtherWH, productsFromOtherWH, allEntriesCanBeFulfilled));
      warehouseWithProducts.put(location, products);
      warehouseWithProducts.put(anotherWH, productsFromOtherWH);
    } else if (CollectionUtils.isNotEmpty(stockLevels)) {
      final List<String> products = new ArrayList<>();
      productCodes.forEach(skuProduct ->
          populateProductFulfillDetails(availabilityMap, skuProduct, order, products,
              allEntriesCanBeFulfilled));
      warehouseWithProducts.put(location, products);
    } else if (CollectionUtils.isNotEmpty(stockLevelsFromOtherWH)) {
      final List<String> productsFromOtherWH = new ArrayList<>();
      productCodes.forEach(skuProduct ->
          populateProductFulfillDetails(availabilityMapForOtherWH, skuProduct, order,
              productsFromOtherWH, allEntriesCanBeFulfilled));
      warehouseWithProducts.put(anotherWH, productsFromOtherWH);
    } else {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "This product {} of the order {} does not have enough stock to fulfill ", productCodes, order.getCode());
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
    }else{
      order.setStatus(OrderStatus.RECEIVED_MANUAL_REVIEW);
      getModelService().save(order);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "Some of the product are unallocated for the order {}, hence the status is set to {} ",
          order.getCode(), order.getStatus().getCode());
    }
  }

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
      return orderEntryModel.getUnAllocatedQuantity() <= availableQty;
    });
  }

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


  private void releaseStockForGivenSerial(Set<String> productsCode, Date startDate,Date endDate){
  final Collection<StockLevelModel> serialStock = getBlStockLevelDao()
      .findALLSerialStockLevelsForDateAndCodes(productsCode, startDate,
          endDate);
  if (CollectionUtils.isNotEmpty(serialStock)) {
    serialStock.forEach(stockLevel -> {
      stockLevel.setReservedStatus(false);
      stockLevel.setOrder(null);
    });
    modelService.saveAll(serialStock);
  }
}

  private boolean allUnallocatedProductsFulfilled(final AbstractOrderModel order ) {
    return order.getEntries().stream().allMatch(entry ->
        entry.getQuantity() == entry.getSerialProducts().size());
  }

  private void setUnallocatedQtyToZero(final AbstractOrderEntryModel entry) {
    if(entry.getUnAllocatedQuantity() > 0 && entry.getQuantity() == entry.getSerialProducts().size()) {
      entry.setUnAllocatedQuantity(0L);
      getModelService().save(entry);
      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "All the products are fulfilled of this entry {} for the order {} ",
          entry, entry.getOrder().getCode());
    }
  }
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

    public BaseStoreService getBaseStoreService() {
      return baseStoreService;
    }
    public void setBaseStoreService(BaseStoreService baseStoreService) {
      this.baseStoreService = baseStoreService;
    }
    public BlStockLevelDao getBlStockLevelDao() {
      return blStockLevelDao;
    }
    public void setBlStockLevelDao(BlStockLevelDao blStockLevelDao) {
      this.blStockLevelDao = blStockLevelDao;
    }
  public BlReshufflerService getBlReshufflerService() {
    return blReshufflerService;
  }

  public void setBlReshufflerService(
      BlReshufflerService blReshufflerService) {
    this.blReshufflerService = blReshufflerService;
  }

  public BlDeliveryStateSourcingLocationFilter getBlDeliveryStateSourcingLocationFilter() {
    return blDeliveryStateSourcingLocationFilter;
  }

  public void setBlDeliveryStateSourcingLocationFilter(
      BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter) {
    this.blDeliveryStateSourcingLocationFilter = blDeliveryStateSourcingLocationFilter;
  }
  public BlOptimizeShippingFromWHService getBlOptimizeShippingFromWHService() {
    return blOptimizeShippingFromWHService;
  }

  public void setBlOptimizeShippingFromWHService(
      BlOptimizeShippingFromWHService blOptimizeShippingFromWHService) {
    this.blOptimizeShippingFromWHService = blOptimizeShippingFromWHService;
  }
  public BlCommerceStockService getBlCommerceStockService() {
    return blCommerceStockService;
  }

  public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService) {
    this.blCommerceStockService = blCommerceStockService;
  }
  public BlProductService getBlProductService() {
    return blProductService;
  }

  public void setBlProductService(BlProductService blProductService) {
    this.blProductService = blProductService;
  }

  public BlAssignSerialService getBlAssignSerialService() {
    return blAssignSerialService;
  }

  public void setBlAssignSerialService(
      BlAssignSerialService blAssignSerialService) {
    this.blAssignSerialService = blAssignSerialService;
  }

  }
