package com.bl.Ordermanagement.actions.order.allocation;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.filters.BlDeliveryStateSourcingLocationFilter;
import com.bl.Ordermanagement.reshuffler.service.BlOptimizeShippingFromWHService;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.Ordermanagement.services.BlAssignSerialService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.services.consignment.entry.BlConsignmentEntryService;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import de.hybris.platform.task.RetryLaterException;
import de.hybris.platform.warehousing.data.sourcing.SourcingContext;
import de.hybris.platform.warehousing.data.sourcing.SourcingLocation;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.util.*;

public class BlModifiedOrderAction extends AbstractProceduralAction<OrderProcessModel> {

    private static final Logger LOG = Logger.getLogger(BlModifiedOrderAction.class);

    private BaseStoreService baseStoreService;
    private BlStockLevelDao blStockLevelDao;
    private BlDeliveryStateSourcingLocationFilter blDeliveryStateSourcingLocationFilter;
    private BlOptimizeShippingFromWHService blOptimizeShippingFromWHService;
    private BlCommerceStockService blCommerceStockService;
    private BlProductService blProductService;
    private BlAssignSerialService blAssignSerialService;
    private BlAllocationService blAllocationService;
    private BlConsignmentEntryService blConsignmentEntryService;
    @Resource(name = "defaultBlUserService")
    private DefaultBlUserService defaultBlUserService;

    @Override
    public void executeAction(OrderProcessModel process)
            throws RetryLaterException, Exception {
        final OrderModel order = process.getOrder();
        try {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Started action for modify rental date with Process: {} in step {} for the order {}",
                    process.getCode(), getClass().getSimpleName(), order.getCode());
            final Set<String> productSet = new HashSet<>();
            order.getConsignments().forEach(consignmentModel -> {
                Set<String> olderProductCode = new HashSet<>();
                consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
                    consignmentEntryModel.getSerialProducts().forEach(blProductModel -> {
                        if (blProductModel instanceof BlSerialProductModel) {
                            olderProductCode.add(blProductModel.getCode());
                        }
                    });
                    consignmentEntryModel.setConsignmentEntryStatus(new HashMap<>());
                    consignmentEntryModel.setItems(new HashMap<>());
                    consignmentEntryModel.setSerialProducts(Collections.emptyList());
                });
                releaseStockForGivenSerial(olderProductCode, consignmentModel.getOptimizedShippingStartDate(), consignmentModel.getOptimizedShippingEndDate());
                consignmentModel.setOptimizedShippingStartDate(order.getActualRentalStartDate());
                consignmentModel.setOptimizedShippingEndDate(order.getActualRentalEndDate());
            });

            order.getEntries().forEach(entryModel -> {
                entryModel.setSerialProducts(Collections.emptyList());
                entryModel.setUnAllocatedQuantity(entryModel.getQuantity());
                productSet.add(entryModel.getProduct().getCode());
                modelService.save(entryModel);
            });
            processOrder(order, productSet);
        } catch (Exception ex) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Some error occur while processing order for rental date modification for the order {}", order.getCode());
            BlLogger.logMessage(LOG, Level.ERROR, "Some error occur while processing modify rental date order", ex);
        }
    }

    public void processOrder(OrderModel order, Set<String> productSet) {
        //Get all warehouses from base store
        final BaseStoreModel baseStoreModel = getBaseStoreService()
                .getBaseStoreForUid(BlCoreConstants.BASE_STORE_ID);
        final List<WarehouseModel> warehouses = baseStoreModel.getWarehouses();

        boolean fulfillmentCompleted = false;
        final WarehouseModel location = getBlDeliveryStateSourcingLocationFilter().applyFilter(order);
        fulfillmentCompleted = fulfillFromWH(location, order, productSet, warehouses);
        setOrderStatus(order);
        if (fulfillmentCompleted) {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Order fulfilment successful for modify rental date {} and {} for the order {}", order.getRentalStartDate(), order.getRentalEndDate(), order.getCode());
        } else {
            BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Order fulfilment not successful for modify rental date {} and {} for the order {}", order.getRentalStartDate(), order.getRentalEndDate(), order.getCode());
        }
    }

    private boolean fulfillFromWH(final WarehouseModel location,final AbstractOrderModel order,
                                   final Set<String> productCodes, final List<WarehouseModel> warehouses) {
        final WarehouseModel anotherWH = getBlOptimizeShippingFromWHService().getAnotherWarehouse(warehouses, location);
        final boolean noSplitting = checkFulfillmentFromSingleWH(order, anotherWH, location, productCodes);
        if (!noSplitting) {
            if (!checkFulfillmentFromSingleWH(order, location, anotherWH, productCodes)) {
                BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                        "For the modified date all the products can not be fulfilled from single warehouse for the order {}",
                        order.getCode());
                return fulfillFromMultipleWarehouses(order, location, anotherWH, productCodes);
            } else {
                return true;
            }
        } else {
            return true;
        }
    }

    private boolean checkFulfillmentFromSingleWH(final AbstractOrderModel order, final WarehouseModel warehouse,
                                                 final WarehouseModel preferredWH, final Set<String> productCodes) {
        BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                "list of products {} to fulfill from preferred warehouse {} for the order {}",
                productCodes.toString(), preferredWH.getCode(), order.getCode());

        if (CollectionUtils.isNotEmpty(productCodes)) {
            final Collection<StockLevelModel> stockLevels = getBlOptimizeShippingFromWHService()
                    .getStocks(productCodes, preferredWH,order);

            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                    "Stock size {} for the product {} from given warehouse {} when rental date modified for the order {}",
                    stockLevels.size(), productCodes, preferredWH.getCode(), order.getCode());

            final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
                    .groupBySkuProductWithAvailability(stockLevels);
            if (MapUtils.isNotEmpty(availabilityMap) && isSourcingNoSplittingPossible(
                    productCodes,availabilityMap, order, warehouse)) {

                BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                        "all the products can be fulfilled from this warehouse {} for the order {}",
                        preferredWH.getCode(), order.getCode());

                final SourcingContext context = getBlOptimizeShippingFromWHService()
                        .createSourcingContext(order.getEntries());
                createSourcingLocation(context, preferredWH, availabilityMap);
                getBlAssignSerialService().assignSerialsFromLocation(context, context.getSourcingLocations().iterator().next());
                getBlOptimizeShippingFromWHService().deleteOtherConsignmentIfAny(order, warehouse);

                final ConsignmentModel consignment = getBlOptimizeShippingFromWHService().getConsignment(preferredWH, order);
                if (null != consignment) {
                    final Set<SourcingResult> sourcingResults = context.getResult().getResults();
                    final SourcingResult result =
                            CollectionUtils.isNotEmpty(sourcingResults) ? sourcingResults.iterator().next() :
                                    new SourcingResult();
                    getBlAllocationService().optimizeShippingMethodForConsignment(consignment, result);
                }
                getBlOptimizeShippingFromWHService().createConsignment(order, context, preferredWH);
                return true;
            }
        }
        return false;
    }

    private boolean fulfillFromMultipleWarehouses(final AbstractOrderModel order, final WarehouseModel location, final WarehouseModel anotherWH,
                                                  final Set<String> productCodes) {
        final Map<WarehouseModel, List<String>> warehouseWithProducts = new HashMap<>();
        final Collection<StockLevelModel> stockLevels = getBlOptimizeShippingFromWHService().getStocks(productCodes, location, order);
        final Collection<StockLevelModel> stockLevelsFromOtherWH = getBlOptimizeShippingFromWHService().getStocks(productCodes, anotherWH,
                order);
        final Map<String, List<StockLevelModel>> availabilityMap = getBlCommerceStockService()
                .groupBySkuProductWithAvailability(stockLevels);
        final Map<String, List<StockLevelModel>> availabilityMapForOtherWH = getBlCommerceStockService()
                .groupBySkuProductWithAvailability(stockLevelsFromOtherWH);

        final SourcingContext context = getBlOptimizeShippingFromWHService()
                .createSourcingContext(order.getEntries());
// creating sourcinglocation
        createSourcingLocation(context, location, availabilityMap);
        createSourcingLocation(context, anotherWH, availabilityMapForOtherWH);

        boolean sourcingComplete = false;
        for (SourcingLocation sourcingLocation : context.getSourcingLocations()) {
            if (null != sourcingLocation.getAvailabilityMap()) {
                sourcingComplete = getBlAssignSerialService()
                        .assignSerialsFromLocation(context, sourcingLocation);
            }
            if (sourcingComplete) {
                break;
            }
        }

        final Set<SourcingResult> sourcingResults = context.getResult().getResults();
        sourcingResults.forEach(sourcingResult -> {
            ConsignmentModel consignment = getBlOptimizeShippingFromWHService().getConsignment(sourcingResult.getWarehouse(), order);
            if (null != consignment) {
                getBlAllocationService().optimizeShippingMethodForConsignment(consignment, sourcingResult);
            }
            createConsignment(order, context, sourcingResult.getWarehouse(), sourcingResult, consignment);
        });
        return allUnallocatedProductsFulfilled(order);
    }

    public void createConsignment(final AbstractOrderModel order, final SourcingContext context,
                                  final WarehouseModel warehouseModel, final SourcingResult result, final ConsignmentModel consignment) {

        if (Objects.isNull(consignment)) {
            final ConsignmentModel newConsignment = getBlAllocationService().createConsignment(order,
                    BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + order.getCode()
                            + BlOrdermanagementConstants.UNDER_SCORE
                            + order.getConsignments().size(), result);
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                    "A new consignment has been created {} ", newConsignment.getCode());
        } else {

            final List<AbstractOrderEntryModel> orderEntries = new ArrayList<>();
            Set<ConsignmentEntryModel> createdConsignmentEntries = new HashSet<>();

            context.getOrderEntries().forEach(orderEntry -> {

                Optional<ConsignmentEntryModel> consEntry = consignment.getConsignmentEntries().stream().filter(consignmentEntryModel -> consignmentEntryModel.getOrderEntry().equals(orderEntry)).findFirst();
                if (consEntry.isPresent()) {
                    updateConsignmentEntry(consEntry.get(), result, orderEntry);
                } else {
                    Set<BlSerialProductModel> blSerialProductModels = result.getSerialProductMap()
                            .get(orderEntry.getEntryNumber());
                   final ConsignmentEntryModel consignmentEntry = getBlAllocationService()
                            .createConsignmentEntry(orderEntry, Long.valueOf(blSerialProductModels.size()), consignment, result);
                    orderEntry.setUnAllocatedQuantity(orderEntry.getUnAllocatedQuantity() - Long.valueOf(orderEntry.getSerialProducts().size()));
                    createdConsignmentEntries.add(consignmentEntry);
                    getBlOptimizeShippingFromWHService().reserveStocksForSerialProductsThroughReshuffler(blSerialProductModels, consignmentEntry);

                    Set<ConsignmentEntryModel> consignmentEntries = new HashSet<>();
                    if (CollectionUtils.isNotEmpty(orderEntry.getConsignmentEntries())) {
                        consignmentEntries.addAll(orderEntry.getConsignmentEntries());
                        if (!orderEntry.getConsignmentEntries().contains(consignmentEntry)) ;
                        {
                            consignmentEntries.add(consignmentEntry);
                            orderEntry.setConsignmentEntries(consignmentEntries);
                        }
                    }
                }
                getModelService().save(orderEntry);
                if (orderEntry.getUnAllocatedQuantity() <= 0) {
                    orderEntries.add(orderEntry);
                }
            });
            final List<AbstractOrderEntryModel> contextOrderEntries = new ArrayList<>(context.getOrderEntries());
            contextOrderEntries.removeAll(orderEntries);
            context.setOrderEntries(contextOrderEntries);
            createdConsignmentEntries.addAll(consignment.getConsignmentEntries());
            consignment.setConsignmentEntries(createdConsignmentEntries);
            consignment.setStatus(ConsignmentStatus.READY);
            getModelService().save(consignment);

        }
    }

    public void updateConsignmentEntry(final ConsignmentEntryModel entry,final SourcingResult result,
                                       final AbstractOrderEntryModel orderEntry) {
        final List<BlProductModel> consignmentEntrySerialProducts =
                CollectionUtils.isEmpty(entry.getSerialProducts()) ? new ArrayList<>() : entry.getSerialProducts();
        final List<BlProductModel> associatedSerialProducts = new ArrayList<>(consignmentEntrySerialProducts);

        final Set<BlSerialProductModel> serialProductModels =
                null == result.getSerialProductMap() ? new HashSet<>() : result.getSerialProductMap()
                        .get(orderEntry.getEntryNumber());

        if (CollectionUtils.isNotEmpty(serialProductModels)) {

            associatedSerialProducts.addAll(serialProductModels);

            final Set<BlSerialProductModel> serialProducts = new HashSet<>();
            associatedSerialProducts.forEach(serial -> {
                if (serial instanceof BlSerialProductModel) {
                    serialProducts.add((BlSerialProductModel) serial);
                }
            });
            orderEntry.setUnAllocatedQuantity(orderEntry.getUnAllocatedQuantity() - Long.valueOf(serialProducts.size()));
            entry.setQuantity(Long.valueOf(serialProducts.size()));
            entry.setSerialProducts(
                    new ArrayList<>(serialProducts));   //setting serial products from result
            getBlConsignmentEntryService().setItemsMap(entry, serialProducts);
            getBlAllocationService().setSerialCodesToBillingCharges(entry, serialProducts);
            getModelService().save(entry);
            getBlOptimizeShippingFromWHService().reserveStocksForSerialProductsThroughReshuffler(serialProductModels, entry);
        }
    }

    private void createSourcingLocation(final SourcingContext context, final WarehouseModel location, final Map<String, List<StockLevelModel>> availabilityMap) {
        final SourcingLocation sourcingLocation = new SourcingLocation();
        sourcingLocation.setWarehouse(location);
        sourcingLocation.setContext(context);
        sourcingLocation.setAvailabilityMap(availabilityMap);
        Collection<SourcingLocation> sourcingLocations = context.getSourcingLocations();
        if (CollectionUtils.isEmpty(sourcingLocations)) {
            sourcingLocations = new HashSet<>();
        }
        sourcingLocations.add(sourcingLocation);
        context.setSourcingLocations(sourcingLocations);
    }

    private void setOrderStatus(final AbstractOrderModel order) {
        final boolean allQuantityFulfilled = order.getEntries().stream().allMatch(entry -> {
            setUnallocatedQtyToZero(entry);
            return entry.getQuantity() == entry.getSerialProducts().size();
        });
        if (allQuantityFulfilled) {
            order.setStatus(OrderStatus.PENDING);
            getModelService().save(order);
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                    "All the unallocated products are fulfilled for the order {}, hence the status is set to {} ",
                    order.getCode(), order.getStatus().getCode());
        } else {
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


    private void releaseStockForGivenSerial(Set<String> productsCode, Date startDate, Date endDate) {
        final Collection<StockLevelModel> serialStock = getBlStockLevelDao()
                .findALLSerialStockLevelsForDateAndCodes(productsCode, startDate,
                        endDate);
        if (CollectionUtils.isNotEmpty(serialStock)) {
            serialStock.forEach(stockLevel -> {
                try {
                    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                            "Release stock for serial product {}, for stock date {} while modify rental date before change Hard Assign {} , reserve status {}, associated order {}"
                                    + ",current date {} current user {}", stockLevel.getSerialProductCode(), stockLevel.getDate(), stockLevel.getHardAssigned(), stockLevel.getReservedStatus(),
                            stockLevel.getOrder(), new Date(), (defaultBlUserService.getCurrentUser() != null ? defaultBlUserService.getCurrentUser().getUid() : "In Automation"));
                } catch (Exception e) {
                    BlLogger.logMessage(LOG, Level.ERROR, "Some error occur while release stock in modify rental date flow", e);
                }
                stockLevel.setReservedStatus(false);
                stockLevel.setOrder(null);
                BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                        "Stock status is changed to {} for the serial product {} ", stockLevel.getReservedStatus(),
                        stockLevel.getSerialProductCode());
            });
            modelService.saveAll(serialStock);
        }
    }

    private boolean allUnallocatedProductsFulfilled(final AbstractOrderModel order) {
        return order.getEntries().stream().allMatch(entry ->
                entry.getQuantity() == entry.getSerialProducts().size());
    }

    private void setUnallocatedQtyToZero(final AbstractOrderEntryModel entry) {
        if (entry.getUnAllocatedQuantity() > 0 && entry.getQuantity() == entry.getSerialProducts().size()) {
            entry.setUnAllocatedQuantity(0L);
            getModelService().save(entry);
            BlLogger.logFormatMessageInfo(LOG, Level.INFO,
                    "All the products are fulfilled of this entry {} for the order {} ",
                    entry, entry.getOrder().getCode());
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

    public BlAllocationService getBlAllocationService() {
        return blAllocationService;
    }

    public void setBlAllocationService(BlAllocationService blAllocationService) {
        this.blAllocationService = blAllocationService;
    }

    public BlConsignmentEntryService getBlConsignmentEntryService() {
        return blConsignmentEntryService;
    }

    public void setBlConsignmentEntryService(BlConsignmentEntryService blConsignmentEntryService) {
        this.blConsignmentEntryService = blConsignmentEntryService;
    }
}
