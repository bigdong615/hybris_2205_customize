package com.bl.Ordermanagement.services.impl;

import com.bl.Ordermanagement.reshuffler.service.BlOptimizeShippingFromWHService;
import com.bl.Ordermanagement.services.BlCSAgentOrderModificationService;
import com.bl.Ordermanagement.services.BlSourcingService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.customer.impl.DefaultBlUserService;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;
import com.google.common.collect.Maps;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import de.hybris.platform.warehousing.data.sourcing.SourcingResults;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * This class will allow CS agent to add or modify entry on existing order
 */
public class DefaultBlCSAgentOrderModificationService implements BlCSAgentOrderModificationService {

    private static final Logger LOG = Logger.getLogger(DefaultBlCSAgentOrderModificationService.class);
    @Resource(name = "defaultBlAllocationService")
    private DefaultBlAllocationService defaultBlAllocationService;

    @Resource(name = "modelService")
    private ModelService modelService;

    @Resource(name = "blStockLevelDao")
    private BlStockLevelDao blStockLevelDao;

    @Resource(name="blOrderModificationService")
    private DefaultBlOrderModificationService blOrderModificationService;

    @Resource
    private BlOptimizeShippingFromWHService blOptimizeShippingFromWHService;

    @Resource(name="blSourcingService")
    private BlSourcingService blSourcingService;

    @Resource(name="blOrderService")
    private BlOrderService blOrderService;
    @Resource(name = "defaultBlUserService")
    private DefaultBlUserService defaultBlUserService;




    /**
     * Add new entry to existing order by CS Agent in CS Cockpit
     *
     * @param orderEntryModel
     * @param interceptorContext
     * @param originalSerialProducts
     * @param isOrderModified
     */
    @Override
    public void addNewOrderEntry(OrderEntryModel orderEntryModel, InterceptorContext interceptorContext, List originalSerialProducts, boolean isOrderModified) throws InterceptorException {

        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Auto allocation and assignment on the New Order Entry {} ",
                orderEntryModel.getOrder().getCode());
        final AbstractOrderModel orderModel = orderEntryModel.getOrder();
        if(OrderStatus.SHIPPED.equals(orderModel.getStatus())){
            modelService.remove(orderEntryModel);
            updateOrderDetailsForModifiedEntry(orderModel);
            throw new InterceptorException("Item cannot be added to order if Consignment Status = BL_SHIPPED, Order Status = Shipped");
        } else if(allowedOrderStatusforModification(orderModel.getStatus())) {
            originalSerialProducts= (List<BlProductModel>) originalSerialProducts.stream().map(ogSerial -> ((BlProductModel) ogSerial)).collect(Collectors.toList());
            performAllocationAndAssignment(orderEntryModel,originalSerialProducts,orderEntryModel.getItemModelContext().getOriginalValue(OrderEntryModel.QUANTITY), orderEntryModel.getItemModelContext().getOriginalValue(OrderEntryModel.UNALLOCATEDQUANTITY),false);
        }

    }

    /**
     * Modify existing entry on order when quantity is modified
     *
     * @param orderEntryModel
     * @param interceptorContext
     * @param originalSerialProducts
     * @param isOrderModified
     */
    @Override
    public void modifyExistingEntryForQuantity(OrderEntryModel orderEntryModel, InterceptorContext interceptorContext, List originalSerialProducts, boolean isOrderModified) throws InterceptorException {
        originalSerialProducts= (List<BlProductModel>) originalSerialProducts.stream().map(ogSerial -> ((BlProductModel) ogSerial)).collect(Collectors.toList());
        performAllocationAndAssignment(orderEntryModel,originalSerialProducts,orderEntryModel.getItemModelContext().getOriginalValue(OrderEntryModel.QUANTITY), orderEntryModel.getItemModelContext().getOriginalValue(OrderEntryModel.UNALLOCATEDQUANTITY),true);

    }

    /**
     * Perform Allocation and assignment when new entry is added or existing entry is modified with higher quantity
     * @param orderEntryModel
     * @param originalQuantity
     * @param originalUnallocatedQuantity
     * @throws InterceptorException
     */
    private void performAllocationAndAssignment(final OrderEntryModel orderEntryModel, final List<BlProductModel> originalSerials, final Long originalQuantity, final Long originalUnallocatedQuantity, final boolean isQtyModified) throws InterceptorException {
        SourcingResults sourcingResults;
        final OrderModel order = orderEntryModel.getOrder();
        sourcingResults = blSourcingService.sourceOrder(order, orderEntryModel);
        final int allocatedQty = getAllocatedQuantityValue(sourcingResults);
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Allocated Quantity to the modified or new entry {} ", allocatedQty);
        boolean isSourcingComplete = allocatedQty == orderEntryModel.getQuantity() && !isQtyModified  ? true: isQtyModified && allocatedQty == (orderEntryModel.getQuantity().intValue() - originalQuantity);
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "is Sourcing Complete {}  is it for modified order {}  ", isSourcingComplete , isQtyModified);

        if (CollectionUtils.isNotEmpty(sourcingResults.getResults())  && BooleanUtils.isTrue(isSourcingComplete)) {
            updateNewOrModifiedOrderEntry(orderEntryModel, sourcingResults, order, originalSerials,originalQuantity,originalUnallocatedQuantity );

        }
        else{
            removeOrModifyOrderEntry(orderEntryModel,originalSerials,originalQuantity,originalUnallocatedQuantity);
            throw new InterceptorException("Serial not allocated to the added/modified Product (SKU)");
        }

    }

    /**
     * Update entry in existing or new consignment or related consignment entry
     *
     * @param orderEntryModel
     * @param sourcingResults
     * @param order
     * @param originalSerials
     * @param originalQuantity
     * @param originalUnallocatedQuantity
     */
    private void updateNewOrModifiedOrderEntry(final OrderEntryModel orderEntryModel,final SourcingResults sourcingResults, final OrderModel order,final List<BlProductModel> originalSerials,final Long originalQuantity,final Long originalUnallocatedQuantity) throws InterceptorException {
        Set<SourcingResult> sourcingResultSet = sourcingResults.getResults();

        for (SourcingResult sourcingResult : sourcingResultSet) {
            final Optional<ConsignmentModel> consignmentModel = order.getConsignments().stream()
                    .filter(consignment -> consignment.getWarehouse().getCode().equals(sourcingResult.getWarehouse().getCode())).findFirst();
            if (sourcingResult.getAllocation().containsKey(orderEntryModel)) {
                if (consignmentModel.isPresent()) {
                    createNewConsignmentEntry(orderEntryModel, sourcingResult, consignmentModel.get());
                } else {
                    final SourcingResults sourcingResults1 = new SourcingResults();
                    final Set<SourcingResult> results = new HashSet<>();
                    results.add(sourcingResult);
                    sourcingResults1.setResults(results);
                    getBlOrderModificationService().createConsignmentForModifiedOrder(orderEntryModel,sourcingResults);
                }
            }
        }
            orderEntryModel.setUpdatedTime(new Date());
            updateOrderDetailsForModifiedEntry(order);

    }

    /**
     * Calculates the allocated value as per required
     * @param sourcingResults
     * @return
     */
    private int getAllocatedQuantityValue(SourcingResults sourcingResults) {
         AtomicInteger allocatedQty= new AtomicInteger();
        sourcingResults.getResults().forEach(sourcingResult ->
            allocatedQty.addAndGet(sourcingResult.getAllocation().values().stream().reduce(0L, Long::sum).intValue())
        );
       return allocatedQty.get();
    }

    /**
     * method will be called when any order entry is created for order having the same warehouse whose consignment is already present.
     * @param orderEntryModel
     * @param sourceResult
     * @param consignment
     */
    private void createNewConsignmentEntry(final OrderEntryModel orderEntryModel, final SourcingResult sourceResult,
                                           final ConsignmentModel consignment)
    {
        final Set<ConsignmentEntryModel> consignmentEntries = consignment.getConsignmentEntries();
        final Set<String> serialCodes = new HashSet<>();
        final Optional<ConsignmentEntryModel> consignmentEnt = orderEntryModel.getConsignmentEntries().stream().
                filter(consignmentEntryModel -> consignmentEntryModel.getConsignment().equals(consignment)).findFirst();
        if(consignmentEnt.isPresent()) {
            blOptimizeShippingFromWHService.updateConsignmentEntry(consignmentEnt.get(), sourceResult, orderEntryModel);
            final List<BlProductModel> associatedSerialProducts = new ArrayList<>();
            orderEntryModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
                consignmentEntryModel.getSerialProducts().forEach(serial-> {
                    if(serial instanceof BlSerialProductModel) {
                        associatedSerialProducts.add(serial);
                    }
                });
            });
            orderEntryModel.setSerialProducts(associatedSerialProducts);
        } else {
            final Long entryQty = BooleanUtils.isTrue(orderEntryModel.getAqautechProduct())? orderEntryModel.getQuantity() : Long.valueOf(sourceResult.getSerialProductMap().get(orderEntryModel.getEntryNumber()).size());
            final ConsignmentEntryModel createConsignmentEntry = defaultBlAllocationService
                    .createConsignmentEntry(orderEntryModel,entryQty , consignment,
                            sourceResult);
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment entry created for order {}",
                    consignment.getOrder().getCode());

            final Set<ConsignmentEntryModel> entries = new HashSet<>(consignmentEntries);
            entries.add(createConsignmentEntry);

            consignment.setConsignmentEntries(entries);
            final List<BlProductModel> assignedSerialProducts = new ArrayList<>(
                    orderEntryModel.getSerialProducts());
            assignedSerialProducts.addAll(orderEntryModel.getModifiedSerialProductList());
            orderEntryModel.setSerialProducts(assignedSerialProducts);
        }

        if (consignment.getOrder().getIsRentalOrder()  &&  !blOrderService.isAquatechProductOrder(orderEntryModel.getOrder())) {

            if(CollectionUtils.isEmpty(serialCodes)){
                final List<BlSerialProductModel> serialProducts = sourceResult.getSerialProductMap().values().stream()
                        .flatMap(Collection::stream)
                        .collect(Collectors.toList());
                List<String> serialProductCodes = serialProducts.stream().map(ProductModel::getCode).collect(Collectors.toList());
                serialCodes.addAll(serialProductCodes);
            }

            Collection<StockLevelModel> serialStocks = blStockLevelDao
                    .findSerialStockLevelsForDateAndCodes(serialCodes, consignment.getOptimizedShippingStartDate(),
                            consignment.getOptimizedShippingEndDate(), Boolean.FALSE);

            if(CollectionUtils.isNotEmpty(serialStocks))
            {
                serialStocks.forEach(stock -> {
                    try{
                        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                                "Reserve stock for serial product {}, for stock date {} while creating new order entry before change Hard Assign {}, reserve status {}, associated order {}"
                                        + ",current date {} current user {}",stock.getSerialProductCode(), stock.getDate(), stock.getHardAssigned(), stock.getReservedStatus(),
                                stock.getOrder(), new Date(), (defaultBlUserService.getCurrentUser()!=null? defaultBlUserService.getCurrentUser().getUid():"In Automation"));
                    }catch (Exception e){
                        BlLogger.logMessage(LOG,Level.ERROR,"Some error occur while reserve stock in creating new order entry flow",e);
                    }
                    if(stock.getDate().equals(consignment.getOptimizedShippingStartDate()) && StringUtils.isNotBlank(stock.getOrder())){
                        stock.setReservedStatus(true);
                        stock.setOrder(stock.getOrder()+ "," + orderEntryModel.getOrder().getCode());
                    }
                    else if(stock.getDate().equals(consignment.getOptimizedShippingEndDate()) && StringUtils.isNotBlank(stock.getOrder())){
                        stock.setReservedStatus(true);
                        stock.setOrder(stock.getOrder()+ "," + orderEntryModel.getOrder().getCode());
                    }
                    else{
                        stock.setReservedStatus(true);
                        stock.setOrder(orderEntryModel.getOrder().getCode());
                    }
                });
                modelService.saveAll(serialStocks);
            }
        }
        modelService.save(consignment);
        modelService.refresh(consignment);
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Consignment Entry created for consignment {} and order {}.", consignment.getCode(),consignment.getOrder().getCode());
        getBlOrderModificationService().recalculateOrder(orderEntryModel.getOrder());
    }



    /**
     * Update Order When entry is added/modified
     * @param abstractOrderModel
     */
    private void updateOrderDetailsForModifiedEntry(final AbstractOrderModel abstractOrderModel) {
        abstractOrderModel.setOrderModifiedDate(new Date());
        abstractOrderModel.setUpdatedTime(new Date());
        modelService.save(abstractOrderModel);
        modelService.refresh(abstractOrderModel);
        BlLogger.logFormattedMessage(LOG , Level.DEBUG , "order{} is modified and updated with OrderModifiedDate {} and UpdatedTime {} " ,
                abstractOrderModel.getCode() , abstractOrderModel.getOrderModifiedDate() , abstractOrderModel.getUpdatedTime());
    }

    /**
     * Update removed entry on Order when not fulfilling stock or allocation
     *
     * @param orderEntryModel
     * @param originalSerialProducts
     * @param originalQuantity
     * @param originalUnallocatedQuantity
     */
    private void removeOrModifyOrderEntry(final OrderEntryModel orderEntryModel, final  List<BlProductModel> originalSerialProducts, final Long originalQuantity, final Long originalUnallocatedQuantity) {

        final AbstractOrderModel abstractOrderModel = orderEntryModel.getOrder();
        final List<BlProductModel> serialProducts = orderEntryModel.getSerialProducts();
        final Long modifiedQuantity = orderEntryModel.getQuantity();
        if(originalQuantity.intValue() - modifiedQuantity.intValue() == 0) {
            modelService.remove(orderEntryModel);
        }
        if(!modelService.isRemoved(orderEntryModel)) {
            orderEntryModel.setQuantity(originalQuantity);
            orderEntryModel.setUnAllocatedQuantity(originalUnallocatedQuantity);
            orderEntryModel.setUpdatedTime(new Date());
            if (CollectionUtils.isNotEmpty(originalSerialProducts) && CollectionUtils.isNotEmpty(serialProducts)) {
                orderEntryModel.setSerialProducts(originalSerialProducts);
                updateStockForSerials(originalSerialProducts, serialProducts, orderEntryModel);
            }
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "The modified entry is updated ");
            modelService.save(orderEntryModel);
        }
        updateOrderDetailsForModifiedEntry(abstractOrderModel);
    }

    /**
     * Remove serials when entry removed
     *
     * @param originalSerialProducts
     * @param serialProducts
     * @param orderEntryModel
     */
    private void updateStockForSerials(final List<BlProductModel> originalSerialProducts,final List<BlProductModel> serialProducts,final OrderEntryModel orderEntryModel) {
        List<String> newSerialCodes = serialProducts.stream().map(BlProductModel::getCode).collect(Collectors.toList());
        List<String> oldSerialCodes = originalSerialProducts.stream().map(BlProductModel::getCode).collect(Collectors.toList());
        Map<String,BlProductModel> codeSerialMap = Maps.newHashMap();
        serialProducts.forEach(ser -> codeSerialMap.put(ser.getCode(),ser));
        newSerialCodes.removeIf(oldSerialCodes::contains);
    }

    /**
     * Allowed statuses for modification
     * @param orderStatus
     * @return
     */
    public boolean allowedOrderStatusforModification(final OrderStatus orderStatus) {

        switch (orderStatus.getCode()) {
            case BlCoreConstants.PENDING_STATUS:
            case BlCoreConstants.RECEIVED_MANUAL_REVIEW:
            case BlCoreConstants.FRAUD_CHECKED:
            case BlCoreConstants.SUSPENDED:
            case BlCoreConstants.CHECKED_VALID:
            case BlCoreConstants.CHECKED_INVALID:
                return Boolean.TRUE;
            default :
        }
        return Boolean.FALSE;
    }

    /**
     * Gets bl order modification service.
     *
     * @return the bl order modification service
     */
    public DefaultBlOrderModificationService getBlOrderModificationService() {
        return blOrderModificationService;
    }

    /**
     * Sets bl order modification service.
     *
     * @param blOrderModificationService the bl order modification service
     */
    public void setBlOrderModificationService(DefaultBlOrderModificationService blOrderModificationService) {
        this.blOrderModificationService = blOrderModificationService;
    }

}
