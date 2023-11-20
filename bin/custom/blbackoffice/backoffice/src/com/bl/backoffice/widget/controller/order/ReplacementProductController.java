package com.bl.backoffice.widget.controller.order;

import com.bl.Ordermanagement.reallocation.BlReallocationService;
import com.bl.Ordermanagement.services.BlAllocationService;
import com.bl.Ordermanagement.services.impl.DefaultBlAllocationService;
import com.bl.backoffice.wizards.util.ReplacementProductData;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.NotesEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.NotesModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.price.strategies.BlProductDynamicPriceStrategy;
import com.bl.core.product.dao.impl.DefaultBlProductDao;
import com.bl.core.stock.BlStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.product.PriceService;
import de.hybris.platform.product.UnitService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.*;

import javax.annotation.Resource;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

public class ReplacementProductController extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(ReplacementProductController.class);

    private static final String IN_SOCKET = "inputObject";
    private static final String TITLE = "customersupportbackoffice.replacement.product.confirm.title";
    private static final String CANCEL_BUTTON = "cancel";
    private static final String COMPLETE = "completed";
    private static final String OUT_CONFIRM = "confirmReplacement";
    private  static final   String CONTINUE_BUTTON ="processButton";


    private OrderModel orderModel;

    @Wire
    private Grid serialEntries;
    @Wire
    private Combobox reason;

    @WireVariable
    private transient ModelService modelService;
    @Resource
    private DefaultBlProductDao defaultBlProductDao;

    @Resource(name="blStockService")
    private BlStockService blStockService;
    
    @Resource(name="orderDao")
    private BlOrderDao orderDao;

    @Resource(name = "priceService")
    private PriceService priceService;
    @Resource(name = "blProductDynamicPriceStrategy")
    private BlProductDynamicPriceStrategy blProductDynamicPriceStrategy;
    @Resource(name = "defaultBlAllocationService")
    private DefaultBlAllocationService defaultBlAllocationService;
    @Resource(name = "unitService")
    private UnitService unitService;

    @WireVariable
    private transient BlReallocationService blReallocationService;
    @WireVariable
    private transient BlAllocationService blAllocationService;


    @SocketEvent(socketId = IN_SOCKET)
    public void initPartialRefundForm(final OrderModel inputOrder) {
        this.setOrderModel(inputOrder);
        this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
                .getLabel(TITLE)).append(
                this.getOrderModel().getCode()).toString());
        List<ReplacementProductData> replacementProductDataList=createProductReplacementDate();
        this.serialEntries.setModel(new ListModelList<>(replacementProductDataList));
        this.serialEntries.renderAll();

    }

    private     List<ReplacementProductData> createProductReplacementDate(){
        List<ReplacementProductData> replacementProductDataList=new ArrayList<>();
        orderModel.getConsignments().forEach(consignmentModel -> {
            consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
                consignmentEntryModel.getSerialProducts().forEach(blProductModel -> {
                    if (blProductModel instanceof BlSerialProductModel   && !ProductTypeEnum.SUBPARTS.equals(blProductModel.getProductType())){
                        BlSerialProductModel serialProductModel = (BlSerialProductModel)blProductModel;
                        ReplacementProductData replacementProductData= new ReplacementProductData();
                        replacementProductData.setProductName(serialProductModel.getBlProduct().getName());
                        replacementProductData.setAssignedSerial(serialProductModel.getCode());
                        replacementProductData.setOcLocation(serialProductModel.getOcLocation());
                        replacementProductData.setOldSerial(serialProductModel);
                        replacementProductData.setOrderEntry(consignmentEntryModel.getOrderEntry());
                        replacementProductData.setConsignment(consignmentModel);
                        replacementProductData.setConsEntry(consignmentEntryModel);
                        replacementProductDataList.add(replacementProductData);
                    }
                });
            });
        });
        return replacementProductDataList;
    }

    @ViewEvent(componentID = CONTINUE_BUTTON, eventName = Events.ON_CLICK)
    public void submitSerialData() {
        Optional<Component> first = this.getSerialEntriesGridRows().stream().filter(row -> Boolean.TRUE.equals(((Checkbox)
                row.getChildren().iterator().next()).isChecked())).findFirst();
        if (first.isPresent()){
            Component row =((Row) first.get());
            ReplacementProductData replacementProductData = ((Row) first.get()).getValue();
           String  newserial =((Textbox) row.getChildren().get(4)).getValue();
          String   selectedReason=( (Combobox)row.getChildren().get(5)).getValue();
            replacementProductData.setNewSerial(((Textbox) row.getChildren().get(4)).getValue());
            replacementProductData.setSelectedReason(( (Combobox)row.getChildren().get(5)).getValue());
            Messagebox.show("Confirm you want to replace "+replacementProductData.getAssignedSerial()+" with "+newserial+" on order "+orderModel.getCode(), "Replacement Confirmation", new Messagebox.Button[]
                    {Messagebox.Button.YES, Messagebox.Button.NO},null, Messagebox.QUESTION, null, clickEvent -> {
                if (Messagebox.Button.YES == clickEvent.getButton())
                {
                    validateAndUpdate(replacementProductData,row);
                }else{
                    cancelAndLeave();
                }
            }, null);
        }else {
            Messagebox.show("Please Select atleast one serial which you want to replace");
        }


    }

    private   void  validateAndUpdate(final ReplacementProductData productData, Component row){
        AbstractOrderModel actualOrder = orderDao.getOrderByCode(orderModel.getCode());
if(isSerialAvailableOnOrder(actualOrder,productData.getOldSerial())) {
    final BlSerialProductModel serial = this.defaultBlProductDao.getSerialByBarcode(productData.getNewSerial());
    if (Objects.isNull(serial)) {
        Messagebox.show("No serial present with given barcode :" + productData.getNewSerial());
        return;
    } else if (Objects.isNull(serial.getWarehouseLocation())) {
        Messagebox.show("No warehouse associated with serial");
    } else if (!blStockService.isStockAvailable(new HashSet<>(Arrays.asList(serial.getCode())), productData.getConsignment().getOptimizedShippingStartDate(), productData.getConsignment().getOptimizedShippingEndDate(), serial.getWarehouseLocation())) {
        Messagebox.show("The replacement serial is not available");
    } else {
        if (productData.getOldSerial().getBlProduct().equals(serial.getBlProduct())) {
            if (BooleanUtils.isTrue(serial.getWarehouseLocation().getCode().equals(productData.getConsignment().getWarehouse().getCode()))) {

                replaceSerialWithSameProductAndWarehouse(productData, serial, productData.getNewSerial(), productData.getOldSerial(), row);

            } else {
                // replaceSerialWithSameProductAndDifferentWarehouse()
                // same product from different warehouse
            }

        } else {
            validateAndReplaceSerilForDifferentProduct(productData, serial, productData.getNewSerial(), productData.getOldSerial(), row);
        }
    }
}else {
    Messagebox.show("The original serial is not on this order, double check.");
}

    }
private  void validateAndReplaceSerilForDifferentProduct(final ReplacementProductData productData,final BlSerialProductModel newSerial, final String barCode, final BlSerialProductModel oldSerial,
                                                         final Component row){
   if(isReplacementPossible(productData,newSerial.getBlProduct())){
                 createAndUpdateEntry(productData,newSerial,barCode,oldSerial,row);
   }else{
       Messagebox.show("Replacement not possible due to price of new serial is lower than new one");
       return;
   }
}
 private void createAndUpdateEntry(final ReplacementProductData productData,final BlSerialProductModel newSerial, final String barCode, final BlSerialProductModel oldSerial,
                                   final Component row){
        //creating new order entry
     final OrderEntryModel entryModel =createAndUpdateOrderEntry(newSerial,productData.getOrderEntry());


     final ConsignmentModel consignment = productData.getConsignment();
     final ConsignmentEntryModel consignmentEntry=createAndUpdateConsignmentEntry(newSerial,consignment,entryModel);
     modelService.save(entryModel);
     modelService.refresh(entryModel);
     modelService.save(consignmentEntry);
     modelService.refresh(consignmentEntry);
     modelService.save(consignment);
     modelService.refresh(consignment);

     //  releasing stock
     final Collection<StockLevelModel> availableStockForRelease =blStockService.getStockForSingleSerial(productData.getAssignedSerial(),consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate());
     Boolean reservedStatus=   blStockService.isActiveStatus(productData.getOldSerial().getSerialStatus())? Boolean.FALSE:Boolean.TRUE;
     blStockService.updateAndSaveStockRecord(availableStockForRelease,reservedStatus,null);

     //reserving stock
     final Collection<StockLevelModel> availableStockForReseved = blStockService.getAvailableStockForSingleSerial(newSerial.getCode(),
             consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate(),
             newSerial.getWarehouseLocation());
     blStockService.updateAndSaveStockRecord(availableStockForReseved,Boolean.TRUE,orderModel.getCode());
     removeSerialAndUpdateOrderEntry(productData,oldSerial);
  createAndUpdateOrderNotes(productData,newSerial);
     close();
 }
 private OrderEntryModel createAndUpdateOrderEntry(final BlSerialProductModel newSerial,final AbstractOrderEntryModel oldOrderEntry){
     OrderEntryModel entryModel = modelService.create(OrderEntryModel.class);
     entryModel.setEntryNumber(orderModel.getEntries().size());
     entryModel.setQuantity(1L);
     entryModel.setSerialProducts(Arrays.asList(newSerial));
     entryModel.setOrder(orderModel);
     entryModel.setProduct(newSerial.getBlProduct());
     entryModel.setUnAllocatedQuantity(0L);
     entryModel.setBasePrice(oldOrderEntry.getBasePrice());
     entryModel.setTotalPrice(oldOrderEntry.getBasePrice());
     entryModel.setDiscountValues(oldOrderEntry.getDiscountValues());
     entryModel.setUnit(unitService.getUnitForCode("pieces"));
     entryModel.setGearGuardWaiverSelected(oldOrderEntry.getGearGuardWaiverSelected());
     entryModel.setGearGuardWaiverPrice(oldOrderEntry.getGearGuardWaiverPrice());
     entryModel.setGearGuardProFullWaiverSelected(oldOrderEntry.getGearGuardProFullWaiverSelected());
     entryModel.setGearGuardProFullWaiverPrice(oldOrderEntry.getGearGuardProFullWaiverPrice());
     entryModel.setReplacementEntry(Boolean.TRUE);
     entryModel.setTaxValues(oldOrderEntry.getTaxValues());
     return entryModel;
 }
 private ConsignmentEntryModel createAndUpdateConsignmentEntry(final BlSerialProductModel newSerial,final ConsignmentModel consignment,final AbstractOrderEntryModel entryModel){
     SourcingResult sourcingResult = new SourcingResult();
     Map<Integer , Set<BlSerialProductModel>> serialProductMap = new HashMap<>();
     Set<BlSerialProductModel> serialProducts = new HashSet<>();
     serialProducts.add(newSerial);
     serialProductMap.put(entryModel.getEntryNumber(),serialProducts);
     sourcingResult.setSerialProductMap(serialProductMap);
     ConsignmentEntryModel consignmentEntry = defaultBlAllocationService.createConsignmentEntry(entryModel, 1L,consignment, sourcingResult);
     Set<ConsignmentEntryModel> consignmentEntryModels = CollectionUtils.isNotEmpty(consignment.getConsignmentEntries()) ?new HashSet<>( consignment.getConsignmentEntries()) : new HashSet<ConsignmentEntryModel>();
     consignmentEntryModels.add(consignmentEntry);
     consignment.setConsignmentEntries(consignmentEntryModels);
     return consignmentEntry;
 }
 private void removeSerialAndUpdateOrderEntry(final ReplacementProductData productData,final BlSerialProductModel oldSerial){
     AbstractOrderEntryModel orderEntry = productData.getOrderEntry();
     ConsignmentEntryModel consEntry = productData.getConsEntry();
     List<BlProductModel> serialProductOnOrderEntry= orderEntry.getSerialProducts();


            if (CollectionUtils.isNotEmpty(serialProductOnOrderEntry) && serialProductOnOrderEntry.size()>1) {

                serialProductOnOrderEntry = CollectionUtils.isNotEmpty(serialProductOnOrderEntry) ? new ArrayList<>(serialProductOnOrderEntry) : new ArrayList<>();
                serialProductOnOrderEntry.remove(oldSerial);
                orderEntry.setSerialProducts(serialProductOnOrderEntry);
                orderEntry.setQuantity(orderEntry.getQuantity() - 1L);

                List<BlProductModel> serialProducts = consEntry.getSerialProducts();
                serialProducts = CollectionUtils.isNotEmpty(serialProducts) ? new ArrayList<BlProductModel>(serialProducts) : new ArrayList<BlProductModel>();
                serialProducts.remove(oldSerial);
                consEntry.setQuantity(consEntry.getQuantity()-1L);
                consEntry.setSerialProducts(serialProducts);

                Map<String, ItemStatusEnum> items = consEntry.getItems();
                items = (items == null || items.isEmpty()) ? new HashMap<>() : new HashMap<>(items);
                items.remove(oldSerial);
                consEntry.setItems(items);

                Map<String, ConsignmentEntryStatusEnum> consignmentEntryStatus = consEntry
                        .getConsignmentEntryStatus();
                consignmentEntryStatus = (consignmentEntryStatus == null || consignmentEntryStatus.isEmpty()) ? new HashMap<>() : new HashMap<>(consignmentEntryStatus);
                consignmentEntryStatus.remove(oldSerial.getCode());
                consEntry.setConsignmentEntryStatus(consignmentEntryStatus);
                modelService.save(consEntry);
                modelService.refresh(consEntry);
            }else {
                orderEntry.setSerialProducts(new ArrayList<>());
                orderEntry.setQuantity(0L);
                orderEntry.setConsignmentEntries(new HashSet<>());
                consEntry.setSerialProducts(new ArrayList<>());
              modelService.remove(consEntry);
            }
     modelService.save(orderEntry);
     modelService.refresh(orderEntry);
 }
private boolean isReplacementPossible(final ReplacementProductData productData,final BlProductModel newProduct){
    final LocalDateTime rentalStartDate = BlDateTimeUtils.getFormattedDateTime(productData.getConsignment().getRentalStartDate());
    final LocalDateTime rentalEndDate = BlDateTimeUtils.getFormattedDateTime(productData.getConsignment().getRentalEndDate());
    final long rentalDays = ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate);
    PriceInformation priceinfo = getWebPriceForProduct(newProduct, rentalDays);
    double basePrice = productData.getOrderEntry().getBasePrice();

    if (priceinfo.getPriceValue().getValue()>basePrice){
        return true;
    }else {
        return false;
    }
}
    public PriceInformation getWebPriceForProduct(final ProductModel product,final Long rentalDays)
    {
        if (PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product) && !((BlProductModel) product).isBundleProduct())
        {
            validateParameterNotNull(product, "Product cannot be null");
            final List<PriceInformation> prices = priceService.getPriceInformationsForProduct(product);
            if (CollectionUtils.isNotEmpty(prices))
            {
                final PriceInformation defaultPriceInformation = prices.get(0);
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Default Price is {} for product {}",
                        defaultPriceInformation.getPriceValue().getValue(), product.getCode());
                return Objects.nonNull(rentalDays) && rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY
                        ? blProductDynamicPriceStrategy.getDynamicPriceInformationForProduct((BlProductModel) product,
                        defaultPriceInformation, rentalDays)
                        : defaultPriceInformation;
            }

        }
        return null;
    }
    protected void replaceSerialWithSameProductAndWarehouse(final ReplacementProductData productData,final BlSerialProductModel newSerial, final String barCode, final BlSerialProductModel oldSerial,
                              final Component row) {

        ConsignmentEntryModel consEntry = productData.getConsEntry();
  updatingBothEntry(consEntry,newSerial,oldSerial);

        modelService.save(consEntry.getOrderEntry());
        modelService.refresh(consEntry.getOrderEntry());
        modelService.save(consEntry);
        modelService.refresh(consEntry);

              //  releasing stock
        final Collection<StockLevelModel> availableStockForRelease =blStockService.getStockForSingleSerial(productData.getAssignedSerial(),productData.getConsignment().getOptimizedShippingStartDate(), productData.getConsignment().getOptimizedShippingEndDate());
        Boolean reservedStatus=   blStockService.isActiveStatus(productData.getOldSerial().getSerialStatus())? Boolean.FALSE:Boolean.TRUE;
        blStockService.updateAndSaveStockRecord(availableStockForRelease,reservedStatus,null);

        //reserving stock
        final Collection<StockLevelModel> availableStockForReseved = blStockService.getAvailableStockForSingleSerial(newSerial.getCode(),
                productData.getConsignment().getOptimizedShippingStartDate(), productData.getConsignment().getOptimizedShippingEndDate(),
                newSerial.getWarehouseLocation());
        blStockService.updateAndSaveStockRecord(availableStockForReseved,Boolean.TRUE,orderModel.getCode());

        createAndUpdateOrderNotes(productData,newSerial);
        updateConsignmentEntryQuantity(productData.getConsEntry());
        close();
    }
private   boolean isSerialAvailableOnOrder(AbstractOrderModel order,BlSerialProductModel serialProduct){
   for (ConsignmentModel consignmentModel :orderModel.getConsignments()){
        for(ConsignmentEntryModel consignmentEntryModel :consignmentModel.getConsignmentEntries()) {
            for(BlProductModel blProductModel: consignmentEntryModel.getSerialProducts()){
                if (blProductModel.getCode().equals(serialProduct.getCode()) ) {
                    return true;
                }
            }
        }
    }
                    return Boolean.FALSE;
}
private  ConsignmentEntryModel isOrderEntryAlreadyPresent(AbstractOrderModel order,BlSerialProductModel serialProduct){
    for (ConsignmentModel consignmentModel :orderModel.getConsignments()){
        for(ConsignmentEntryModel consignmentEntryModel :consignmentModel.getConsignmentEntries()) {
            for(BlProductModel blProductModel: consignmentEntryModel.getSerialProducts()){
                if (blProductModel.getCode().equals(serialProduct.getCode()) ) {
                    return consignmentEntryModel;
                }
            }
        }
    }
    return null;
}
    private void updatingBothEntry(final ConsignmentEntryModel consEntry,final BlSerialProductModel newSerial,final BlSerialProductModel oldSerial){
        List<BlProductModel> serialProducts = consEntry.getSerialProducts();
        serialProducts = CollectionUtils.isNotEmpty(serialProducts)?new ArrayList<BlProductModel>(serialProducts) :new ArrayList<BlProductModel>();
        serialProducts.add(newSerial);
        serialProducts.remove(oldSerial);
        consEntry.setSerialProducts(serialProducts);

        Map<String, ItemStatusEnum> items = consEntry.getItems();
        items = (items == null || items.isEmpty()) ? new HashMap<>()  : new HashMap<>(items);
        if(items.isEmpty()){
            items.put(newSerial.getCode(),ItemStatusEnum.NOT_INCLUDED);
        }else {
            items.put(newSerial.getCode(), items.get(oldSerial));
            items.remove(oldSerial);
        }
        consEntry.setItems(items);

        Map<String, ConsignmentEntryStatusEnum> consignmentEntryStatus = consEntry
                .getConsignmentEntryStatus();
        consignmentEntryStatus= (consignmentEntryStatus == null || consignmentEntryStatus.isEmpty()) ? new HashMap<>() : new HashMap<>(consignmentEntryStatus);
        if (consignmentEntryStatus.isEmpty()){
            consignmentEntryStatus.put(newSerial.getCode(),ConsignmentEntryStatusEnum.NOT_SHIPPED);
        }else {
            consignmentEntryStatus
                    .put(newSerial.getCode(), consignmentEntryStatus.get(oldSerial.getCode()));
            consignmentEntryStatus.remove(oldSerial.getCode());
        }
        consEntry.setConsignmentEntryStatus(consignmentEntryStatus);

        List<BlProductModel> serialProductOnOrderEntry = consEntry.getOrderEntry()
                .getSerialProducts();
        serialProductOnOrderEntry = CollectionUtils.isNotEmpty(serialProductOnOrderEntry) ? new ArrayList<>(serialProductOnOrderEntry) : new ArrayList<>();
        serialProductOnOrderEntry.add(newSerial);
        serialProductOnOrderEntry.remove(oldSerial);
        consEntry.getOrderEntry().setSerialProducts(serialProductOnOrderEntry);
    }
    /**
     * @param entry
     */
    private void updateConsignmentEntryQuantity(final ConsignmentEntryModel entry)
    {
        final long quantity = entry.getSerialProducts().stream()
                .filter(blSerialProduct -> blSerialProduct instanceof BlSerialProductModel).collect(Collectors.toList()).size();
        entry.setQuantity(quantity);
        modelService.save(entry);

    }

    private void createAndUpdateOrderNotes(final ReplacementProductData productData,BlSerialProductModel newSerial){
        final NotesModel notesModel = modelService.create(NotesModel.class);
        notesModel.setType(NotesEnum.ORDER_NOTES);
        notesModel.setNote("Replacement for "+productData.getOldSerial().getCode()+" "+productData.getOldSerial().getBlProduct().getName()+" to "+newSerial.getCode()+" "+newSerial.getBlProduct().getName()+" - "+productData.getSelectedReason());
        notesModel.setUserID(orderModel.getUser().getUid());
       modelService.save(notesModel);
        if (CollectionUtils.isNotEmpty(orderModel.getOrderNotes()))
        {
            final List<NotesModel> allOrderNotes = Lists.newArrayList(orderModel.getOrderNotes());
            allOrderNotes.add(notesModel);
            orderModel.setOrderNotes(allOrderNotes);
        }
        else
        {
            orderModel.setOrderNotes(Lists.newArrayList(notesModel));
        }
 modelService.save(orderModel);
        modelService.refresh(orderModel);
    }

    private void cancelAndLeave(){
       // Messagebox.show("from cancle");
        close();
    }
    private List<Component> getSerialEntriesGridRows()
    {
        return this.serialEntries.getRows().getChildren();
    }

        @ViewEvent(componentID = CANCEL_BUTTON, eventName = Events.ON_CLICK)
    public void close() {
        this.sendOutput(OUT_CONFIRM, COMPLETE);
    }

    public OrderModel getOrderModel() {
        return orderModel;
    }

    public void setOrderModel(OrderModel orderModel) {
        this.orderModel = orderModel;
    }
    public BlAllocationService getBlAllocationService() {
        return blAllocationService;
    }

    public void setBlAllocationService(BlAllocationService blAllocationService) {
        this.blAllocationService = blAllocationService;
    }
}
