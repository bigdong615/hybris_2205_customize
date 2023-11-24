package com.bl.backoffice.widget.controller.order;

import com.bl.Ordermanagement.services.impl.DefaultBlAllocationService;
import com.bl.backoffice.wizards.util.ReplacementProductData;
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
import com.bl.core.service.BlBackOfficePriceService;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.product.PriceService;
import de.hybris.platform.product.UnitService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.data.sourcing.SourcingResult;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.*;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

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
    @Resource(name = "blBackOfficePriceService")
    private BlBackOfficePriceService blBackOfficePriceService;

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

            final BlSerialProductModel serial = this.defaultBlProductDao.getSerialByBarcode(replacementProductData.getNewSerial());
            if (Objects.isNull(serial)) {
                Messagebox.show("No serial present with given barcode :" + replacementProductData.getNewSerial());
                return;
            }
            Messagebox.show("Confirm you want to replace "+replacementProductData.getAssignedSerial()+" "+replacementProductData.getOldSerial().getBlProduct().getName()+" with "+newserial+" "+serial.getBlProduct().getName()+" on order "+orderModel.getCode(), "Replacement Confirmation", new Messagebox.Button[]
                    {Messagebox.Button.YES, Messagebox.Button.NO},null, Messagebox.QUESTION, null, clickEvent -> {
                if (Messagebox.Button.YES == clickEvent.getButton())
                {
                    validateAndUpdate(replacementProductData,row,serial);
                }else{
                    close();
                }
            }, null);
        }else {
            Messagebox.show("Please Select atleast one serial which you want to replace");
        }


    }

    private   void  validateAndUpdate(final ReplacementProductData productData, Component row,final BlSerialProductModel serial){
        AbstractOrderModel actualOrder = orderDao.getOrderByCode(orderModel.getCode());
if(isSerialAvailableOnOrder(actualOrder,productData.getOldSerial())) {
     if (Objects.isNull(serial.getWarehouseLocation())) {
        Messagebox.show("No warehouse associated with serial");
    } else if (!blStockService.isStockAvailable(new HashSet<>(Arrays.asList(serial.getCode())), productData.getConsignment().getOptimizedShippingStartDate(), productData.getConsignment().getOptimizedShippingEndDate(), serial.getWarehouseLocation())) {
        Messagebox.show("The replacement serial is not available");
    } else {
         BlLogger.logFormatMessageInfo(LOG, Level.INFO,"Going to replace serial: {} - product: {} with serial: {} - product {} for the order {}",
                 productData.getOldSerial().  getCode(),productData.getOldSerial().getBlProduct().getName(),serial.getCode(),serial.getBlProduct().getName(),orderModel.getCode());
        if (productData.getOldSerial().getBlProduct().getCode().equals(serial.getBlProduct().getCode())) {
            if (BooleanUtils.isTrue(serial.getWarehouseLocation().getCode().equals(productData.getConsignment().getWarehouse().getCode()))) {
                replaceSerialWithSameProductAndWarehouse(productData, serial,productData.getOldSerial(), row);
            } else {
                // replaceSerialWithSameProductAndDifferentWarehouse()
                // same product from different warehouse
            }

        } else {
            if (BooleanUtils.isTrue(serial.getWarehouseLocation().getCode().equals(productData.getConsignment().getWarehouse().getCode()))) {
                validateAndReplaceSerilForDifferentProduct(productData, serial, productData.getNewSerial(), productData.getOldSerial(), row);
            }else {
                // replaceSerialWithDifferentProductAndDifferentWarehouse()
            }
        }
    }
}else {
    Messagebox.show("The original serial is not on this order, double check.");
}

    }
private  void validateAndReplaceSerilForDifferentProduct(final ReplacementProductData productData,final BlSerialProductModel newSerial, final String barCode, final BlSerialProductModel oldSerial,
                                                         final Component row){
   if(isReplacementPossible(productData,newSerial.getBlProduct())){
       try {
           createAndUpdateEntry(productData, newSerial, barCode, oldSerial, row);
       }catch (Exception e){
  BlLogger.logFormattedMessage(LOG,Level.INFO,"300",e,"Some error occure while replacement of serial {} for different product {} for the order {}",
          newSerial.getCode(),newSerial.getBlProduct().getCode(),orderModel.getCode());
       }
   }else{
       Messagebox.show("Replacement not possible due to price of new serial is lower than old one");
       return;
   }
}
 private void createAndUpdateEntry(final ReplacementProductData productData,final BlSerialProductModel newSerial, final String barCode, final BlSerialProductModel oldSerial,
                                   final Component row){
     AbstractOrderEntryModel entryModel = isOrderEntryAlreadyPresent(orderModel, newSerial);
     if (entryModel!=null){
         //updating older entry
         entryModel.setQuantity(entryModel.getQuantity()+1);
         final ArrayList<BlProductModel> serialList =CollectionUtils.isNotEmpty(entryModel.getSerialProducts())? Lists.newArrayList(entryModel.getSerialProducts()):Lists.newArrayList();
         serialList.add(newSerial);
         entryModel.setSerialProducts(serialList);
     }else {
         //creating and updating new order entry
          entryModel = createAndUpdateOrderEntry(newSerial, productData.getOrderEntry());
     }
     final ConsignmentModel consignment = productData.getConsignment();
     ConsignmentEntryModel consignmentEntry = isConsignmentEntryAlreadyPresent(orderModel, newSerial);
     if (null!=consignmentEntry){
         //updating consignment entry
         consignmentEntry.setQuantity(consignmentEntry.getQuantity()+1L);
         final ArrayList<BlProductModel> serialProduct = CollectionUtils.isNotEmpty(consignmentEntry.getSerialProducts())?Lists.newArrayList(consignmentEntry.getSerialProducts()):Lists.newArrayList();
         serialProduct.add(newSerial);
         consignmentEntry.setSerialProducts(serialProduct);

         HashMap<String, ItemStatusEnum> itemsMaps = (consignmentEntry.getItems() == null || consignmentEntry.getItems().isEmpty())? Maps.newHashMap() : Maps.newHashMap(consignmentEntry.getItems());
         itemsMaps.put(newSerial.getCode(),ItemStatusEnum.NOT_INCLUDED);
         consignmentEntry.setItems(itemsMaps);

         HashMap<String, ConsignmentEntryStatusEnum> statusMaps = (consignmentEntry.getConsignmentEntryStatus() == null || consignmentEntry.getConsignmentEntryStatus().isEmpty()) ?Maps.newHashMap():Maps.newHashMap(consignmentEntry.getConsignmentEntryStatus());
         statusMaps.put(newSerial.getCode(),ConsignmentEntryStatusEnum.NOT_SHIPPED);
consignmentEntry.setConsignmentEntryStatus(statusMaps);
         final Set<ConsignmentEntryModel> consignmentEntries = new HashSet<>();
         if (entryModel.getConsignmentEntries() != null) {
             entryModel.getConsignmentEntries().forEach(consignmentEntries::add);
         }

         consignmentEntries.add(consignmentEntry);
         entryModel.setConsignmentEntries(consignmentEntries);
     }else {
      consignmentEntry=createAndUpdateConsignmentEntry(newSerial,consignment,entryModel);
     }
     modelService.save(entryModel);
     modelService.refresh(entryModel);
     modelService.save(consignmentEntry);
     modelService.refresh(consignmentEntry);
     modelService.save(consignment);
     modelService.refresh(consignment);
        //updating stock records
     updateStockRecords(productData,newSerial);
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
     BlLogger.logFormatMessageInfo(LOG,Level.INFO,"New order entry created for product {} while replacement of serial {} for the order {}",
             newSerial.getBlProduct().getCode(),newSerial.getCode(),orderModel.getCode());
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
     BlLogger.logFormatMessageInfo(LOG,Level.INFO,"New consignment entry created for serial {} for product {} while replacement of serial for the order {}",
             newSerial.getCode(),newSerial.getBlProduct().getCode(),orderModel.getCode());
     return consignmentEntry;
 }
 private void removeSerialAndUpdateOrderEntry(final ReplacementProductData productData,final BlSerialProductModel oldSerial){

        AbstractOrderEntryModel orderEntry = productData.getOrderEntry();
     ConsignmentEntryModel consEntry = productData.getConsEntry();
     List<BlProductModel> serialProductOnOrderEntry= orderEntry.getSerialProducts();
     BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Updating older order entry {} and consignment entry {} for older serial {} after replacement of serial for the order {}",
             orderEntry.getPk(),consEntry.getPk(),oldSerial.getCode(),orderModel.getCode());
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
                items.remove(oldSerial.getCode());
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
              modelService.remove(consEntry);
            }
     modelService.save(orderEntry);
     modelService.refresh(orderEntry);
 }
private boolean isReplacementPossible(final ReplacementProductData productData,final BlProductModel newProduct) {
    boolean replacementPossible = false;
    try {
        BigDecimal productPrice = blBackOfficePriceService.getProductPrice(newProduct, orderModel.getRentalStartDate(), orderModel.getRentalEndDate(), false);
        double basePrice = productData.getOrderEntry().getBasePrice();
        BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Price of replacement product {} and old product {} for the order {}",
                productPrice.doubleValue(),basePrice,orderModel.getCode());
        replacementPossible = productPrice.doubleValue() > basePrice ? true : false;
    } catch (Exception parse) {
        BlLogger.logFormattedMessage(LOG,Level.INFO,"300",parse,"Some error occure in replacement flow while fetching price of product {} for duration {} - {} for the order {}",
                newProduct.getCode(),orderModel.getRentalStartDate(),orderModel.getRentalEndDate(),orderModel.getCode());
            }
    return replacementPossible;
}

    protected void replaceSerialWithSameProductAndWarehouse(final ReplacementProductData productData,final BlSerialProductModel newSerial, final BlSerialProductModel oldSerial,
                              final Component row) {

        ConsignmentEntryModel consEntry = productData.getConsEntry();
        try {
            updatingBothEntry(consEntry, newSerial, oldSerial);
            modelService.save(consEntry.getOrderEntry());
            modelService.refresh(consEntry.getOrderEntry());
            modelService.save(consEntry);
            modelService.refresh(consEntry);
            //updating stock records
            updateStockRecords(productData, newSerial);
            createAndUpdateOrderNotes(productData, newSerial);
        }catch (Exception e){
            BlLogger.logFormattedMessage(LOG,Level.INFO,"300",e,"Some error occure while replacement of serial from ca agent for the order {}",orderModel.getCode());
        }
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
private  ConsignmentEntryModel isConsignmentEntryAlreadyPresent(AbstractOrderModel order,BlSerialProductModel serialProduct){
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
    private  AbstractOrderEntryModel isOrderEntryAlreadyPresent(AbstractOrderModel order,BlSerialProductModel serialProduct){
        for (AbstractOrderEntryModel entryModel :orderModel.getEntries()){
            if (entryModel.getProduct().getCode().equals(serialProduct.getBlProduct().getCode())){
                return entryModel;
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
        items = (items == null || items.isEmpty()) ? Maps.newHashMap()  : Maps.newHashMap(items);
        if(items.isEmpty()){
            items.put(newSerial.getCode(),ItemStatusEnum.NOT_INCLUDED);
        }else {
            items.put(newSerial.getCode(), items.get(oldSerial.getCode()));
            items.remove(oldSerial.getCode());
        }
        consEntry.setItems(items);

        Map<String, ConsignmentEntryStatusEnum> consignmentEntryStatus = consEntry
                .getConsignmentEntryStatus();
        consignmentEntryStatus= (consignmentEntryStatus == null || consignmentEntryStatus.isEmpty()) ? Maps.newHashMap() : Maps.newHashMap(consignmentEntryStatus);
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
        serialProductOnOrderEntry = CollectionUtils.isNotEmpty(serialProductOnOrderEntry) ? Lists.newArrayList(serialProductOnOrderEntry) :Lists.newArrayList();
        serialProductOnOrderEntry.add(newSerial);
        serialProductOnOrderEntry.remove(oldSerial);
        consEntry.getOrderEntry().setSerialProducts(serialProductOnOrderEntry);
    }
    private void updateStockRecords(final ReplacementProductData productData,BlSerialProductModel newSerial){
        ConsignmentModel consignment = productData.getConsignment();
        //  releasing stock
        final Collection<StockLevelModel> availableStockForRelease =blStockService.getStockForSingleSerial(productData.getAssignedSerial(),consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate());
        Boolean reservedStatus=   blStockService.isActiveStatus(productData.getOldSerial().getSerialStatus())? Boolean.FALSE:Boolean.TRUE;
       BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Stock updating while release stock for the serial {}, duration {} - {}, reserve status {} for the order {}",
               productData.getAssignedSerial(),consignment.getOptimizedShippingStartDate(),consignment.getOptimizedShippingEndDate(),reservedStatus,orderModel.getCode());
        blStockService.updateAndSaveStockRecord(availableStockForRelease,reservedStatus,null);

        //reserving stock
        final Collection<StockLevelModel> availableStockForReseved = blStockService.getAvailableStockForSingleSerial(newSerial.getCode(),
                consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate(),
                newSerial.getWarehouseLocation());
        BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Reserve stock while replacement serial for the serial {} duration {} - {} for the order {}",
                newSerial.getCode(),consignment.getOptimizedShippingStartDate(),consignment.getOptimizedShippingEndDate(),orderModel.getCode());
        blStockService.updateAndSaveStockRecord(availableStockForReseved,Boolean.TRUE,orderModel.getCode());

    }

    private void createAndUpdateOrderNotes(final ReplacementProductData productData,BlSerialProductModel newSerial){
        final NotesModel notesModel = modelService.create(NotesModel.class);
        notesModel.setType(NotesEnum.ORDER_NOTES);
        String replacementNotes ="Replacement for "+productData.getOldSerial().getCode()+" "+productData.getOldSerial().getBlProduct().getName()+" to "+newSerial.getCode()+" "+newSerial.getBlProduct().getName()+" - "+productData.getSelectedReason() ;
        notesModel.setNote(replacementNotes);
        notesModel.setUserID(orderModel.getUser().getUid());
       modelService.save(notesModel);
       BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Created order notes {}: {} :for the order {}",notesModel.getPk(),replacementNotes,orderModel.getCode());
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
}
