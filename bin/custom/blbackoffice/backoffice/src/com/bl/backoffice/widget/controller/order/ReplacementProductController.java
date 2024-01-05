package com.bl.backoffice.widget.controller.order;

import com.bl.Ordermanagement.constants.BlOrdermanagementConstants;
import com.bl.Ordermanagement.services.impl.DefaultBlAllocationService;
import com.bl.backoffice.wizards.util.ReplacementProductData;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.NotesEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
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
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Sets;
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
    @Wire
    private  Checkbox sendEmail;
    @Wire
    private Textbox customerEmailNotes;

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

    @Resource
    private DefaultBlESPEventService blEspEventService;

    @SocketEvent(socketId = IN_SOCKET)
    public void initPartialRefundForm(final OrderModel inputOrder) {
        sendEmail.setChecked(Boolean.TRUE);
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
                replaceSerialWithSameProductAndWarehouse(productData, serial);
            } else {
                 replaceSerialWithSameProductAndDifferentWarehouse(productData,serial);
            }

        } else {
            if (BooleanUtils.isTrue(serial.getWarehouseLocation().getCode().equals(productData.getConsignment().getWarehouse().getCode()))) {
                validateAndReplaceSerilForDifferentProduct(productData, serial);
            }else {
                 replaceSerialWithDifferentProductAndDifferentWarehouse(productData,serial);
                 close();
            }
        }
    }
}else {
    Messagebox.show("The original serial is not on this order, double check.");
}
    }

    /**
     * This Method used to replace serial for different product and same warehouse.
     * @param productData
     * @param newSerial
     */
    private  void validateAndReplaceSerilForDifferentProduct(final ReplacementProductData productData,final BlSerialProductModel newSerial){
   if(isReplacementPossible(productData,newSerial.getBlProduct())){
       try {
           createAndUpdateEntry(productData, newSerial);
       }catch (Exception e){
  BlLogger.logFormattedMessage(LOG,Level.INFO,"300",e,"Some error occure while replacement of serial {} for different product {} for the order {}",
          newSerial.getCode(),newSerial.getBlProduct().getCode(),orderModel.getCode());
       }
   }else{
       Messagebox.show("Replacement not possible due to price of new serial is lower than old one");
       return;
   }
}

    /**
     * This method used to replace serial in case of different product and different warehouse.
     * @param productData
     * @param newSerial
     */
    private void replaceSerialWithDifferentProductAndDifferentWarehouse(final ReplacementProductData productData,final BlSerialProductModel newSerial){
        if(isReplacementPossible(productData,newSerial.getBlProduct())){
            try {
                // Update
                AbstractOrderEntryModel orderEntry = createAndUpdateOrderEntry(productData,newSerial);
createOrUpdateConsignment(orderEntry,newSerial);

                // Remove old serial from consignment entry.
                removeOldSerialFromConsEntry(productData);

    // Remove old Serial from old order entry.
                AbstractOrderEntryModel olderOrderEntry = productData.getOrderEntry();
                List<BlProductModel> serialProductOnOrderEntry= olderOrderEntry.getSerialProducts();
                if (CollectionUtils.isNotEmpty(serialProductOnOrderEntry) && serialProductOnOrderEntry.size()>1) {
                    serialProductOnOrderEntry = Lists.newArrayList(serialProductOnOrderEntry);
                    serialProductOnOrderEntry.remove(productData.getOldSerial());
                    olderOrderEntry.setSerialProducts(serialProductOnOrderEntry);
                    olderOrderEntry.setQuantity(olderOrderEntry.getQuantity() - 1L);
                }else {
                    olderOrderEntry.setSerialProducts(new ArrayList<>());
                    olderOrderEntry.setQuantity(0L);
                    olderOrderEntry.setConsignmentEntries(new HashSet<>());
                } // end of remove old Serial from old order entry.
                modelService.save(olderOrderEntry);
                modelService.refresh(olderOrderEntry);
                modelService.save(orderEntry);
                modelService.refresh(orderEntry);
                createAndUpdateOrderNotes(productData,newSerial);
            }catch (Exception e){
                BlLogger.logFormattedMessage(LOG,Level.INFO,"300",e,"Some error occure while replacement of serial {} for different product {} for the order {}",
                        newSerial.getCode(),newSerial.getBlProduct().getCode(),orderModel.getCode());
            }
        }else{
            Messagebox.show("Replacement not possible due to price of new serial is lower than old one");
            return;
        }
    }

    /**
     * This method used for update both entry in case of different product and same warehouse.
     * @param productData
     * @param newSerial
     */
 private void createAndUpdateEntry(final ReplacementProductData productData,final BlSerialProductModel newSerial){
     AbstractOrderEntryModel entryModel = createAndUpdateOrderEntry(productData,newSerial);

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
     removeSerialAndUpdateOrderEntry(productData,productData.getOldSerial());
  createAndUpdateOrderNotes(productData,newSerial);
     close();
 }

    /**
     * Updating order entry in case of different product.
     * @param productData
     * @param newSerial
     * @return
     */
 private AbstractOrderEntryModel createAndUpdateOrderEntry(final ReplacementProductData productData,final BlSerialProductModel newSerial){
     AbstractOrderEntryModel entryModel = isOrderEntryAlreadyPresent(orderModel, newSerial);
     if (entryModel!=null){
         BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Updating order entry for replacement of serial {} for already present order entry {} for the order {}",
                 newSerial.getCode(),entryModel.getProduct().getCode(),orderModel.getCode());
         //updating older entry
         entryModel.setQuantity(entryModel.getQuantity()+1);
         final ArrayList<BlProductModel> serialList =CollectionUtils.isNotEmpty(entryModel.getSerialProducts())? Lists.newArrayList(entryModel.getSerialProducts()):Lists.newArrayList();
         serialList.add(newSerial);
         entryModel.setSerialProducts(serialList);
     }else {
         BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Creating new order entry for serial {} while serial replacement for the order {}",
                 newSerial.getCode(),orderModel.getCode());
         //creating and updating new order entry
         entryModel = createOrderEntry(newSerial, productData.getOrderEntry());
     }
     return entryModel;
 }

    /**
     * This method used to create new order entry.
     * @param newSerial
     * @param oldOrderEntry
     * @return
     */
 private OrderEntryModel createOrderEntry(final BlSerialProductModel newSerial,final AbstractOrderEntryModel oldOrderEntry){
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
     entryModel.setOldProduct((BlProductModel) oldOrderEntry.getProduct());
     BlLogger.logFormatMessageInfo(LOG,Level.INFO,"New order entry created for product {} while replacement of serial {} for the order {}",
             newSerial.getBlProduct().getCode(),newSerial.getCode(),orderModel.getCode());
     return entryModel;
 }

  /**
     * This method used to create new consignment entry.
     * @param newSerial
     * @param consignment
     * @param entryModel
     * @return
     */
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

    /**
     * This method used to remove old serial related data from both entry.
     * @param productData
     * @param oldSerial
     */
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

    /**
     * This method used to compare price to get possibility of replacement in case of different product.
     * @param productData
     * @param newProduct
     * @return
     */
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

    /**
     * This method used to replace serial in case of same product and same warehouse.
     * @param productData
     * @param newSerial
     */
    protected void replaceSerialWithSameProductAndWarehouse(final ReplacementProductData productData,final BlSerialProductModel newSerial) {

        ConsignmentEntryModel consEntry = productData.getConsEntry();
        try {
            updatingConsignmentEntry(consEntry, newSerial, productData.getOldSerial());
            modelService.save(consEntry);
            modelService.refresh(consEntry);

            List<BlProductModel> serialProductOnOrderEntry = consEntry.getOrderEntry()
                    .getSerialProducts();
            serialProductOnOrderEntry = CollectionUtils.isNotEmpty(serialProductOnOrderEntry) ? Lists.newArrayList(serialProductOnOrderEntry) :Lists.newArrayList();
            serialProductOnOrderEntry.add(newSerial);
            serialProductOnOrderEntry.remove(productData.getOldSerial());
            consEntry.getOrderEntry().setSerialProducts(serialProductOnOrderEntry);
            modelService.save(consEntry.getOrderEntry());
            modelService.refresh(consEntry.getOrderEntry());

            //updating stock records
            updateStockRecords(productData, newSerial);
            createAndUpdateOrderNotes(productData, newSerial);
        }catch (Exception e){
            BlLogger.logFormattedMessage(LOG,Level.INFO,"300",e,"Some error occure while replacement of serial from ca agent for the order {}",orderModel.getCode());
        }
        close();
    }

    /**
     * This method used to check is given serial present on order or not?
     * @param order
     * @param serialProduct
     * @return
     */
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

    /**
     * This method used to check any consignment entry present on give order or not for given serial.
     * @param order
     * @param serialProduct
     * @return
     */
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

    /**
     * This method used to check any order entry present on give order or not for given serial.
     * @param order
     * @param serialProduct
     * @return
     */
    private  AbstractOrderEntryModel isOrderEntryAlreadyPresent(AbstractOrderModel order,BlSerialProductModel serialProduct){
        for (AbstractOrderEntryModel entryModel :orderModel.getEntries()){
            if (entryModel.getProduct().getCode().equals(serialProduct.getBlProduct().getCode())){
                return entryModel;
            }
        }
        return null;
    }

    /**
     * Updating consignment entry if replacement of serial for same product and same warehouse.
     * @param consEntry
     * @param newSerial
     * @param oldSerial
     */
    private void updatingConsignmentEntry(final ConsignmentEntryModel consEntry,final BlSerialProductModel newSerial,final BlSerialProductModel oldSerial){
        List<BlProductModel> serialProducts = consEntry.getSerialProducts();
        serialProducts = CollectionUtils.isNotEmpty(serialProducts)?Lists.newArrayList(serialProducts) :Lists.newArrayList();
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
    }

    /**
     * This method used to add new serial on given consignment entry.
     * @param consEntry
     * @param newSerial
     */
    private void addNewSerialToConsignmentEntryIfAlreadyExist(final ConsignmentEntryModel consEntry,final BlSerialProductModel newSerial){
        List<BlProductModel> serialProducts =Lists.newArrayList( consEntry.getSerialProducts());
        serialProducts.add(newSerial);
        consEntry.setSerialProducts(serialProducts);

        Map<String, ItemStatusEnum> items = Maps.newHashMap( consEntry.getItems());
            items.put(newSerial.getCode(),ItemStatusEnum.NOT_INCLUDED);
        consEntry.setItems(items);

        Map<String, ConsignmentEntryStatusEnum> consignmentEntryStatus = Maps.newHashMap(consEntry
                .getConsignmentEntryStatus());
            consignmentEntryStatus.put(newSerial.getCode(),ConsignmentEntryStatusEnum.NOT_SHIPPED);
        consEntry.setConsignmentEntryStatus(consignmentEntryStatus);
    }

    /**
     * This method used to update stock records.
     * @param productData
     * @param newSerial
     */
    private void updateStockRecords(final ReplacementProductData productData,BlSerialProductModel newSerial){
        ConsignmentModel consignment = productData.getConsignment();
        //  releasing stock
        releaseStock(productData);
        //reserving stock
       reservedStock(consignment,newSerial);
    }

    private void releaseStock(final ReplacementProductData productData){
        ConsignmentModel consignment = productData.getConsignment();
        final Collection<StockLevelModel> availableStockForRelease =blStockService.getStockForSingleSerial(productData.getAssignedSerial(),consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate());
        Boolean reservedStatus=   blStockService.isActiveStatus(productData.getOldSerial().getSerialStatus())? Boolean.FALSE:Boolean.TRUE;
        BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Stock updating while release stock for the serial {}, duration {} - {}, reserve status {} for the order {}",
                productData.getAssignedSerial(),consignment.getOptimizedShippingStartDate(),consignment.getOptimizedShippingEndDate(),reservedStatus,orderModel.getCode());
        blStockService.updateAndSaveStockRecord(availableStockForRelease,reservedStatus,null);
    }
    private void reservedStock(final ConsignmentModel consignment,BlSerialProductModel newSerial){
        final Collection<StockLevelModel> availableStockForReseved = blStockService.getAvailableStockForSingleSerial(newSerial.getCode(),
                consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate(),
                newSerial.getWarehouseLocation());
        BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Reserve stock while replacement serial for the serial {} duration {} - {} for the order {}",
                newSerial.getCode(),consignment.getOptimizedShippingStartDate(),consignment.getOptimizedShippingEndDate(),orderModel.getCode());
        blStockService.updateAndSaveStockRecord(availableStockForReseved,Boolean.TRUE,orderModel.getCode());

    }

    /**
     * This method used to create order notes  and update on order.
     * @param productData
     * @param newSerial
     */
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
        if( sendEmail.isChecked()){
            getBlEspEventService().sendReplacementProductEvent(orderModel,productData.getOldSerial().getBlProduct().getName(),newSerial.getBlProduct().getName(),customerEmailNotes.getValue());
        }
    }

    /**
     * This method used to replace serial for  same product and different warehouse.
     * @param productData
     * @param newSerial
     */
  private void   replaceSerialWithSameProductAndDifferentWarehouse(final ReplacementProductData productData,final BlSerialProductModel newSerial){
     AbstractOrderEntryModel orderEntry = productData.getOrderEntry();
      BlSerialProductModel oldSerial = productData.getOldSerial();
      List<BlProductModel> serialProducts = Lists.newArrayList(orderEntry.getSerialProducts());
      serialProducts.add(newSerial);
      serialProducts.remove(oldSerial);
      orderEntry.setSerialProducts(serialProducts);
      createOrUpdateConsignment(orderEntry,newSerial);
      removeOldSerialFromConsEntry(productData);
modelService.save(orderEntry);
modelService.refresh(orderEntry);
      createAndUpdateOrderNotes(productData,newSerial);
close();
  }

    /**
     * This method used to remove old serial from consignment entry.
     * @param productData
     */
  private void removeOldSerialFromConsEntry(final ReplacementProductData productData){
      ConsignmentEntryModel oldConsEntry = productData.getConsEntry();
      List<BlProductModel> serials = oldConsEntry.getSerialProducts();
      BlSerialProductModel oldSerial = productData.getOldSerial();
      List<BlProductModel> remainingSerial = serials.stream().filter(product -> (product instanceof BlSerialProductModel) && !product.getProductType().getCode().equals(ProductTypeEnum.SUBPARTS)).collect(Collectors.toList());
      remainingSerial.remove(oldSerial);
      if(CollectionUtils.isEmpty(remainingSerial)){
          ConsignmentModel consignment = oldConsEntry.getConsignment();
          Set<ConsignmentEntryModel> consignmentEntries = consignment.getConsignmentEntries();
          if (consignmentEntries.isEmpty() || consignmentEntries.contains(oldConsEntry) && consignmentEntries.size()==1){
              modelService.remove(oldConsEntry);
              modelService.remove(consignment);
          }else {
              consignmentEntries = Sets.newHashSet(consignmentEntries);
              consignmentEntries.remove(oldConsEntry);
              consignment.setConsignmentEntries(consignmentEntries);
              modelService.remove(oldConsEntry);
              modelService.save(consignment);
              modelService.refresh(consignment);
          }
      }else {
          serials = Lists.newArrayList(serials) ;
          serials.remove(oldSerial);
          Map<String, ItemStatusEnum> items = Maps.newHashMap(oldConsEntry.getItems());
          items.remove(oldSerial.getCode());
          Map<String, ConsignmentEntryStatusEnum> consignmentEntryStatus = Maps.newHashMap(oldConsEntry.getConsignmentEntryStatus());
consignmentEntryStatus.remove(oldSerial.getCode());
oldConsEntry.setSerialProducts(serials);
oldConsEntry.setItems(items);
oldConsEntry.setConsignmentEntryStatus(consignmentEntryStatus);
oldConsEntry.setQuantity(oldConsEntry.getQuantity()-1L);
modelService.save(oldConsEntry);
modelService.refresh(oldConsEntry);
      }
      // Release stock for old serial.
      releaseStock(productData);
  }

    /**
     * This method used to create or updae consignment and consignment entry.
     * @param orderEntry
     * @param newSerial
     */
  private void createOrUpdateConsignment(final AbstractOrderEntryModel orderEntry,final BlSerialProductModel newSerial){
      ConsignmentModel consignmentModel;
      BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Going to create or update consignment data for serial replacement for the order {}",orderModel.getCode());
      Optional<ConsignmentModel> consignment = orderModel.getConsignments().stream().filter(cons -> cons.getWarehouse().getCode().equals(newSerial.getWarehouseLocation().getCode())).findFirst();
      if (consignment.isPresent()){
          consignmentModel=consignment.get();
          ConsignmentEntryModel consignmentEntryModel;
          Optional<ConsignmentEntryModel> entryModelOption=consignmentModel.getConsignmentEntries().stream().filter(consEntry -> consEntry.getOrderEntry().getProduct().getCode().equals(newSerial.getBlProduct().getCode())).findFirst();
          if (entryModelOption.isPresent()){
              consignmentEntryModel=entryModelOption.get();
              //updating consignment entry if it already exist.
              addNewSerialToConsignmentEntryIfAlreadyExist(consignmentEntryModel,newSerial);
          }else {
              // creating and updating cons entry
              final SourcingResult sourcingResult=createSourceResult(orderEntry,newSerial);
              consignmentEntryModel = defaultBlAllocationService.createConsignmentEntry(orderEntry, 1L, consignmentModel, sourcingResult);
              Set<ConsignmentEntryModel> consignmentEntryModels = CollectionUtils.isNotEmpty(consignmentModel.getConsignmentEntries()) ?new HashSet<>( consignmentModel.getConsignmentEntries()) : new HashSet<ConsignmentEntryModel>();
              consignmentEntryModels.add(consignmentEntryModel);
              consignmentModel.setConsignmentEntries(consignmentEntryModels);
          }
          reservedStock(consignmentModel,newSerial);
          //need to reserve stock of new serial if not updated.
          modelService.save(consignmentEntryModel);
          modelService.refresh(consignmentEntryModel);
          modelService.save(consignmentModel);
          modelService.refresh(consignmentModel);
      }else {
          final SourcingResult sourcingResult=createSourceResult(orderEntry,newSerial);
          final Map<AbstractOrderEntryModel, Long> allocationMap = new HashedMap();
          allocationMap.put(orderEntry,1L);
          sourcingResult.setAllocation(allocationMap);
          consignmentModel=defaultBlAllocationService.createConsignment(orderModel, BlCoreConstants.CONSIGNMENT_PROCESS_PREFIX + orderModel.getCode()
                  + BlOrdermanagementConstants.UNDER_SCORE + orderModel.getConsignments().size(), sourcingResult);
      }
  }

    /**
     * Creating sourcing result which is used for creating consingment and cons entry.
     * @param orderEntry
     * @param newSerial
     * @return
     */
  private SourcingResult createSourceResult(AbstractOrderEntryModel orderEntry,final BlSerialProductModel newSerial){
      final SourcingResult sourcingResult = new SourcingResult();
      sourcingResult.setWarehouse(newSerial.getWarehouseLocation());

      Map<Integer , Set<BlSerialProductModel>> serialProductMap = new HashMap<>();
      Set<BlSerialProductModel> serialProductset = new HashSet<>();
      serialProductset.add(newSerial);
      serialProductMap.put(orderEntry.getEntryNumber(),serialProductset);
      sourcingResult.setSerialProductMap(serialProductMap);
      return sourcingResult;
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
    public DefaultBlESPEventService getBlEspEventService() {
        return blEspEventService;
    }

    public void setBlEspEventService(DefaultBlESPEventService blEspEventService) {
        this.blEspEventService = blEspEventService;
    }
}
