package com.bl.backoffice.widget.controller.order;

import com.bl.Ordermanagement.reallocation.BlReallocationService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.core.product.dao.impl.DefaultBlProductDao;
import com.bl.core.stock.BlStockService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.store.BaseStoreModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.event.Events;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.*;
import com.bl.backoffice.wizards.util.ReplacementProductData;

import java.util.*;
import java.util.stream.Collectors;
import org.zkoss.zk.ui.Component;

import javax.annotation.Resource;

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
    @WireVariable
    private transient BlReallocationService blReallocationService;


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
    /**
     * Refund order amount.
     */
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
if(isOldSerialAvailableOnOrder(actualOrder,productData.getOldSerial())) {
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
            //different product
        }
    }
}else {
    Messagebox.show("The original serial is not on this order, double check.");
}
        Messagebox.show("from validate");
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
        updateConsignmentEntryQuantity(productData.getConsEntry());
    }
private   boolean isOldSerialAvailableOnOrder(AbstractOrderModel order,BlSerialProductModel oldSerial){
   for (ConsignmentModel consignmentModel :orderModel.getConsignments()){
        for(ConsignmentEntryModel consignmentEntryModel :consignmentModel.getConsignmentEntries()) {
            for(BlProductModel blProductModel: consignmentEntryModel.getSerialProducts()){
                if (blProductModel.getCode().equals(oldSerial.getCode()) ) {
                    return true;
                }
            }
        }
    }
                    return Boolean.FALSE;
}
    private void updatingBothEntry(final ConsignmentEntryModel consEntry,final BlSerialProductModel newSerial,final BlSerialProductModel oldSerial){
        List<BlProductModel> serialProducts = consEntry.getSerialProducts();
        serialProducts = CollectionUtils.isNotEmpty(serialProducts)?new ArrayList<BlProductModel>(serialProducts) :new ArrayList<BlProductModel>();
        serialProducts.add(newSerial);
        serialProducts.remove(oldSerial);
        consEntry.setSerialProducts(serialProducts);

        Map<String, ItemStatusEnum> items = consEntry.getItems();
        items = (items == null || items.isEmpty()) ? new HashMap<>() : new HashMap<>(items);
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

    private void cancelAndLeave(){
        Messagebox.show("from cancle");
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
}
