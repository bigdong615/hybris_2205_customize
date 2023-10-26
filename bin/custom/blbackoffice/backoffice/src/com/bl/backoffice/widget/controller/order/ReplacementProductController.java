package com.bl.backoffice.widget.controller.order;

import com.bl.Ordermanagement.reallocation.BlReallocationService;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.dao.impl.DefaultBlProductDao;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
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
    @WireVariable
    private transient BlReallocationService blReallocationService;

    @SocketEvent(socketId = IN_SOCKET)
    public void initPartialRefundForm(final OrderModel inputOrder) {
        this.setOrderModel(inputOrder);
        this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
                .getLabel(TITLE)).append(
                this.getOrderModel().getCode()).toString());
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
                        //replacementProductData.setNewSerial(StringUtils.EMPTY);
                        replacementProductDataList.add(replacementProductData);

                    }
                });
            });
        });
        /*orderModel.getEntries().forEach(entryModel -> {
            entryModel.getSerialProducts().forEach(blProductModel -> {
                if (blProductModel instanceof BlSerialProductModel){
                    BlSerialProductModel serialProductModel = (BlSerialProductModel)blProductModel;
                    ReplacementProductData replacementProductData= new ReplacementProductData();
                    replacementProductData.setProductName(serialProductModel.getBlProduct().getName());
                    replacementProductData.setAssignedSerial(serialProductModel.getCode());
                    replacementProductData.setOcLocation(serialProductModel.getOcLocation());
                    replacementProductData.setOldSerial(serialProductModel);
                    replacementProductData.setOrderEntry(entryModel);
                    //replacementProductData.setNewSerial(StringUtils.EMPTY);
                                  replacementProductDataList.add(replacementProductData);

                }
            });
        });*/

        this.serialEntries.setModel(new ListModelList<>(replacementProductDataList));
        this.serialEntries.renderAll();

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
           // Messagebox.show("Confirm you want to replace "+productData.getAssignedSerial()+" with "+productData.getNewSerial()+" on order [order number]");

           String  newserial =((Textbox) row.getChildren().get(4)).getValue();
          String   selectedValue =( (Combobox)row.getChildren().get(5)).getValue();
            replacementProductData.setNewSerial(newserial);
            replacementProductData.setSelectedReason(selectedValue);
            Messagebox.show("Confirm you want to replace "+replacementProductData.getAssignedSerial()+" with "+newserial+" on order "+orderModel.getCode()+" selectedValue :"+selectedValue, "Replacement Confirmation", new Messagebox.Button[]
                    {Messagebox.Button.YES, Messagebox.Button.NO},null, Messagebox.QUESTION, null, clickEvent -> {
                if (Messagebox.Button.YES == clickEvent.getButton())
                {
                    validateAndUpdate(replacementProductData,row);
                }else{
                    cancelAndLeave();
                }
            }, null);


        }else {
            Messagebox.show("Please enter atleast one Order");
        }


    }

    private   void  validateAndUpdate(final ReplacementProductData productData, Component row){
        final BlSerialProductModel serial = this.defaultBlProductDao.getSerialByBarcode(productData.getNewSerial());
        if (Objects.isNull(serial)) {
            Messagebox.show("No serial present with given barcode :" + productData.getNewSerial());
            return;
        }
        else if (Objects.isNull(serial.getWarehouseLocation())) {
            Messagebox.show("No warehouse associated with serial");
        }else{
            if(productData.getOldSerial().getBlProduct().equals(serial.getBlProduct())) {
                if (BooleanUtils.isTrue(serial.getWarehouseLocation().getCode().equals(productData.getConsignment().getWarehouse().getCode()))) {

                    replaceSerialWithSameProductAndWarehouse(productData, serial, productData.getNewSerial(), productData.getAssignedSerial(), row);

                } else {
                   // replaceSerialWithSameProductAndDifferentWarehouse()
   // same product from different warehouse
                }

            }else {
                //different product
            }
           // Messagebox.show("This barcode :"+productData.getNewSerial()+" belonds to different warehouse :"+serial.getCode());
        }
        Messagebox.show("from validate");
    }

    protected void replaceSerialWithSameProductAndWarehouse(final ReplacementProductData productData,final BlSerialProductModel newSerial, final String barCode, final String oldSerialCode,
                              final Component row) {

        ConsignmentEntryModel consEntry = productData.getConsEntry();
       // List<BlProductModel> serialProducts = consEntry.getSerialProducts();
      //  Map<String, ItemStatusEnum> items = consEntry.getItems();
        Map<String, ConsignmentEntryStatusEnum> consignmentEntryStatus = consEntry.getConsignmentEntryStatus();


        List<BlProductModel> serialProducts = consEntry.getSerialProducts();
        serialProducts = CollectionUtils.isNotEmpty(serialProducts)?new ArrayList<BlProductModel>(serialProducts) :new ArrayList<BlProductModel>();
        serialProducts.add(newSerial);
        serialProducts.remove(productData.getOldSerial());
        consEntry.setSerialProducts(serialProducts);

        Map<String, ItemStatusEnum> items = consEntry.getItems();
        items = (items == null || items.isEmpty()) ? new HashMap<>() : new HashMap<>(items);
        if(items.isEmpty()){
            items.put(newSerial.getCode(),ItemStatusEnum.NOT_INCLUDED);
        }else {
            items.put(newSerial.getCode(), items.get(productData.getOldSerial()));
            items.remove(productData.getOldSerial());
        }
        consEntry.setItems(items);

        List<BlProductModel> serialProductOnOrderEntry = consEntry.getOrderEntry()
                .getSerialProducts();
        serialProductOnOrderEntry = CollectionUtils.isNotEmpty(serialProductOnOrderEntry) ? new ArrayList<>(serialProductOnOrderEntry) : new ArrayList<>();
        serialProductOnOrderEntry.add(newSerial);
        serialProductOnOrderEntry.remove(productData.getOldSerial());
        consEntry.getOrderEntry().setSerialProducts(serialProductOnOrderEntry);

        modelService.save(consEntry.getOrderEntry());
        modelService.refresh(consEntry.getOrderEntry());
        modelService.save(consEntry);
        modelService.refresh(consEntry);

        blReallocationService.removeReserveStocksForSerialProducts(new HashSet<>(Arrays.asList(productData.getAssignedSerial())),
               productData.getConsignment().getOptimizedShippingStartDate(), productData.getConsignment().getOptimizedShippingEndDate(), Boolean.TRUE,
                productData.getConsignment().getWarehouse());
        // need to reserve stock for new serial.

        final List<String> serialsCodesToRemove = new ArrayList<String>();
        final List<String> serialsCodesToAdd = new ArrayList<String>();
        final List<BlProductModel> productEntries = new ArrayList<BlProductModel>();
        productEntries.addAll(productData.getConsEntry().getSerialProducts());
        final Map<String, ItemStatusEnum> newItems = new HashMap<String, ItemStatusEnum>();
        newItems.putAll(productData.getConsEntry().getItems());
        for (final BlProductModel productEntry : productData.getConsEntry().getSerialProducts()) {
            if (productEntry instanceof BlSerialProductModel && productEntry.getCode().equals(oldSerialCode)) {
                final BlSerialProductModel serialProduct = (BlSerialProductModel) productEntry;
                if (serial != null && serialProduct.getBlProduct().equals(serial.getBlProduct())) {
                    if (productData.getConsEntry().getItems().containsKey(serialProduct.getCode())) {
                        newItems.remove(serialProduct.getCode());
                        newItems.put(serial.getCode(), productData.getConsEntry().getItems().get(serialProduct.getCode()));
                    }
                    productEntries.remove(productEntry);
                    productEntries.add(serial);
                    serialsCodesToRemove.add(oldSerialCode);
                    serialsCodesToAdd.add(serial.getCode());


                        blReallocationService.removeReserveStocksForSerialProducts(new HashSet<>(serialsCodesToRemove),
                                orderModel.getConsignments().iterator().next().getOptimizedShippingStartDate(), orderModel.getConsignments().iterator().next().getOptimizedShippingEndDate(), Boolean.TRUE,
                                orderModel.getConsignments().iterator().next().getWarehouse());
                }
                else
                {
                    throw new WrongValueException((row.getChildren().get(4)),
                            this.getLabel("warehousingbackoffice.reassignserial.validation.incorrect.barcode"));
                }
            }
        }
        productData.getConsEntry().getOrderEntry().setSerialProducts(productEntries.stream()
                .filter(blSerialProduct -> blSerialProduct instanceof BlSerialProductModel).collect(Collectors.toList()));
        productData.getConsEntry().setSerialProducts(productEntries);
        productData.getConsEntry().setItems(newItems);
        modelService.save(productData.getConsEntry().getOrderEntry());
        modelService.save(productData.getConsEntry());
        updateConsignmentEntryQuantity(productData.getConsEntry());
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
