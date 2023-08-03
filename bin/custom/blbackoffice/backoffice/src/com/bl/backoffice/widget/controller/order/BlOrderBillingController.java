package com.bl.backoffice.widget.controller.order;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.BillInfoStatus;
import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.facades.order.BlOrderFacade;
import com.bl.logging.BlLogger;
import com.bl.tax.populators.BlTaxServiceRequestPopulator;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.google.common.util.concurrent.AtomicDouble;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.*;
import org.zkoss.zul.impl.InputElement;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

public class BlOrderBillingController extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(BlOrderBillingController.class);
    private static final String IN_SOCKET = "inputObject";
    private OrderModel orderModel;
    @Wire
    private Combobox missingItemToolCombobox;

    @Wire
    private Checkbox globalBillEntriesSelection;

    @Wire
    private Doublebox totalAmountDueDouble;

    @Wire
    private Button createBill;

    @Wire
    private Button deleteBill;

    @Wire
    private Button capturePayment;

    @Wire
    private Button sendInvoice;

    @Wire
    private Radio billPaidTrue;

    @Wire
    private Radio billPaidFalse;

    @Wire
    private Checkbox globalProcessingFeeChkbox;

    @Wire
    private Textbox selectedProduct;

    @Wire
    private Textbox selectedProcessingFee;


    private ListModelList billingChargesReason = new ListModelList();

    @WireVariable
    private transient EnumerationService enumerationService;
    @WireVariable
    private transient BackofficeLocaleService cockpitLocaleService;
    @Wire
    private Grid orderEntries;

    @WireVariable
    private transient ModelService modelService;

    private static final String MISSING_ITEM_TOOL = "customersupportbackoffice.billing.widget.title";
    private transient List<BlOrderBillingItemDTO> orderEntriesForBilling;

    private final Set<BlProductModel> allSerialProducts = new HashSet<>();

    private String totalAmountDue;

    private transient BlOrderFacade blOrderFacade;
    private transient OrderFacade orderFacade;

    private transient BrainTreeCheckoutFacade brainTreeCheckoutFacade;
    private transient PriceDataFactory priceDataFactory;
    private transient DefaultBlESPEventService blEspEventService;
    private static final int DECIMAL_PRECISION = 2;
    private transient BrainTreeTransactionService brainTreeTransactionService;


    @SocketEvent(socketId = IN_SOCKET)
    public void initOrderBillingForm(final OrderModel inputOrder) {
        this.setOrderModel(inputOrder);
        this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
                .getLabel(MISSING_ITEM_TOOL)).append(
                this.getOrderModel().getCode()).toString());
        getMissingItemReasons();
        disableOrEnableFields(Boolean.TRUE);

    }

    private void getMissingItemReasons() {
        this.getEnumerationService().getEnumerationValues(ItemBillingChargeTypeEnum.class).forEach(reason ->
                this.billingChargesReason.add(this.getEnumerationService().getEnumerationName(reason, this.getLocale())));

        this.getEnumerationService().getEnumerationValues(ItemBillingChargeTypeEnum.class);
        this.missingItemToolCombobox.setModel(new ListModelArray<>(this.billingChargesReason));
    }

    /**
     * This method is called when we will change the value of missingItemToolCombobox
     */
    @ViewEvent(componentID = "globalBillEntriesSelection", eventName = BlInventoryScanLoggingConstants.ON_CHECK)
    public void checkGlobalBillEntriesSelection() {
        applyToGridBillEntriesSelection();
        calculateLineItemTotalAmountDue();
    }

    /**
     * This method is called when we will change the value of missingItemToolCombobox
     */
    @ViewEvent(componentID = "globalProcessingFeeChkbox", eventName = BlInventoryScanLoggingConstants.ON_CHECK)
    public void checkGlobalProcessingFeeSelection() {
        applyToGridProcessingFeeSelection();
        calculateLineItemTotalAmountDue();
    }

    /**
     * This method is called when we will change the value of missingItemToolCombobox
     */
    @ViewEvent(componentID = "missingItemToolCombobox", eventName = BlInventoryScanLoggingConstants.ON_CHANGE_EVENT)
    public void changedMissingItemToolCombobox()
    {
        getModelService().refresh(orderModel);
        this.orderEntriesForBilling = new ArrayList<>();
        this.billingChargesReason.addToSelection(this.missingItemToolCombobox.getValue());
        initializePopupRequiredFields(getOrderModel());
        disableOrEnableFields(Boolean.FALSE);
        this.totalAmountDueDouble.setValue(0.0d);
        this.globalProcessingFeeChkbox.setChecked(Boolean.FALSE);
        this.globalBillEntriesSelection.setChecked(Boolean.FALSE);
        this.getOrderEntries().setModel(new ListModelList<>(this.orderEntriesForBilling));
        this.getOrderEntries().renderAll();

    }

    private void calculateLineItemTotalAmountDue() {
        final AtomicDouble lineItemTotals = new AtomicDouble(0.0d);
        for (final Component row : this.getOrderEntriesGridRows()) {
            if (((Checkbox) row.getChildren().iterator().next()).isChecked()) {
                final String amountDue =  ((Textbox)row.getChildren().get(5)).getValue();
                final String processingFee = ((Textbox)row.getChildren().get(7)).getValue();
                final String tax = ((Textbox)row.getChildren().get(8)).getValue();
                if (((Checkbox) row.getChildren().get(6)).isChecked()) {
                    lineItemTotals.addAndGet(Double.parseDouble(amountDue) + Double.parseDouble(processingFee) + Double.parseDouble(tax));
                }
                else {
                    lineItemTotals.addAndGet(Double.parseDouble(amountDue) + Double.parseDouble(tax));
                }
            }
        }

        this.totalAmountDueDouble.setValue(lineItemTotals.doubleValue());
    }



    private void initializePopupRequiredFields(OrderModel inputOrder) {
        this.orderEntriesForBilling = new ArrayList<>();

        inputOrder.getEntries().forEach(abstractOrderEntryModel -> {
            abstractOrderEntryModel.getSerialProducts().forEach(serialProduct -> {
                BlOrderBillingItemDTO itemDTO = new BlOrderBillingItemDTO();
                itemDTO.setProductName(abstractOrderEntryModel.getProduct().getName());
                itemDTO.setSerialNo(serialProduct.getCode());
                itemDTO.setAmount(((BlProductModel) abstractOrderEntryModel.getProduct()).getRetailPrice() !=null ? ((BlProductModel) abstractOrderEntryModel.getProduct()).getRetailPrice() : 0.0);
                setDamageWaiver(itemDTO,abstractOrderEntryModel);
                setSubTotal(itemDTO);
                setProcessingFee(itemDTO);
                itemDTO.setTax(22.5);
                setUnpaidBillNotes(itemDTO,serialProduct);
                setSerialProductCodes(serialProduct);
                this.orderEntriesForBilling.add(itemDTO);

            });
        });

    }
    @ViewEvent(componentID = "selectedProduct", eventName = "onChange")
    public void billSelectedProduct()
    {

        final String[] selctedCheckBoxArray = this.selectedProduct.getValue().split(BlInventoryScanLoggingConstants.ORDER_BILL_SPLIT_STRING);
        if(this.selectedProduct != null && this.selectedProduct.getValue().contains(BlInventoryScanLoggingConstants.ORDER_BILL_SPLIT_STRING))
        {
            for (final Component row : this.getOrderEntriesGridRows()) {
                if (((Textbox) row.getChildren().get(2)).getValue().equals(selctedCheckBoxArray[0])) {
                    Double amount=Double.parseDouble(((Textbox) row.getChildren().get(3)).getValue());
                    if(selctedCheckBoxArray[1].contains("False")) {
                        ((Textbox) row.getChildren().get(5)).setValue(amount.toString());
                    }
                    else {
                        amount=amount * 12/100;
                        ((Textbox) row.getChildren().get(5)).setValue(amount.toString());
                    }
                }
            }

        }
        calculateLineItemTotalAmountDue();

    }

    private void setDamageWaiver(BlOrderBillingItemDTO itemDTO, AbstractOrderEntryModel abstractOrderEntryModel)
    {
        String selectedBillingChargesReason= missingItemToolCombobox.getValue();
        if(selectedBillingChargesReason!=null || !selectedBillingChargesReason.isEmpty())
        {
            if(selectedBillingChargesReason.equals("MISSING CHARGE"))
            {
                if(abstractOrderEntryModel.getGearGuardProFullWaiverSelected() ==Boolean.TRUE)
                {
                    itemDTO.setDamageWaiver(Boolean.TRUE);
                }

            }
            else if(selectedBillingChargesReason.equals("REPAIR CHARGE")
                    || selectedBillingChargesReason.equals("CUSTOMER RESPONSIBLE REPAIRS"))
            {
                if(abstractOrderEntryModel.getGearGuardWaiverSelected()== Boolean.TRUE)
                {
                    itemDTO.setDamageWaiver(Boolean.TRUE);
                }

            }
            else if(abstractOrderEntryModel.getNoDamageWaiverSelected() == Boolean.TRUE)
            {
                itemDTO.setDamageWaiver(Boolean.FALSE);
            }
            else
            {
                itemDTO.setDamageWaiver(Boolean.TRUE);
            }
        }
    }

    private void setSubTotal(BlOrderBillingItemDTO itemDTO)
    {
        if(itemDTO.isDamageWaiver()== Boolean.TRUE)
        {
            itemDTO.setSubtotal(itemDTO.getAmount() * 12/100);
        }
        else
        {
            itemDTO.setSubtotal(itemDTO.getAmount());
        }
    }

    private void setProcessingFee(BlOrderBillingItemDTO itemDTO)
    {
        itemDTO.setProcessingFee(Boolean.TRUE);
    }

    /**
     * Apply to grid.
     *
     *
     */
    private void applyToGridBillEntriesSelection() {
        for (final Component row : this.getOrderEntriesGridRows()) {
            if (this.globalBillEntriesSelection.isChecked() == Boolean.TRUE) {
                ((Checkbox) row.getChildren().iterator().next()).setChecked(Boolean.TRUE);
            } else {
                ((Checkbox) row.getChildren().iterator().next()).setChecked(Boolean.FALSE);
            }
        }
    }

    private void applyToGridProcessingFeeSelection()
    {
        for (final Component row : this.getOrderEntriesGridRows()) {
            if(this.globalProcessingFeeChkbox.isChecked() == Boolean.TRUE) {
                ((Checkbox) row.getChildren().get(6)).setChecked(Boolean.TRUE);
            }
            else {
                ((Checkbox) row.getChildren().get(6)).setChecked(Boolean.FALSE);
            }
        }
    }

    /**
     * Gets order entries grid rows.
     *
     * @return the order entries grid rows
     */
    private List<Component> getOrderEntriesGridRows()
    {
        return this.getOrderEntries().getRows().getChildren();
    }

    private void setUnpaidBillNotes(BlOrderBillingItemDTO itemDTO,BlProductModel serialProduct)
    {
        itemDTO.setUnpaidBillNotes(missingItemToolCombobox.getValue() + " "+ itemDTO.getProductName() + " - " + "Serial# " + serialProduct.getCode());
    }

    private void disableOrEnableFields(Boolean b)
    {
        this.createBill.setDisabled(b);
        this.deleteBill.setDisabled(b);
        this.capturePayment.setDisabled(b);
        this.sendInvoice.setDisabled(b);
        this.billPaidFalse.setDisabled(b);
        this.billPaidTrue.setDisabled(b);
        this.globalProcessingFeeChkbox.setDisabled(b);
        this.globalBillEntriesSelection.setDisabled(b);
    }

    private void setSerialProductCodes(BlProductModel serialProduct)
    {
        this.allSerialProducts.add(serialProduct);
    }

    @ViewEvent(componentID = "createBill", eventName = BlCustomCancelRefundConstants.ON_CLICK)
    public void createBill()
    {
        BlItemsBillingChargeModel billingChargeModel = getModelService().create(BlItemsBillingChargeModel.class);
        try {
            List<Component> checkedEntries = this.getOrderEntriesGridRows().stream().filter(entryRow -> ((Checkbox) entryRow.getFirstChild()).isChecked()).collect(Collectors.toList());
            BigDecimal totalAmount = BigDecimal.valueOf(0.0);
            if(!CollectionUtils.isEmpty(checkedEntries)){
                final String randomId = UUID.randomUUID().toString();
                billingChargeModel.setCode(randomId);
                billingChargeModel.setUpdatedBillTime(new Date());
                billingChargeModel.setBillStatus(BillInfoStatus.NEW_BILL);
                billingChargeModel.setOrder(this.getOrderModel());
                billingChargeModel.setBillPaid(Boolean.FALSE);
                List<ItemBillingChargeTypeEnum> enums = this.getEnumerationService().getEnumerationValues(ItemBillingChargeTypeEnum.class);
                billingChargeModel.setBillChargeType(enums.get(0));
                billingChargeModel.setOrderCode(this.getOrderModel().getCode());

                BigDecimal taxAmount = BigDecimal.valueOf(0.0);
                for(Component input : checkedEntries){
                    taxAmount = taxAmount.add(new BigDecimal(((InputElement) input.getChildren().get(8)).getText()));
                }
                billingChargeModel.setTaxAmount(taxAmount);

                List<String> serialCodes = new ArrayList<>();
                for(Component codes : checkedEntries){
                    serialCodes.add(((InputElement) codes.getChildren().get(2)).getText());
                }
                billingChargeModel.setSerialCodes(serialCodes);

                BigDecimal chargedAmount = BigDecimal.valueOf(0.0);
                for(Component amount : checkedEntries){
                    chargedAmount = chargedAmount.add((new BigDecimal(((InputElement) amount.getChildren().get(5)).getText())));
                }

                BigDecimal processingFee = BigDecimal.valueOf(0.0);
                for(Component fee : checkedEntries){
                    processingFee = processingFee.add(new BigDecimal(((InputElement) fee.getChildren().get(7)).getText()));
                }

                List<String> unPaidBillNotes = new ArrayList<>();
                for(Component billNotes : checkedEntries){
                    unPaidBillNotes.add((((InputElement) billNotes.getChildren().get(9)).getText()));
                }
                billingChargeModel.setUnPaidBillingNotes(unPaidBillNotes);

                totalAmount = totalAmount.add(chargedAmount).add(taxAmount).add(processingFee);
                this.totalAmountDue = totalAmount.toString();
                billingChargeModel.setChargedAmount(totalAmount);
                getModelService().save(billingChargeModel);
                getModelService().refresh(this.getOrderModel());
                final CustomerModel customerModel = (CustomerModel) this.getOrderModel().getUser();
                List<BlItemsBillingChargeModel> billModels = new ArrayList<>();
                if(!CollectionUtils.isEmpty(customerModel.getOutstandingBills())){
                    billModels.addAll(customerModel.getOutstandingBills());
                    billModels.add(billingChargeModel);
                }
                else {
                    billModels = Arrays.asList(billingChargeModel);
                }
                customerModel.setOutstandingBills(billModels);
                getModelService().save(customerModel);
                Messagebox.show("Successfully created the bill for order " + "" + this.getOrderModel().getCode()  , "SUCCESS", Messagebox.OK, Messagebox.INFORMATION);

            }
            else{
                Messagebox.show("Please mark the checkboxes to create the bills.", "ERROR", Messagebox.OK, Messagebox.ERROR);
            }

        }
        catch (ModelSavingException exception) {
            BlLogger.logMessage(LOG , Level.ERROR , "Error while saving the Billing model for order {} due to {} " , this.getOrderModel().getCode(),exception);
        }

    }

    @ViewEvent(componentID = "deleteBill", eventName = "onClick" )
    public void deleteBill()
    {
        List<BlItemsBillingChargeModel> allBills = this.getOrderModel().getOrderBills();
        if(!CollectionUtils.isEmpty(allBills)) {
            try {
                getModelService().removeAll(allBills);
                getModelService().refresh(this.getOrderModel());
                Messagebox.show("All the bills associated with the order are deleted now.", "SUCCESS", Messagebox.OK, Messagebox.INFORMATION);
            } catch (Exception ex) {
                BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Unable to delete the Bills for the order : {} due to {}" ,
                        this.getOrderModel().getCode(), ex);
            }
        }
        else {
            Messagebox.show("No bills to Delete.", "ERROR", Messagebox.OK, Messagebox.ERROR);
        }

    }

    @ViewEvent(componentID = "capturePayment", eventName = "onClick")
    public void captureBillPayment(){

        String orderCode = this.getOrderModel().getCode();
        String paymentInfoId = String.valueOf(this.getOrderModel().getPaymentInfo().getOriginal().getPk());
        String paymentMethodNonce = ((BrainTreePaymentInfoModel) this.getOrderModel().getPaymentInfo()).getNonce();
        String billPayTotal = this.totalAmountDue;
        String poNumber = "";
        String poNotes = "";

        boolean isSuccess = false;
        double payBillAmount = Double.parseDouble(billPayTotal);
        AbstractOrderModel order = null;
        if ((StringUtils.isNotBlank(orderCode) && StringUtils.isNotBlank(paymentInfoId)) || StringUtils.isNotBlank(poNumber) ) {
            order = this.getOrderModel();

            isSuccess = payBillSuccess(paymentInfoId, paymentMethodNonce, payBillAmount, poNumber,
                    poNotes, order);

            if (isSuccess) {
                try {
                    blOrderFacade.setResolvedStatusOnRepairLog(orderCode);
                    final Map<String, List<String>> billingChargeTypeMap = brainTreeCheckoutFacade.setOrderPayBillFlagTrue(order);
                    blEspEventService.triggerBillPaidEspEvent(billPayTotal, billingChargeTypeMap, (OrderModel) order);
                    Messagebox.show("Payment capture done.", "SUCCESS", Messagebox.OK, Messagebox.INFORMATION);
                }
                catch (final Exception e) {
                    BlLogger.logMessage(LOG , Level.ERROR , "Error while executing getPayBillDetailsForOrder " , e);
                }
            }
            else {
                Messagebox.show("Payment capture button clicked.", "ERROR", Messagebox.OK, Messagebox.ERROR);

            }
                Messagebox.show("Payment capture button clicked.", "ERROR", Messagebox.OK, Messagebox.ERROR);

        }
    }

    private boolean payBillSuccess(String paymentInfoId, String paymentMethodNonce, double billPayTotal, String poNumber, String poNotes, AbstractOrderModel order) {
        {
            boolean isSuccess = false;
            if (null != order && StringUtils.isNotBlank(poNumber)) {
                isSuccess = blOrderFacade.savePoPaymentForPayBillOrder(poNumber, poNotes, order.getCode());
                if (BooleanUtils.isTrue(isSuccess)) {

                }
            } else if(null != order) {
                // It creates a cloned payment info from the original payment info
                final BrainTreePaymentInfoModel paymentInfo = brainTreeCheckoutFacade
                        .getClonedBrainTreePaymentInfoCode(
                                (CustomerModel) order.getUser(), paymentInfoId, paymentMethodNonce);
                if (null != paymentInfo) {
                    paymentInfo.setExtendOrder(Boolean.FALSE);
                    paymentInfo.setModifyPayment(Boolean.FALSE);
                    paymentInfo.setBillPayment(Boolean.TRUE);
                    paymentInfo.setCreateNewTransaction(Boolean.TRUE);
                    modelService.save(paymentInfo);
                    isSuccess = brainTreeTransactionService
                            .createAuthorizationTransactionOfOrder(order,
                                    BigDecimal.valueOf(billPayTotal).setScale(DECIMAL_PRECISION, RoundingMode.HALF_EVEN), true, paymentInfo);
                }
                if (BooleanUtils.isTrue(isSuccess)) {

                }
            }
            return isSuccess;
        }
    }

    @ViewEvent(componentID = "sendInvoice", eventName = "onClick")
    public void sendBillingInvoice(){
        Messagebox.show("Send Invoice button clicked.", "SUCCESS", Messagebox.OK, Messagebox.INFORMATION);
    }



    public OrderModel getOrderModel() {
        return orderModel;
    }

    public void setOrderModel(final OrderModel orderModel) {
        this.orderModel = orderModel;
    }

    public EnumerationService getEnumerationService() {
        return enumerationService;
    }

    public void setEnumerationService(EnumerationService enumerationService) {
        this.enumerationService = enumerationService;
    }

    public Grid getOrderEntries() {
        return orderEntries;
    }

    public void setOrderEntries(Grid orderEntries) {
        this.orderEntries = orderEntries;
    }

    private Locale getLocale() {
        return this.getCockpitLocaleService().getCurrentLocale();
    }

    private BackofficeLocaleService getCockpitLocaleService() {
        return this.cockpitLocaleService;
    }


    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }


}
