package com.bl.backoffice.widget.controller.order;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.constants.BlloggingConstants;
import com.bl.core.enums.BillInfoStatus;
import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.facades.order.BlOrderFacade;
import com.bl.logging.BlLogger;
import com.bl.tax.billing.BillingPojo;
import com.bl.tax.populators.BlTaxServiceRequestPopulator;
import com.bl.tax.service.impl.DefaultBlAvalaraTaxService;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.google.common.util.concurrent.AtomicDouble;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import com.hybris.cockpitng.util.notifications.NotificationService;
import com.hybris.cockpitng.util.notifications.event.NotificationEvent;
import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.payment.model.PaymentTransactionModel;
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
import com.hybris.cockpitng.util.notifications.NotificationService;

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

    private transient BlOrderFacade blOrderFacade;

    private transient BrainTreeCheckoutFacade brainTreeCheckoutFacade;
    private transient DefaultBlESPEventService blEspEventService;

    private transient DefaultBlAvalaraTaxService defaultBlAvalaraTaxService;
    private static final int DECIMAL_PRECISION = 2;
    private transient BrainTreeTransactionService brainTreeTransactionService;
    @Resource
    private transient NotificationService notificationService;
    private static final String BILL_CREATED = "blbackoffice.order.billing.tool.wizard.notification.bill.created";
    private static final String BILL_CREATION_ERROR = "blbackoffice.order.billing.tool.wizard.notification.bill.creation.error";
    private static final String BILL_DELETED = "blbackoffice.order.billing.tool.wizard.notification.bill.deleted";
    private static final String NO_BILL_TO_DELETE = "blbackoffice.order.billing.tool.wizard.notification.no.bill.delete";
    private static final String CAPTURE_SUCCESS = "blbackoffice.order.billing.tool.wizard.notification.bill.capture.success";
    private static final String CAPTURE_ERROR = "blbackoffice.order.billing.tool.wizard.notification.bill.capture.error";
    private static final String CAPTURE_AMOUNT_ERROR = "blbackoffice.order.billing.tool.wizard.notification.bill.capture.amount.error";
    private static final String BILL_INVOICE_SUCCESS = "blbackoffice.order.billing.tool.wizard.notification.bill.invoice.success";
    private static final String BILL_INVOICE_FAILURE = "blbackoffice.order.billing.tool.wizard.notification.bill.invoice.failure";
    private static final String BILL_INVOICE_EXCEPTION = "blbackoffice.order.billing.tool.wizard.notification.bill.invoice.exception";


    @SocketEvent(socketId = IN_SOCKET)
    public void initOrderBillingForm(final OrderModel inputOrder) {
        this.setOrderModel(inputOrder);
        this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
                .getLabel(MISSING_ITEM_TOOL)).append(" - ").append(
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
        setTax();
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
        checkIfBillIsPaid();
        applyToGridBillEntriesSelection();

    }

    private void checkIfBillIsPaid() {
        if(!this.getOrderModel().getOrderBills().isEmpty()) {
            Boolean allBillsPaid = this.getOrderModel().getOrderBills().stream().allMatch(bill -> bill.isBillPaid());
            this.billPaidTrue.setChecked(allBillsPaid);
            this.billPaidFalse.setChecked(!allBillsPaid);
        }
        else {
            this.billPaidTrue.setChecked(Boolean.FALSE);
            this.billPaidFalse.setChecked(Boolean.TRUE);
        }
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
        setTax();
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
     * This method will calculate tax and wil get displayed on tax field.
     */
    private void setTax() {
        if(billPaidTrue.isSelected()==Boolean.FALSE) {
            try {
                BillingPojo billing = new BillingPojo();
                for (final Component row : this.getOrderEntriesGridRows()) {
                    if (((Checkbox) row.getChildren().iterator().next()).isChecked()) {
                        final String amountDue = ((Textbox) row.getChildren().get(5)).getValue();
                        final String serialNo = ((Textbox) row.getChildren().get(2)).getValue();
                        final String productName = ((Textbox) row.getChildren().get(1)).getValue();
                        billing.setOrder(this.getOrderModel());
                        billing.setBillPaid(billPaidTrue.isSelected() ? Boolean.TRUE : Boolean.FALSE);
                        billing.setAmount(Double.parseDouble(amountDue));
                        billing.setBillingChargesReason(getBillingChargesReason());
                        billing.setSerialNo(serialNo);
                        billing.setProductName(productName);
                        Double billingTax = getDefaultBlAvalaraTaxService().processBillingTax(billing);
                        ((Textbox) row.getChildren().get(8)).setValue(billingTax.toString());
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }



    /**
     * Apply to grid.
     *
     *
     */
    private void applyToGridBillEntriesSelection() {
        if (this.getOrderModel().getStatus().equals(OrderStatus.COMPLETED)) {
            this.globalBillEntriesSelection.setDisabled(Boolean.TRUE);
            this.globalProcessingFeeChkbox.setDisabled(Boolean.TRUE);
            this.getOrderEntriesGridRows().stream().forEach(gridRow -> ((Checkbox) gridRow.getChildren().iterator().next()).setDisabled(Boolean.TRUE));
        } else {
            for (final Component row : this.getOrderEntriesGridRows()) {
                if (this.globalBillEntriesSelection.isChecked() == Boolean.TRUE) {
                    ((Checkbox) row.getChildren().iterator().next()).setChecked(Boolean.TRUE);
                } else {
                    ((Checkbox) row.getChildren().iterator().next()).setChecked(Boolean.FALSE);
                }
            }
        }
    }

    private void applyToGridProcessingFeeSelection()
    {
        if (this.getOrderModel().getStatus().equals(OrderStatus.COMPLETED)) {
            this.getOrderEntriesGridRows().stream().forEach(gridRow -> ((Checkbox) gridRow.getChildren().iterator().next()).setDisabled(Boolean.TRUE));
        } else {
            for (final Component row : this.getOrderEntriesGridRows()) {
                if (this.globalProcessingFeeChkbox.isChecked() == Boolean.TRUE) {
                    ((Checkbox) row.getChildren().get(6)).setChecked(Boolean.TRUE);
                } else {
                    ((Checkbox) row.getChildren().get(6)).setChecked(Boolean.FALSE);
                }
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
        if(this.getOrderModel().getStatus().equals(OrderStatus.COMPLETED)){
            this.createBill.setDisabled(Boolean.TRUE);
            this.deleteBill.setDisabled(Boolean.TRUE);
            this.capturePayment.setDisabled(Boolean.TRUE);
        }
        else {
            this.createBill.setDisabled(b);
            this.deleteBill.setDisabled(b);
            this.capturePayment.setDisabled(b);
        }
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
                billingChargeModel.setBillChargeType(getBillingChargesReason());
                billingChargeModel.setOrderCode(this.getOrderModel().getCode());

                BigDecimal taxAmount =  sumValues(checkedEntries, 8);
                billingChargeModel.setTaxAmount(taxAmount);

                List<String> serialCodes = createStringList(checkedEntries, 2);
                billingChargeModel.setSerialCodes(serialCodes);
                updateSerialCodes(serialCodes,checkedEntries);

                BigDecimal chargedAmount = sumValues(checkedEntries, 5);
                BigDecimal processingFee = sumValues(checkedEntries, 7);

                List<String> unPaidBillNotes = createStringList(checkedEntries, 9);new ArrayList<>();
                billingChargeModel.setUnPaidBillingNotes(unPaidBillNotes);

                totalAmount = totalAmount.add(chargedAmount).add(taxAmount).add(processingFee);


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
                notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                        NotificationEvent.Level.INFO, this.getLabel(BILL_CREATED));

            }
            else{
                notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                        NotificationEvent.Level.FAILURE, this.getLabel(BILL_CREATION_ERROR));
            }

        }
        catch (ModelSavingException exception) {
            BlLogger.logMessage(LOG , Level.ERROR , "Error while saving the Billing model for order {} due to {} " , this.getOrderModel().getCode(),exception);
        }

    }

    private void updateSerialCodes(List<String> serialCodes, List<Component> checkedEntries) {

        List<String> updateSerials = new ArrayList<>();
        for (String code : serialCodes) {
            String mainProduct = "";
            String subTotal = "";

            for (Component comp : checkedEntries) {
                InputElement codeElement = (InputElement) comp.getChildren().get(2);

                if (code.equals(codeElement.getText())) {
                    mainProduct = ((InputElement) comp.getChildren().get(1)).getText();
                    subTotal = ((InputElement) comp.getChildren().get(5)).getText();
                    break; // No need to continue searching once found
                }
            }
            if(StringUtils.isNotBlank(mainProduct) && StringUtils.isNotBlank(subTotal)){
                String updatedCode = code + "," + mainProduct + "," + subTotal;
                updateSerials.add(updatedCode);
            }
        }
        serialCodes.clear();
        serialCodes.addAll(updateSerials);

    }

    private ItemBillingChargeTypeEnum getBillingChargesReason()
    {
        String selectedBillingChargesReason = missingItemToolCombobox.getValue();
        for(ItemBillingChargeTypeEnum itemBillingChargeTypeEnum : this.getEnumerationService().getEnumerationValues(ItemBillingChargeTypeEnum.class))
        {
            if(selectedBillingChargesReason.equals(this.getEnumerationService().getEnumerationName(itemBillingChargeTypeEnum, this.getLocale())))
            {
                return itemBillingChargeTypeEnum;
            }
        }
       return ItemBillingChargeTypeEnum.valueOf(selectedBillingChargesReason);
    }
    private BigDecimal sumValues(List<Component> components, int index) {
        BigDecimal sum = BigDecimal.ZERO;
        for(Component component : components){
            sum = sum.add(new BigDecimal(getText(component, index)));
        }
        return sum;
    }

    private List<String> createStringList(List<Component> components, int index) {
        List<String> list = new ArrayList<>();
        for(Component component : components){
            list.add(getText(component, index));
        }
        return list;
    }

    private String getText(Component component, int index) {
        return ((InputElement) component.getChildren().get(index)).getText();
    }

    @ViewEvent(componentID = "deleteBill", eventName = "onClick" )
    public void deleteBill()
    {
        List<BlItemsBillingChargeModel> allBills = this.getOrderModel().getOrderBills();
        if(!CollectionUtils.isEmpty(allBills)) {
            try {
                getModelService().removeAll(allBills);
                getModelService().refresh(this.getOrderModel());
                notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                        NotificationEvent.Level.INFO, this.getLabel(BILL_DELETED));
            } catch (Exception ex) {
                BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Unable to delete the Bills for the order : {} due to {}" ,
                        this.getOrderModel().getCode(), ex);
            }
        }
        else {
            notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                    NotificationEvent.Level.INFO, this.getLabel(NO_BILL_TO_DELETE));
        }

    }

    @ViewEvent(componentID = "capturePayment", eventName = "onClick")
    public void captureBillPayment(){

        final BigDecimal totalBillPay = calculateTotalBillPay();
        if (totalBillPay.compareTo(BigDecimal.ZERO) <= 0) {
            notifyAmountError();
            return;
        }
        String orderCode = this.getOrderModel().getCode();
        AbstractOrderModel order = this.getOrderModel();
        String poNumber = "";
        String poNotes = "";

        CustomerModel customerModel = (CustomerModel) order.getUser();
        String paymentInfoId = "";
        String paymentMethodNonce = "";

            if(customerModel.getDefaultPaymentInfo() != null){
                paymentInfoId = String.valueOf(customerModel.getDefaultPaymentInfo().getPk());
                paymentMethodNonce = ((BrainTreePaymentInfoModel) customerModel.getDefaultPaymentInfo()).getNonce();
            }
            else if (!order.getPaymentTransactions().isEmpty()) {
                PaymentTransactionModel paymentTransaction = order.getPaymentTransactions().get(0);
                paymentInfoId = String.valueOf(paymentTransaction.getInfo().getPk());
                paymentMethodNonce = ((BrainTreePaymentInfoModel) paymentTransaction.getInfo()).getNonce();
            }

            final BigDecimal payBillAmount = totalBillPay;


            boolean isSuccess = false;
            if ((!orderCode.isEmpty() && !paymentInfoId.isEmpty()) || !poNumber.isEmpty()) {
                isSuccess = capturePayment(paymentInfoId, paymentMethodNonce, payBillAmount, poNumber,
                        poNotes, order);

                if (isSuccess) {
                    handleSuccessfulPayment(orderCode, totalBillPay);
                } else {
                    handleFailedPayment();

                }
            }
    }
    private void handleSuccessfulPayment(String orderCode, BigDecimal totalBillPay) {
        try {
            blOrderFacade.setResolvedStatusOnRepairLog(orderCode);
            Map<String, List<String>> billingChargeTypeMap = brainTreeCheckoutFacade.setOrderPayBillFlagTrue(getOrderModel());
            blEspEventService.triggerBillPaidEspEvent(totalBillPay.toString(), billingChargeTypeMap, (OrderModel) getOrderModel());
            getModelService().refresh(this.getOrderModel());
            checkIfBillIsPaid();
            updateSerialStatusForPaidBills(getOrderModel());
            notifySuccess(CAPTURE_SUCCESS);
        } catch (Exception e) {
            BlLogger.logMessage(LOG, Level.ERROR, "Error while executing getPayBillDetailsForOrder ", e);
        }
    }

    private void handleFailedPayment() {
        notifyError(CAPTURE_ERROR);
    }

    private void notifySuccess(String messageKey) {
        notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                NotificationEvent.Level.INFO, getLabel(messageKey));
    }

    private void notifyError(String messageKey) {
        notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                NotificationEvent.Level.INFO, getLabel(messageKey));
    }

    private void notifyAmountError() {
        notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                NotificationEvent.Level.INFO, getLabel(CAPTURE_AMOUNT_ERROR));
    }

    private BigDecimal calculateTotalBillPay() {
        BigDecimal totalBillpay = BigDecimal.ZERO;
        for (BlItemsBillingChargeModel bill : this.getOrderModel().getOrderBills()) {
            if(BooleanUtils.isFalse(bill.isBillPaid())) {
                totalBillpay = totalBillpay.add(bill.getChargedAmount());
            }
        }
        return totalBillpay;
    }

    private boolean capturePayment(final String paymentInfoId, final String paymentMethodNonce, final BigDecimal billPayTotal, final String poNumber, final String poNotes, final AbstractOrderModel order) {

        boolean isSuccess = false;
        if (Objects.nonNull(order) && !poNumber.isEmpty()) {
            isSuccess = blOrderFacade.savePoPaymentForPayBillOrder(poNumber, poNotes, order.getCode());

        } else if(Objects.nonNull(order)) {
            // It creates a cloned payment info from the original payment info
            final BrainTreePaymentInfoModel paymentInfo = brainTreeCheckoutFacade
                    .getClonedBrainTreePaymentInfoCode(
                            (CustomerModel) order.getUser(), paymentInfoId, paymentMethodNonce);
            if (Objects.nonNull(paymentInfo)) {
                paymentInfo.setExtendOrder(Boolean.FALSE);
                paymentInfo.setModifyPayment(Boolean.FALSE);
                paymentInfo.setBillPayment(Boolean.TRUE);
                paymentInfo.setCreateNewTransaction(Boolean.TRUE);
                modelService.save(paymentInfo);
                isSuccess = brainTreeTransactionService
                        .createAuthorizationTransactionOfOrder(order,
                                billPayTotal.setScale(DECIMAL_PRECISION, RoundingMode.HALF_EVEN), true, paymentInfo);
            }

        }
        return isSuccess;

    }

    private void updateSerialStatusForPaidBills(final AbstractOrderModel order) {
        List < BlItemsBillingChargeModel > paidBillsWithMissingCharge = order.getOrderBills().stream().filter(bill ->
                bill.isBillPaid() && bill.getBillChargeType().getCode().equals(this.billingChargesReason)).collect(Collectors.toList());
        List < String > serialsWithMissingStatus = new ArrayList < > ();
        paidBillsWithMissingCharge.forEach(pBill ->  serialsWithMissingStatus.addAll(pBill.getSerialCodes()));

        order.getEntries().forEach(abstractOrderEntryModel -> {
            abstractOrderEntryModel.getSerialProducts().forEach(blProductModel -> {
                if(serialsWithMissingStatus.contains(blProductModel.getCode()) && abstractOrderEntryModel.getGearGuardProFullWaiverSelected() == Boolean.FALSE){
                    ((BlSerialProductModel) blProductModel).setSerialStatus(SerialStatusEnum.STOLEN_PAID_IN_FULL);
                }
                else if(serialsWithMissingStatus.contains(blProductModel.getCode()) && abstractOrderEntryModel.getGearGuardProFullWaiverSelected() == Boolean.TRUE){
                    ((BlSerialProductModel) blProductModel).setSerialStatus(SerialStatusEnum.STOLEN_PAID_12_PERCENT);
                }
                getModelService().save(blProductModel);
                BlLogger.logMessage(LOG , Level.DEBUG , "Serial Status updated of serial {} from order" , blProductModel.getCode());
            });
        });
    }

    @ViewEvent(componentID = "sendInvoice", eventName = "onClick")
    public void sendBillingInvoice() {
        try {
            if (calculateTotalBillPay().compareTo(BigDecimal.ZERO) <= 0 && !CollectionUtils.isEmpty(this.getOrderModel().getOrderBills())) {
                blEspEventService.sendBillPaidESPEvent(orderModel);
                notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                        NotificationEvent.Level.INFO, this.getLabel(BILL_INVOICE_SUCCESS));
            } else {
                notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                        NotificationEvent.Level.INFO, this.getLabel(BILL_INVOICE_FAILURE));
            }
        } catch (Exception ex) {
            notificationService.notifyUser(StringUtils.EMPTY, BlloggingConstants.MSG_CONST,
                    NotificationEvent.Level.INFO, this.getLabel(BILL_INVOICE_EXCEPTION));
            BlLogger.logMessage(LOG, Level.ERROR, "Error while executing sendBillingInvoice() ", ex);
        }
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


    public DefaultBlAvalaraTaxService getDefaultBlAvalaraTaxService() {
        return defaultBlAvalaraTaxService;
    }

    public void setDefaultBlAvalaraTaxService(DefaultBlAvalaraTaxService defaultBlAvalaraTaxService) {
        this.defaultBlAvalaraTaxService = defaultBlAvalaraTaxService;
    }
}