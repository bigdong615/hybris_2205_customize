package com.bl.backoffice.widget.controller.order;


import com.bl.core.enums.BillInfoStatus;
import com.bl.core.enums.ItemBillingChargeTypeEnum;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlProductModel;
import com.bl.facades.order.BlOrderFacade;
import com.bl.logging.BlLogger;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.hybris.backoffice.i18n.BackofficeLocaleService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.servicelayer.exceptions.ModelSavingException;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.Component;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zk.ui.select.annotation.WireVariable;
import org.zkoss.zul.*;
import org.zkoss.zul.impl.InputElement;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

public class BlOrderBillingController extends DefaultWidgetController {

    private static final Logger LOGGER = Logger.getLogger(BlOrderBillingController.class);

    private static final String IN_SOCKET = "inputObject";
    private OrderModel orderModel;
    @Wire
    private Combobox missingItemToolCombobox;
    private List<String> billingChargesReason = new ArrayList<>();
    @WireVariable
    private transient EnumerationService enumerationService;
    @WireVariable
    private transient BackofficeLocaleService cockpitLocaleService;
    @Wire
    private Grid orderEntries;
    @WireVariable
    private transient ModelService modelService;
    private String totalAmountDue;

    private transient BlOrderFacade blOrderFacade;
    private transient OrderFacade orderFacade;

    private transient BrainTreeCheckoutFacade brainTreeCheckoutFacade;
    private transient PriceDataFactory priceDataFactory;
    private transient DefaultBlESPEventService blEspEventService;

    private static final String MISSING_ITEM_TOOL = "customersupportbackoffice.billing.widget.title";
    private transient List<BlOrderBillingItemDTO> orderEntriesForBilling;
    private static final int DECIMAL_PRECISION = 2;
    private transient BrainTreeTransactionService brainTreeTransactionService;



    @SocketEvent(socketId = IN_SOCKET)
    public void initPartialRefundForm(final OrderModel inputOrder) {
        this.setOrderModel(inputOrder);
        this.getWidgetInstanceManager().setTitle(new StringBuilder(this.getWidgetInstanceManager()
                .getLabel(MISSING_ITEM_TOOL)).append(
                this.getOrderModel().getCode()).toString());
        getMissingItemReasons();
        this.initializePopupRequiredFields(inputOrder);
        this.getOrderEntries().setModel(new ListModelList<>(this.orderEntriesForBilling));
        this.getOrderEntries().renderAll();
    }

    private void getMissingItemReasons() {
        this.getEnumerationService().getEnumerationValues(ItemBillingChargeTypeEnum.class).forEach(reason ->
                this.billingChargesReason.add(this.getEnumerationService().getEnumerationName(reason, this.getLocale())));

        this.getEnumerationService().getEnumerationValues(ItemBillingChargeTypeEnum.class);
        this.missingItemToolCombobox.setModel(new ListModelArray<>(this.billingChargesReason));
    }

    private void initializePopupRequiredFields(OrderModel inputOrder) {
        this.orderEntriesForBilling = new ArrayList<>();

        inputOrder.getEntries().forEach(abstractOrderEntryModel -> {
            abstractOrderEntryModel.getSerialProducts().forEach(serialProduct -> {
                BlOrderBillingItemDTO itemDTO = new BlOrderBillingItemDTO();
                itemDTO.setProductName(abstractOrderEntryModel.getProduct().getName());
                itemDTO.setSerialNo(serialProduct.getCode());
                itemDTO.setAmount(((BlProductModel) abstractOrderEntryModel.getProduct()).getRetailPrice() !=null ? ((BlProductModel) abstractOrderEntryModel.getProduct()).getRetailPrice() : 0.0);
                itemDTO.setDamageWaiver(abstractOrderEntryModel.getGearGuardProFullWaiverSelected());
                Double subtotal = ((BlProductModel) abstractOrderEntryModel.getProduct()).getRetailPrice() !=null ? ((BlProductModel) abstractOrderEntryModel.getProduct()).getRetailPrice() * 12/100 : 0.0 ;
                itemDTO.setSubtotal(subtotal);
                itemDTO.setProcessingFee(Boolean.TRUE);
                itemDTO.setTax(24.05);
                itemDTO.setUnpaidBillNotes("Missing Item " + abstractOrderEntryModel.getProduct().getName() + " - " + "Serial# " + serialProduct.getCode());
                this.orderEntriesForBilling.add(itemDTO);

            });
        });

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
                    unPaidBillNotes.add((((InputElement) billNotes.getChildren().get(9)).getText())); //processingFee.add(new BigDecimal(((InputElement) fee.getChildren().get(7)).getText()));
                }
                billingChargeModel.setUnPaidBillingNotes(unPaidBillNotes);

                totalAmount = totalAmount.add(chargedAmount).add(taxAmount).add(processingFee);
                this.totalAmountDue = totalAmount.toString();
                billingChargeModel.setChargedAmount(totalAmount);
                getModelService().save(billingChargeModel);
                getModelService().refresh(this.getOrderModel());
                Messagebox.show("Successfully created the bill for order " + "" + this.getOrderModel().getCode()  , "SUCCESS", Messagebox.OK, Messagebox.INFORMATION);

            }
            else{
                Messagebox.show("Please mark the checkboxes to create the bills.", "ERROR", Messagebox.OK, Messagebox.ERROR);
            }

        }
        catch (ModelSavingException exception) {
            System.out.println("unable to save bcz " + exception);
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
                BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR, "Unable to delete the Bills for the order : {} due to {}" ,
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
        // = request.getParameter("paymentId");
        String paymentMethodNonce = ((BrainTreePaymentInfoModel) this.getOrderModel().getPaymentInfo()).getNonce();
        // = request.getParameter("paymentNonce");
        String billPayTotal = this.totalAmountDue;//request.getParameter("payBillTotal");
        String poNumber = ""; //request.getParameter("extendPoNumber");
        String poNotes = "";//request.getParameter("extendPoNotes");

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
                    final OrderData orderDetails = orderFacade.getOrderDetailsForCode(orderCode);
                    //order = brainTreeCheckoutFacade.getOrderByCode(orderCode);
                    //PriceData payBillTotal = convertDoubleToPriceData(payBillAmount, order);
                    //orderDetails.setOrderTotalWithTaxForPayBill(payBillTotal);
                    //model.addAttribute(ORDER_DATA, orderDetails);
                    final Map<String, List<String>> billingChargeTypeMap = brainTreeCheckoutFacade.setPayBillFlagTrue(order);
                    blEspEventService.triggerBillPaidEspEvent(billPayTotal, billingChargeTypeMap, (OrderModel) order);
                    //final ContentPageModel payBillSuccessPage = getContentPageForLabelOrId(
                            //BraintreeaddonControllerConstants.PAY_BILL_SUCCESS_CMS_PAGE);
                    //storeCmsPageInModel(model, payBillSuccessPage);
                   // setUpMetaDataForContentPage(model, payBillSuccessPage);
                    //model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS,
                            //ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
                    //return getViewForPage(model);
                    Messagebox.show("Payment capture button clicked.", "SUCCESS", Messagebox.OK, Messagebox.INFORMATION);
                }
                catch (final Exception e) {
                    BlLogger.logMessage(LOGGER , Level.ERROR , "Error while executing getPayBillDetailsForOrder " , e);
                }
            }
            else {
                Messagebox.show("Payment capture button clicked.", "ERROR", Messagebox.OK, Messagebox.ERROR);
                //return REDIRECT_PREFIX + MY_ACCOUNT + orderCode + PAY_BILL;
            }
            //return REDIRECT_PREFIX + MY_ACCOUNT + orderCode + PAY_BILL;
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

    private PriceData convertDoubleToPriceData(final Double price , final AbstractOrderModel orderModel) {
        return priceDataFactory.create(PriceDataType.BUY ,BigDecimal.valueOf(price),orderModel.getCurrency());
    }

    @ViewEvent(componentID = "sendInvoice", eventName = "onClick")
    public void sendBillingInvoice(){
        Messagebox.show("Send Invoice button clicked.", "SUCCESS", Messagebox.OK, Messagebox.INFORMATION);
    }



    private List<Component> getOrderEntriesGridRows()
    {
        return this.getOrderEntries().getRows().getChildren();
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

    protected ModelService getModelService()
    {
        return this.modelService;
    }
}