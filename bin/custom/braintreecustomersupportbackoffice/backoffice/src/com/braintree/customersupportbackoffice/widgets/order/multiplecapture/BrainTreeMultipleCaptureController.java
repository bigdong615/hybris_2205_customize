package com.braintree.customersupportbackoffice.widgets.order.multiplecapture;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficeMultiCaptureFacade;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.order.capture.partial.services.BraintreePartialCaptureService;
import com.braintree.order.submitForSettlement.service.BraintreeSubmitForSettlementService;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.zkoss.util.Locales;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Comboitem;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;

import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.OrderManagementActions.*;

public class BrainTreeMultipleCaptureController extends DefaultWidgetController {
    private static final Logger LOG = Logger.getLogger(BrainTreeMultipleCaptureController.class);

    private static final String IN_SOCKET = "inputObject";
    private static final String OUT_MODIFIED_ITEM = "modifiedItem";

    private OrderModel order;
    private String transactionId;

    @Wire
    private Textbox orderCode;
    @Wire
    private Textbox customer;
    @Wire
    private Textbox amount;
    @Wire
    private Combobox transactions;

    @Resource(name = "orderService")
    private BraintreePartialCaptureService braintreePartialCaptureService;

    @Resource
    private BrainTreePaymentService brainTreePaymentService;

    @Resource
    private BrainTreePaymentTransactionService brainTreePaymentTransactionService;

    @Resource
    private BraintreeSubmitForSettlementService braintreeSubmitForSettlementService;

    @Resource
    private BrainTreeConfigService brainTreeConfigService;

    @Resource
    private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

    @Autowired
    private BraintreeBackofficeMultiCaptureFacade braintreeBackofficeMultiCaptureFacade;


    @SocketEvent(socketId = IN_SOCKET)
    public void initCreateReturnRequestForm(OrderModel inputOrder) {
        this.setOrder(inputOrder);
        this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager().getLabel("braintreecustomersupportbackoffice.multiplecapture.confirm.title")
                + " " + this.getOrder().getCode());
        this.orderCode.setValue(this.getOrder().getCode());
        this.customer.setValue(this.getOrder().getUser().getDisplayName());
        configureTransactionsCombobox(inputOrder);
        transactions.setSelectedIndex(0);
        this.amount.setValue(getAmount());
        transactions.addEventListener("onChange", event -> {
            this.amount.setValue(getAmount());
        });
    }

    private String getAmount()
    {
        PaymentTransactionEntryModel currentModel = (PaymentTransactionEntryModel) transactions.getSelectedItem().getValue();
        BigDecimal amountForCapture = braintreePartialCaptureService
                .getPossibleAmountForCaptureForAuthorizedTransaction(currentModel.getPaymentTransaction(), currentModel.getAmount());
        return formatAmount(amountForCapture);
    }

    private void configureTransactionsCombobox(final OrderModel inputOrder) {
        List<PaymentTransactionEntryModel> trans = braintreeBackofficeMultiCaptureFacade.getMultiCaptureableTransactions(inputOrder);
        LOG.info("MultiCaptureable transactions: " + trans);

        for (PaymentTransactionEntryModel v : trans) {
            Comboitem ci = new Comboitem();
            ci.setValue(v);
            ci.setLabel(v.getRequestId());
            if (brainTreeConfigService.getStoreInVault().equals(Boolean.FALSE.toString()) && !isPayPalCheckout())
            {
                amount.setValue(v.getAmount().toString());
                amount.setDisabled(true);
            }
            transactions.appendChild(ci);
        }

        if (!transactions.getItems().isEmpty()) {
            transactions.setSelectedItem(transactions.getItems().get(0));
        }
    }

    @ViewEvent(componentID = "multiplecapturerequest", eventName = "onClick")
    public void confirm() {
        if (transactions.getSelectedIndex() != -1) {
            transactionId = transactions.getSelectedItem().getLabel();
        }
        validateAmount();
        processCapture();
    }

    private void processCapture() {
        final BigDecimal amount = new BigDecimal(this.amount.getValue());

        try {
            Boolean isComplete;
            if (braintreePartialCaptureService.isPartialCapturePossible(order) && isPayPalCheckout()
                    && !isCaptureAmountEqualsTotal(amount))
            {
                isComplete = partialCapture(amount);
            }
            else
            {
                isComplete = submitForSettlement(amount);
            }
            if (isComplete)
            {
                Messagebox.show(getLabel(PARTIAL_CAPTURE_SUCCESS), getLabel(PARTIAL_CAPTURE_TITLE), Messagebox.OK,
                        Messagebox.INFORMATION);
                sendOutput(OUT_MODIFIED_ITEM, order);
            }
            else
            {
                Messagebox.show(getLabel(PARTIAL_CAPTURE_AMOUNT_NOT_MATH_ERROR) + " " + formatAmount(getAmountAvailableForMultiCapture()),
                        getLabel(PARTIAL_CAPTURE_TITLE), Messagebox.OK, Messagebox.ERROR);
            }
        } catch (BraintreeErrorException e) {
            Messagebox.show(e.getMessage(), getLabel(PARTIAL_CAPTURE_ERROR), Messagebox.OK, Messagebox.ERROR);
        }
    }

    private Boolean partialCapture(final BigDecimal amount) throws BraintreeErrorException
    {
        if (order != null && brainTreePaymentTransactionService.isValidTransactionId(order, transactionId))
        {
            return braintreePartialCaptureService.partialCapture(order, amount, transactionId);
        }
        return Boolean.FALSE;
    }

    private Boolean submitForSettlement(final BigDecimal amount) throws BraintreeErrorException
    {
        if (order != null && brainTreePaymentTransactionService.isValidTransactionId(order, transactionId))
        {
            if (braintreeSubmitForSettlementService.isSubmitForSettlementAvailable(order))
            {
                return braintreeSubmitForSettlementService.submitForSettlement(order, amount, transactionId);
            }
            if (brainTreePaymentTransactionService.canPerformDelayedCapture(order, amount))
            {
                return brainTreeCheckoutFacade.authorizePayment(order, amount);
            }
        }
        return Boolean.FALSE;
    }

    private Boolean isPayPalCheckout()
    {
        final String paymentProvider = order.getPaymentInfo().getProperty("paymentProvider");
        return paymentProvider != null && (BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT.equals(paymentProvider)
                || BraintreeConstants.PAYPAL_PAYMENT.equals(paymentProvider));
    }

    private String formatAmount(final BigDecimal amount) {
        final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat.getNumberInstance(Locales.getCurrent());
        decimalFormat.applyPattern("#0.00");
        return decimalFormat.format(amount);
    }

    private boolean isCaptureAmountEqualsTotal(final BigDecimal amount)
    {
        return amount.doubleValue() >= order.getTotalPrice();
    }

    private BigDecimal getAmountAvailableForMultiCapture() {
        if (order != null) {
            return braintreePartialCaptureService.getPossibleAmountForCapture(order);
        }
        return BigDecimal.ZERO;
    }

    private void validateAmount() {
        String value = amount.getValue();
        if (StringUtils.isBlank(value)) {
            throw new WrongValueException(amount, getLabel("bt.customersupport.order.amount.error.empty"));
        }
        try {
            if (BigDecimal.ZERO.equals(new BigDecimal(value))) {
                throw new WrongValueException(amount, getLabel("bt.customersupport.order.amount.error.zero"));
            }
        } catch (NumberFormatException e) {
            throw new WrongValueException(amount, getLabel("bt.customersupport.order.amount.error.number.format"));
        }
    }

    public BraintreePartialCaptureService getBraintreePartialCaptureService() {
        return braintreePartialCaptureService;
    }

    public void setBraintreePartialCaptureService(BraintreePartialCaptureService braintreePartialCaptureService) {
        this.braintreePartialCaptureService = braintreePartialCaptureService;
    }

    public OrderModel getOrder() {
        return order;
    }

    public void setOrder(OrderModel order) {
        this.order = order;
    }
}
