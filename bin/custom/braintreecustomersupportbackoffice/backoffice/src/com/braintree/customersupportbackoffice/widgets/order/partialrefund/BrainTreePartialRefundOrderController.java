package com.braintree.customersupportbackoffice.widgets.order.partialrefund;

import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.facade.backoffice.BraintreeBackofficePartialRefundFacade;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.zkoss.util.Locales;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Combobox;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;

import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.PartialRefundOrderAction.*;


public class BrainTreePartialRefundOrderController extends DefaultWidgetController {
    private static final Logger LOG = Logger.getLogger(BrainTreePartialRefundOrderController.class);

    private static final String IN_SOCKET = "inputObject";
    private static final String OUT_MODIFIED_ITEM = "modifiedItem";

    private PaymentTransactionEntryModel transaction;
    private OrderModel order;

    @Wire
    private Textbox amount;
    @Wire
    private Combobox transactionsCombobox;

    @Autowired
    private BraintreeBackofficePartialRefundFacade braintreeBackofficePartialRefundFacade;

    @SocketEvent(
            socketId = IN_SOCKET
    )
    public void initPartialRefundForm(OrderModel inputOrder) {
        this.setOrder(inputOrder);
        this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager().getLabel("braintree.backoffice.partial.refund.confirm.title") + " "
                + this.getOrder().getCode());
        createTransactionListDropDown(this.order, this.transactionsCombobox);
        transactionsCombobox.setSelectedIndex(0);
        this.amount.setValue(getAmount());
        transactionsCombobox.addEventListener("onChange", event -> {
            PaymentTransactionEntryModel currentModel = (PaymentTransactionEntryModel) transactionsCombobox.getAttribute(transactionsCombobox.getSelectedItem().getLabel());
            amount.setValue(getAmount());
        });
    }

    protected String getAmount() {
        PaymentTransactionEntryModel currentModel = (PaymentTransactionEntryModel) transactionsCombobox.getAttribute(transactionsCombobox.getSelectedItem().getLabel());
        return formatAmount(currentModel.getAmount());
    }

    private String formatAmount(final BigDecimal amount) {
        final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat.getNumberInstance(Locales.getCurrent());
        decimalFormat.applyPattern("#0.00");
        return decimalFormat.format(amount);
    }

    @ViewEvent(
            componentID = "refundrequest",
            eventName = "onClick"
    )
    public void confirmRefund() {
        validateAmount();
        this.transaction = (PaymentTransactionEntryModel) transactionsCombobox.getAttribute(transactionsCombobox.getSelectedItem().getLabel());
        try {
            BrainTreeResponseResultData resendResult = braintreeBackofficePartialRefundFacade.partialRefundTransaction(this.order, this.transaction, new BigDecimal(amount.getValue()));
            processResult(resendResult);
        }
        catch (BraintreeErrorException error) {
            LOG.error("Could not create partial refund for order: " + order.getCode() + " reason: " + error.getMessage());
            Messagebox.show(error.getMessage(), getLabel(WIDGET_PARTIAL_REFUND_TITLE), Messagebox.OK,
                    Messagebox.ERROR);
        }
        sendOutput(OUT_MODIFIED_ITEM, order);
    }

    private Combobox createTransactionListDropDown(OrderModel order, final Combobox transactionsCombobox) {
        for (PaymentTransactionModel transaction : order.getPaymentTransactions()) {
            for (PaymentTransactionEntryModel paymentEntry : transaction.getEntries()) {
                if (TransactionStatus.ACCEPTED.name().equals(paymentEntry.getTransactionStatus()) &&
                        (PaymentTransactionType.CAPTURE.equals(paymentEntry.getType()) || PaymentTransactionType.PARTIAL_CAPTURE.equals(paymentEntry.getType()))) {
                    transactionsCombobox.appendItem(paymentEntry.getRequestId());
                    transactionsCombobox.setAttribute(paymentEntry.getRequestId(), paymentEntry);
                }
            }
        }
        return transactionsCombobox;
    }

    protected void showErrorMessage(final BrainTreeResponseResultData result) throws InterruptedException {
        String errorMessage;
        if (StringUtils.isNotBlank(result.getErrorMessage())) {
            errorMessage = result.getErrorMessage();
        }
        else {
            errorMessage = getLabel(WIDGET_MESSAGE_PARTIAL_REFUND_ERROR);
        }
        Messagebox.show(errorMessage, getLabel(WIDGET_PARTIAL_REFUND_TITLE), Messagebox.OK, Messagebox.ERROR);
    }

    protected void showSuccessMessage(final BrainTreeResponseResultData result) throws InterruptedException {
        Messagebox.show(createSuccessMessage(result), getLabel(WIDGET_PARTIAL_REFUND_TITLE), Messagebox.OK, Messagebox.INFORMATION);
    }

    private String createSuccessMessage(final BrainTreeResponseResultData resendResult) {
        final String message = getLabel(WIDGET_MESSAGE_PARTIAL_REFUND_SUCCESS);
        final String messagePostfix = getLabel(WIDGET_MESSAGE_PARTIAL_REFUND_CREATE_SUCCESS_POSTFIX);
        if (StringUtils.isNotBlank(resendResult.getTransactionId())) {
            return String.format("%s %s: %s", message, messagePostfix, resendResult.getTransactionId());
        }
        return message;
    }

    protected void processResult(final BrainTreeResponseResultData result) {
        try {
            if (result.isSuccess()) {
                showSuccessMessage(result);
            }
            else {
                showErrorMessage(result);
            }
        }
        catch (final InterruptedException e) {
            LOG.error("Errors occurred while showing message box!", e);
        }
    }

    private void validateAmount() {
        String value = amount.getValue();
        if (StringUtils.isBlank(value)) {
            throw new WrongValueException(amount, getLabel(WIDGET_MESSAGE_AMOUNT_EMPTY));
        }
        try {
            if (BigDecimal.ZERO.equals(new BigDecimal(value))) {
                throw new WrongValueException(amount, getLabel(WIDGET_MESSAGE_AMOUNT_ZERO));
            }
        } catch (NumberFormatException e) {
            throw new WrongValueException(amount, getLabel(WIDGET_MESSAGE_AMOUNT_NUMBER_FORMAT));
        }
    }

    public PaymentTransactionEntryModel getTransaction() {
        return transaction;
    }

    public void setTransaction(PaymentTransactionEntryModel transaction) {
        this.transaction = transaction;
    }

    public OrderModel getOrder() {
        return order;
    }

    public void setOrder(OrderModel order) {
        this.order = order;
    }

}

