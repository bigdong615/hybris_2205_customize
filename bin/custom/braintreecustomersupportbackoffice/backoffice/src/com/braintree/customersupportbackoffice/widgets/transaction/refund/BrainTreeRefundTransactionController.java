package com.braintree.customersupportbackoffice.widgets.transaction.refund;

import com.braintree.customersupportbackoffice.widgets.transaction.AbstractTransactionActionController;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import de.hybris.platform.core.model.order.OrderModel;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zul.Messagebox;

import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.TransactionManagementActions.*;

public class BrainTreeRefundTransactionController extends AbstractTransactionActionController {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(BrainTreeRefundTransactionController.class);
    private static final Object REFUNDED = "refunded";

    @SocketEvent(
            socketId = IN_SOCKET
    )
    public void initRefundForm(BrainTreeTransactionDetailModel inputTransaction) {
        this.setTransaction(inputTransaction);
        this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager().getLabel("braintreecustomersupportbackoffice.refundtransaction.confirm.title") + " "
                + this.getTransaction().getId());
        this.transactionId.setValue(this.getTransaction().getId());
        this.amount.setValue(getAmount());
    }

    @ViewEvent(
            componentID = "refundrequest",
            eventName = "onClick"
    )
    public void confirmRefund() {
        validate();
        processRefund();
        this.sendOutput(OUT_CONFIRM, REFUNDED);
    }

    private void processRefund(){
        final OrderModel linkedOrder = transaction.getLinkedOrder();
        if (linkedOrder != null)
        {
            Messagebox.show(getLabel(WIDGET_MESSAGE_REFUND_ERROR_GO_TO_ORDER), getLabel(WIDGET_REFUND_TITLE), Messagebox.OK,
                    Messagebox.ERROR);
        }
        else
        {
            final BrainTreeResponseResultData resendResult = brainTreeCustomerSupportFacade.refundTransaction(transaction,
                    amount.getValue());
            processResult(resendResult);
        }
    }

    @Override
    protected void showErrorMessage(final BrainTreeResponseResultData result) throws InterruptedException
    {
        String errorMessage;
        if (StringUtils.isNotBlank(result.getErrorMessage()))
        {
            errorMessage = result.getErrorMessage();
        }
        else
        {
            errorMessage = getLabel(WIDGET_MESSAGE_REFUND_ERROR);
        }
        Messagebox.show(errorMessage, getLabel(WIDGET_REFUND_TITLE), Messagebox.OK, Messagebox.ERROR);
    }

    @Override
    protected void showSuccessMessage(final BrainTreeResponseResultData result) throws InterruptedException
    {
        Messagebox.show(createSuccessMessage(result), getLabel(WIDGET_REFUND_TITLE), Messagebox.OK, Messagebox.INFORMATION);
    }

    private String createSuccessMessage(final BrainTreeResponseResultData resendResult)
    {
        final String message = getLabel(WIDGET_MESSAGE_REFUND_SUCCESS);
        final String messagePostfix = getLabel(WIDGET_MESSAGE_TRANSACTION_CREATE_SUCCESS_POSTFIX);
        if (StringUtils.isNotBlank(resendResult.getTransactionId()))
        {
            return String.format("%s %s: %s", message, messagePostfix, resendResult.getTransactionId());
        }
        return message;
    }

    public BrainTreeTransactionDetailModel getTransaction() {
        return transaction;
    }

    public void setTransaction(BrainTreeTransactionDetailModel transaction) {
        this.transaction = transaction;
    }
}
