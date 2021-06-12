package com.braintree.customersupportbackoffice.widgets.transaction.clone;

import com.braintree.customersupportbackoffice.widgets.transaction.AbstractTransactionActionController;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zul.Messagebox;

import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.TransactionManagementActions.*;


public class BrainTreeCloneTransactionController extends AbstractTransactionActionController {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(BrainTreeCloneTransactionController.class);
    private static final Object CLONED = "cloned";

    @SocketEvent(
            socketId = IN_SOCKET
    )
    public void initCloneForm(BrainTreeTransactionDetailModel inputTransaction) {
        this.setTransaction(inputTransaction);
        this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager().getLabel("braintreecustomersupportbackoffice.clonetransaction.confirm.title") + " "
                + this.getTransaction().getId());
        this.transactionId.setValue(this.getTransaction().getId());
        this.amount.setValue(getAmount());
    }

    @ViewEvent(
            componentID = "clonerequest",
            eventName = "onClick"
    )
    public void confirmClone() {
        validate();
        processClone();
        this.sendOutput(OUT_CONFIRM, CLONED);
    }

    private void processClone(){
        final BrainTreeResponseResultData resendResult = brainTreeCustomerSupportFacade.cloneTransaction(transaction,
                amount.getValue(), false);
        processResult(resendResult);
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
            errorMessage = getLabel(WIDGET_MESSAGE_CLONE_ERROR);
        }
        Messagebox.show(errorMessage, getLabel(WIDGET_CLONE_TITLE), Messagebox.OK, Messagebox.ERROR);
    }

    @Override
    protected void showSuccessMessage(final BrainTreeResponseResultData result) throws InterruptedException
    {
        Messagebox.show(createdSuccessMessage(result),
                getLabel(WIDGET_CLONE_TITLE), Messagebox.OK,
                Messagebox.INFORMATION);
    }

    private String createdSuccessMessage(final BrainTreeResponseResultData resendResult)
    {
        final String message = getLabel(WIDGET_MESSAGE_CLONE_SUCCESS);
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
