package com.braintree.customersupportbackoffice.widgets.transaction.submitforsettlement;

import com.braintree.model.BrainTreeTransactionDetailModel;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import com.hybris.cockpitng.annotations.SocketEvent;
import com.hybris.cockpitng.annotations.ViewEvent;
import com.hybris.cockpitng.util.DefaultWidgetController;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Messagebox;
import org.zkoss.zul.Textbox;

import javax.annotation.Resource;
import java.math.BigDecimal;

import static com.braintree.customersupportbackoffice.constants.BraintreecustomersupportbackofficeConstants.TransactionManagementActions.*;


public class BrainTreeSubmitForSettlementTransactionController extends DefaultWidgetController {

    private static final long serialVersionUID = 1L;
    private static final Logger LOG = Logger.getLogger(BrainTreeSubmitForSettlementTransactionController.class);
    private static final Object SUBMITTED_FOR_SETTLEMENT = "submitted_for_settlement";
    private static final String IN_SOCKET = "inputObject";
    private static final String OUT_CONFIRM = "confirm";

    protected BrainTreeTransactionDetailModel transaction;

    @Wire
    private Textbox transactionId;
    @Wire
    private Textbox amount;

    @Resource(name = "brainTreePaymentTransactionService")
    private BrainTreePaymentTransactionService brainTreePaymentTransactionService;

    @SocketEvent(
            socketId = IN_SOCKET
    )
    public void initSubmitForSettlementForm(BrainTreeTransactionDetailModel inputTransaction) {
        this.setTransaction(inputTransaction);
        this.getWidgetInstanceManager().setTitle(this.getWidgetInstanceManager().getLabel("braintreecustomersupportbackoffice.submitforsettlementtransaction.confirm.title") + " "
                + this.getTransaction().getId());
        this.transactionId.setValue(this.getTransaction().getId());
        this.amount.setValue(getAmount());
    }

    @ViewEvent(
            componentID = "submitforsettlementrequest",
            eventName = "onClick"
    )
    public void confirmSubmitForSettlement() {
        validate();
        processSubmit();
        this.sendOutput(OUT_CONFIRM, SUBMITTED_FOR_SETTLEMENT);
    }

    private void processSubmit(){
        continueSubmitOrder(transaction, amount);

        try {
            showMessage();
        } catch (InterruptedException e) {
            LOG.error(e.getMessage());
        }
    }

    private void validate()
    {
        String value = amount.getValue();
        if (StringUtils.isBlank(value))
        {
            throw new WrongValueException(amount, getLabel("bt.customersupport.transaction.amount.error.empty"));
        }
        try{
            if (BigDecimal.ZERO.equals(new BigDecimal(value)))
            {
                throw new WrongValueException(amount, getLabel("bt.customersupport.transaction.amount.error.zero"));
            }
        } catch (NumberFormatException e){
            throw new WrongValueException(amount, getLabel("bt.customersupport.transaction.amount.error.number.format"));
        }
    }

    private void showMessage() throws InterruptedException
    {
        Messagebox.show(getLabel(WIDGET_MESSAGE_TRANSACTION_SFS_REFRESH), getLabel(WIDGET_TRANSACTION_SFS_TITLE), Messagebox.OK, Messagebox.INFORMATION);
    }

    private void continueSubmitOrder(final BrainTreeTransactionDetailModel currentTransaction, final Textbox amountField)
    {
        getBrainTreePaymentTransactionService().continueSubmitOrder(currentTransaction, new BigDecimal(amountField.getValue()));
    }


    protected String getAmount()
    {
        final String[] split = this.getTransaction().getAmount().split(" ");
        return split[0];
    }

    public BrainTreePaymentTransactionService getBrainTreePaymentTransactionService() {
        return brainTreePaymentTransactionService;
    }

    public void setBrainTreePaymentTransactionService(BrainTreePaymentTransactionService brainTreePaymentTransactionService) {
        this.brainTreePaymentTransactionService = brainTreePaymentTransactionService;
    }

    public BrainTreeTransactionDetailModel getTransaction() {
        return transaction;
    }

    public void setTransaction(BrainTreeTransactionDetailModel transaction) {
        this.transaction = transaction;
    }
}
