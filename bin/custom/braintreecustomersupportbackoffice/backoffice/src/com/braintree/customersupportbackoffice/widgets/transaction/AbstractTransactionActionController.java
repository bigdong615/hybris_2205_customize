package com.braintree.customersupportbackoffice.widgets.transaction;

import com.braintree.customersupportbackoffice.facade.BrainTreeCustomerSupportFacade;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.hybris.cockpitng.util.DefaultWidgetController;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.zkoss.zk.ui.WrongValueException;
import org.zkoss.zk.ui.select.annotation.Wire;
import org.zkoss.zul.Textbox;

import javax.annotation.Resource;
import java.math.BigDecimal;

public abstract class AbstractTransactionActionController  extends DefaultWidgetController {

    private static final Logger LOG = Logger.getLogger(AbstractTransactionActionController.class);
    protected static final String IN_SOCKET = "inputObject";
    protected static final String OUT_CONFIRM = "confirm";
    private static final Object FAILED = "failed";

    protected BrainTreeTransactionDetailModel transaction;

    @Wire
    protected Textbox transactionId;
    @Wire
    protected Textbox amount;

    @Resource(name = "brainTreeCustomerSupportFacade")
    protected BrainTreeCustomerSupportFacade brainTreeCustomerSupportFacade;

    protected void processResult(final BrainTreeResponseResultData result)
    {
        try
        {
            if (result.isSuccess())
            {
                showSuccessMessage(result);
            }
            else
            {
                showErrorMessage(result);
            }
        }
        catch (final InterruptedException e)
        {
            LOG.debug("Errors occurred while showing message box!", e);
        }
    }

    protected String getAmount()
    {
        final String[] split = this.getTransaction().getAmount().split(" ");
        return split[0];
    }

    protected void validate()
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

    protected abstract void showErrorMessage(BrainTreeResponseResultData result) throws InterruptedException;

    protected abstract void showSuccessMessage(BrainTreeResponseResultData result) throws InterruptedException;

    public BrainTreeTransactionDetailModel getTransaction() {
        return transaction;
    }

    public void setTransaction(BrainTreeTransactionDetailModel transaction) {
        this.transaction = transaction;
    }
}
