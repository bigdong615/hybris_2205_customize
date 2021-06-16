package com.braintree.customersupportbackoffice.actions.transaction.voidaction;

import com.braintree.customersupportbackoffice.actions.transaction.AbstractTransactionAction;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import org.zkoss.zul.Messagebox;

public class BrainTreeVoidTransactionAction extends AbstractTransactionAction {

    @Override
    public ActionResult<BrainTreeTransactionDetailModel> perform(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        ActionResult<BrainTreeTransactionDetailModel> result = null;
        final BrainTreeTransactionDetailModel data = ctx.getData();
        BrainTreeResponseResultData response = getBrainTreeCustomerSupportFacade().voidTransaction(data);
        if (response.isSuccess())
        {
            Messagebox.show(ctx.getLabel("void.action.success.message"), ctx.getLabel("void.action.success"), Messagebox.OK, Messagebox.INFORMATION);
            return new ActionResult<BrainTreeTransactionDetailModel>(ActionResult.SUCCESS, data);
        }
        else
        {
            Messagebox.show(response.getErrorMessage(), ctx.getLabel("void.action.error"), Messagebox.OK, Messagebox.ERROR);
            return new ActionResult<BrainTreeTransactionDetailModel>(ActionResult.ERROR);
        }
    }

    @Override
    public boolean canPerform(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        return  getBrainTreeTransactionManagementActionService().isVoidPossible(ctx.getData());
    }

    @Override
    public boolean needsConfirmation(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        return true;
    }

    @Override
    public String getConfirmationMessage(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        return ctx.getLabel("void.action.confirmation.message");
    }
}
