package com.braintree.customersupportbackoffice.actions.transaction.refund;

import com.braintree.customersupportbackoffice.actions.transaction.AbstractTransactionAction;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;

public class BrainTreeRefundTransactionAction extends AbstractTransactionAction {
    private static final String SOCKET_OUT_CONTEXT = "refundTransactionContext";

    @Override
    public ActionResult<BrainTreeTransactionDetailModel> perform(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        this.sendOutput(SOCKET_OUT_CONTEXT, ctx.getData());
        ActionResult<BrainTreeTransactionDetailModel> actionResult = new ActionResult("success");
        actionResult.getStatusFlags().add(ActionResult.StatusFlag.OBJECT_PERSISTED);
        return actionResult;
    }

    @Override
    public boolean canPerform(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        return getBrainTreeTransactionManagementActionService().isRefundPossible(ctx.getData());

    }
}
