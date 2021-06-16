package com.braintree.customersupportbackoffice.actions.transaction.refresh;

import com.braintree.customersupportbackoffice.actions.transaction.AbstractTransactionAction;
import com.braintree.hybris.data.BraintreeTransactionEntryData;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.core.impl.DefaultWidgetModel;

public class BrainTreeRefreshTransactionAction extends AbstractTransactionAction {

    @Override
    public ActionResult<BrainTreeTransactionDetailModel> perform(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        final BraintreeTransactionEntryData actualTransaction = getBrainTreeCustomerSupportFacade()
                .findTransaction(ctx.getData().getId());

        final BrainTreeTransactionDetailModel originalModel = getTransactionDetailPopulator().convert(actualTransaction);

        ((DefaultWidgetModel)ctx.getParameter("parentWidgetModel")).put("currentObject", originalModel);

        return new ActionResult<BrainTreeTransactionDetailModel>(ActionResult.SUCCESS, originalModel);
    }

    @Override
    public boolean canPerform(final ActionContext<BrainTreeTransactionDetailModel> ctx)
    {
        return true;
    }
}
