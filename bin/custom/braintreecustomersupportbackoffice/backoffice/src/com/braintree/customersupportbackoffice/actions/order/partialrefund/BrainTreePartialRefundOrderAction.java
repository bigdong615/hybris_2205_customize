package com.braintree.customersupportbackoffice.actions.order.partialrefund;

import com.braintree.customersupportbackoffice.actions.order.AbstractOrderAction;
import com.braintree.facade.backoffice.BraintreeBackofficePartialRefundFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import de.hybris.platform.core.model.order.OrderModel;
import org.springframework.beans.factory.annotation.Autowired;


public class BrainTreePartialRefundOrderAction extends AbstractOrderAction  {

    protected static final String SOCKET_OUT_CONTEXT = "partialRefundOrderContext";

    @Autowired
    private BraintreeBackofficePartialRefundFacade braintreeBackofficePartialRefundFacade;

    public boolean canPerform(ActionContext<OrderModel> actionContext) {
        if (actionContext.getData() != null) {
            if (!isExtendedPaymentInfo(actionContext)) {
                actionContext.setParameter(ActionContext.VIEW_MODE_PARAM, ActionContext.VIEWMODE_HIDDEN);
            }
            return braintreeBackofficePartialRefundFacade.isPartialRefundPossible(actionContext.getData());
        }
        return false;
    }

    public ActionResult<OrderModel> perform(ActionContext<OrderModel> actionContext) {
        this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
        ActionResult<OrderModel> actionResult = new ActionResult<>(ActionResult.SUCCESS);
        return actionResult;
    }

    private boolean isExtendedPaymentInfo(ActionContext<OrderModel> actionContext) {
        return actionContext.getData().getPaymentInfo() instanceof BrainTreePaymentInfoModel;
    }
}
