package com.braintree.customersupportbackoffice.actions.order.authorize;

import com.braintree.customersupportbackoffice.actions.order.AbstractOrderAction;
import com.braintree.facade.backoffice.BraintreeBackofficeAuthorizeFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import de.hybris.platform.core.model.order.OrderModel;
import org.springframework.beans.factory.annotation.Autowired;

public class BrainTreeAuthorizeOrderAction extends AbstractOrderAction implements CockpitAction<OrderModel, OrderModel> {

    private static final String SOCKET_OUT_CONTEXT = "authorizeOrderContext";

    @Autowired
    private BraintreeBackofficeAuthorizeFacade braintreeBackofficeAuthorizeFacade;

    public boolean canPerform(ActionContext<OrderModel> actionContext) {
        OrderModel order = actionContext.getData();

        if (order != null) {
            if (!isOrderHasExtendedPaymentInfo(order) ) {
                actionContext.setParameter(ActionContext.VIEW_MODE_PARAM, ActionContext.VIEWMODE_HIDDEN);
                return false;
            }
        }
        return braintreeBackofficeAuthorizeFacade.isAuthorizePossible(order);
    }

    public ActionResult<OrderModel> perform(ActionContext<OrderModel> actionContext) {
        this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
        ActionResult<OrderModel> actionResult = new ActionResult<>(ActionResult.SUCCESS);
        return actionResult;
    }

    private boolean isOrderHasExtendedPaymentInfo(final OrderModel order) {
        return order.getPaymentInfo() instanceof BrainTreePaymentInfoModel;
    }
}
