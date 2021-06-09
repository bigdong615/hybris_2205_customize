package com.braintree.customersupportbackoffice.actions.order.voidorder;

import com.braintree.customersupportbackoffice.actions.order.AbstractOrderAction;
import com.braintree.facade.backoffice.BraintreeBackofficeVoidFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import de.hybris.platform.core.model.order.OrderModel;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

public class BraintreeVoidAction extends AbstractOrderAction {
    private final static Logger LOG = Logger.getLogger(BraintreeVoidAction.class);

    protected static final String SOCKET_OUT_CONTEXT = "voidContext";

    @Autowired
    private BraintreeBackofficeVoidFacade braintreeBackofficeOrderFacade;


    @Override
    public String getConfirmationMessage(ActionContext<OrderModel> actionContext) {
        return "-abc-";
    }

    @Override
    public boolean needsConfirmation(ActionContext<OrderModel> actionContext) {
        return false;
    }

    @Override
    public boolean canPerform(ActionContext<OrderModel> actionContext) {
        if (actionContext.getData() != null) {
            if (!isExtendedPaymentInfo(actionContext)) {
                actionContext.setParameter(ActionContext.VIEW_MODE_PARAM, ActionContext.VIEWMODE_HIDDEN);
                return false;
            }
            return braintreeBackofficeOrderFacade.isVoidPossible(actionContext.getData());
        }
        return false;
    }

    @Override
    public ActionResult<OrderModel> perform(ActionContext<OrderModel> actionContext) {
        LOG.info("perform, actionContext: " + actionContext);

        sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());

        ActionResult<OrderModel> actionResult = new ActionResult<>(ActionResult.SUCCESS);
        return actionResult;
    }

    private boolean isExtendedPaymentInfo(ActionContext<OrderModel> actionContext) {
        return actionContext.getData().getPaymentInfo() instanceof BrainTreePaymentInfoModel;
    }

}
