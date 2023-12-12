package com.bl.backoffice.actions.order;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.core.model.order.OrderModel;

/**
 *
 *
 * @author Jyoti Swamy
 */
public class BlCustomOrderBillingAction extends AbstractComponentWidgetAdapterAware implements
        CockpitAction<OrderModel, OrderModel> {

    private static final String SOCKET_OUTPUT_CTX = "customOrderBillingContext";

    @Override
    public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext) {
        this.sendOutput(SOCKET_OUTPUT_CTX, actionContext.getData());
        return new ActionResult("success");
    }


}