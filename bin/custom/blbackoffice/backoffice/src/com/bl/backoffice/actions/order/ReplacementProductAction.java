package com.bl.backoffice.actions.order;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.core.model.order.OrderModel;
import org.apache.log4j.Logger;

public class ReplacementProductAction  extends AbstractComponentWidgetAdapterAware implements
        CockpitAction<OrderModel, OrderModel> {

    private static final String SOCKET_OUT = "blProductReplacementContext";



    private static final Logger LOG = Logger.getLogger(ReplacementProductAction.class);


    private OrderModel order;


    @Override
    public ActionResult<OrderModel> perform(ActionContext<OrderModel> actionContext) {
        order = actionContext.getData();
        this.sendOutput(SOCKET_OUT, actionContext.getData());
        return new ActionResult<>(ActionResult.SUCCESS);
    }

    @Override
    public boolean canPerform(final ActionContext<OrderModel> actionContext) {
        order = actionContext.getData();
        return (order != null);
    }
}
