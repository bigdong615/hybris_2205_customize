package com.bl.backoffice.actions.order;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.core.model.order.OrderModel;

/**
 * ################## BL-986 #################
 * This action class is being used to render the action button for refund the amount if order
 * has been cancelled.
 *
 * @author Krishan Vashishth
 */
public class BlCustomOrderRefundAction extends AbstractComponentWidgetAdapterAware implements
    CockpitAction<OrderModel, OrderModel> {

  private static final String SOCKET_OUT = "orderRefundContext";

  @Override
  public ActionResult<OrderModel> perform(final ActionContext<OrderModel> ctx) {
    this.sendOutput(SOCKET_OUT, ctx.getData());
    return new ActionResult<>(ActionResult.SUCCESS);
  }
}
