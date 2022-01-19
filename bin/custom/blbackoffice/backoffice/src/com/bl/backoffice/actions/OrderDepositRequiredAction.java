package com.bl.backoffice.actions;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.core.model.order.OrderModel;

/**
 * This method created for deposit required action for Depsoit Required ESP Event
 * @author Manikandan
 */
public class OrderDepositRequiredAction extends AbstractComponentWidgetAdapterAware implements
    CockpitAction<OrderModel, OrderModel> {

  protected static final String SOCKET_OUT_CONTEXT = "blOrderDepositRequiredContext";


  /**
   * This method is responsible for trigger Deposit Required ESP event
   *
   * @param actionContext
   *           the action context
   * @return the boolean
   */
  @Override
  public boolean canPerform(final ActionContext<OrderModel> actionContext)
  {
    final OrderModel order = actionContext.getData();

    return (order != null);
  }

  /**
   * This method will fetch the action context data for blOrderDepositRequiredContext
   *
   * @param actionContext
   *           the action context
   * @return the action result
   */
  public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext)
  {
    this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
    return new ActionResult("success");
  }

}
