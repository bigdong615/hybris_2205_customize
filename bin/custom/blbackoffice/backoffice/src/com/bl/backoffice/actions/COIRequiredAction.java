package com.bl.backoffice.actions;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.core.model.order.OrderModel;


/**
 * This action class is responsible to trigger COI_required ESP event
 *
 * @author Sunil Sahu
 *
 */

public class COIRequiredAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<OrderModel, OrderModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blCoiRequiredContext";

	/**
	 * This method is responsible for trigger COI_required ESP event
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
	 * This method will fetch the action context data for blCoiRequiredContext
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
