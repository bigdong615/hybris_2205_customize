package com.bl.backoffice.actions;

import de.hybris.platform.core.model.order.OrderModel;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


/**
 * This action class is responsible to fetch the order for update shipping information
 *
 * @author Aditi Sharma
 *
 */

public class UpdateShippingAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<OrderModel, OrderModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blCustomShippingContext";

	/**
	 * This method is responsible for fetch the order to update its shipping information
	 *
	 * @param actionContext
	 *           the action context
	 * @return the boolean
	 */
	public boolean canPerform(final ActionContext<OrderModel> actionContext)
	{
		final OrderModel order = actionContext.getData();

		return (order != null);
	}

	/**
	 * This method will fetch the action context data for blCustomShippingContext
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
