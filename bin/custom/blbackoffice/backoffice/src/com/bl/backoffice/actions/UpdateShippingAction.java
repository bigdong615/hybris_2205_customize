//
// Decompiled by Procyon v0.5.36
//

package com.bl.backoffice.actions;

import de.hybris.platform.core.model.order.OrderModel;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


public class UpdateShippingAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<OrderModel, OrderModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blCustomShippingContext";

	/**
	 * Can perform boolean.
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
	 * Perform action result.
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
