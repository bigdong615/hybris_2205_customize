/**
 * @author Keyur
 */
package com.bl.backoffice.actions;

import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import org.apache.commons.collections.CollectionUtils;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


public class CreatePackageScanAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blPackageScanContext";

	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consigment = actionContext.getData();

		return (consigment != null && CollectionUtils.isNotEmpty(consigment.getConsignmentEntries())
				&& checkOrderStatus(consigment));
	}

	public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
	{
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
		return new ActionResult("success");
	}

	public boolean checkOrderStatus(final ConsignmentModel consignment)
	{
		final OrderStatus status = consignment.getOrder().getStatus();
		if (status.equals(OrderStatus.CANCELLED) || status.equals(OrderStatus.CHECKED_INVALID)
				|| status.equals(OrderStatus.PAYMENT_NOT_AUTHORIZED))
		{
			return false;
		}
		return true;
	}
}
