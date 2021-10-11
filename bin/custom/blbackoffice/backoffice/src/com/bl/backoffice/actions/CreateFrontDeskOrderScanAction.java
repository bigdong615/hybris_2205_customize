package com.bl.backoffice.actions;

import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import org.apache.commons.collections.CollectionUtils;

import com.bl.integration.constants.BlintegrationConstants;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


/**
 * This action class is responsible to find the consignment for scanning
 *
 * @author Aditi Sharma
 *
 */
public class CreateFrontDeskOrderScanAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blFrontDeskOrderScanContext";

	/**
	 * This method is responsible for fetch the consignment which are not in CANCELLED, CHECKED_INVALID,
	 * PAYMENT_NOT_AUTHORIZED and PAYMENT_DECLINED status
	 *
	 * @param actionContext
	 *           the action context
	 * @return the boolean
	 */
	@Override
	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consigment = actionContext.getData();

		return (consigment != null && CollectionUtils.isNotEmpty(consigment.getConsignmentEntries())
				&& checkOrderStatus(consigment));
	}

	/**
	 * This method will fetch the action context data for blShippingScanContext
	 *
	 * @param actionContext
	 *           the action context
	 * @return the action result
	 */
	public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
	{
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
		return new ActionResult(BlintegrationConstants.SUCCESS);
	}

	/**
	 * method will used to check the order status for shipment
	 *
	 * @param consignment
	 * @return
	 */
	public boolean checkOrderStatus(final ConsignmentModel consignment)
	{
		if (consignment.getOrder() != null)
		{
			final OrderStatus status = consignment.getOrder().getStatus();
			if (status.equals(OrderStatus.CHECKED_INVALID) || status.equals(OrderStatus.PAYMENT_NOT_AUTHORIZED)
					|| status.equals(OrderStatus.RECEIVED_PAYMENT_DECLINED))
			{
				return false;
			}
		}
		return true;
	}

}
