package com.bl.backoffice.actions;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;

import com.bl.integration.constants.BlintegrationConstants;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


/**
 * This class is responsible for creating package
 *
 * @author Keyur
 */

public class CreatePackageAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blCreatePackageContext";

	@Resource(name = "blShipmentCreationService")
	private DefaultBLShipmentCreationService blShipmentCreationService;


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
				&& getBlShipmentCreationService().checkOrderAndConsignmentStatus(consigment));
	}

	/**
	 * This method will fetch the action context data for blCreatePackageContext
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
	 * @return the blShipmentCreationService
	 */
	public DefaultBLShipmentCreationService getBlShipmentCreationService()
	{
		return blShipmentCreationService;
	}

	/**
	 * @param blShipmentCreationService
	 *           the blShipmentCreationService to set
	 */
	public void setBlShipmentCreationService(final DefaultBLShipmentCreationService blShipmentCreationService)
	{
		this.blShipmentCreationService = blShipmentCreationService;
	}
}
