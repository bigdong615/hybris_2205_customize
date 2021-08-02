/**
 * 
 */
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
 * @author Aditi Sharma
 *
 */
public class CreatePackageScanAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blPackageScanContext";

	@Resource(name = "blShipmentCreationService")
	private DefaultBLShipmentCreationService blShipmentCreationService;

	/**
	 * Can perform boolean.
	 *
	 * @param actionContext
	 *           the action context
	 * @return the boolean
	 */
	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consigment = actionContext.getData();

		return (consigment != null && CollectionUtils.isNotEmpty(consigment.getConsignmentEntries())
				&& getBlShipmentCreationService().checkOrderStatus(consigment));
	}

	/**
	 * Perform action result.
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
