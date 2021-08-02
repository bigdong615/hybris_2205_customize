//
// Decompiled by Procyon v0.5.36
//

package com.bl.backoffice.actions;

import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.List;

import javax.annotation.Resource;

import com.bl.integration.facades.BlCreateShipmentFacade;
import com.bl.integration.services.impl.DefaultBLShipmentCreationService;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


public class CreateShipmentAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{

	@Resource(name = "modelService")
	private ModelService modelService;

	@Resource(name = "blCreateShipmentFacade")
	private BlCreateShipmentFacade blCreateShipmentFacade;
	
	@Resource(name = "blShipmentCreationService")
	private DefaultBLShipmentCreationService blShipmentCreationService;

	protected static final String SOCKET_OUT_CONTEXT = "blCreatePackageShipmentContext";

	/**
	 * Can perform boolean.
	 *
	 * @param actionContext
	 *           the action context
	 * @return the boolean
	 */
	
	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consignment = actionContext.getData();

		return (consignment != null && getBlShipmentCreationService().checkOrderStatus(consignment));
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
		final ConsignmentModel consignment = actionContext.getData();
		modelService.refresh(consignment);
		final List<PackagingInfoModel> packages = consignment.getPackaginginfos();

		//Create Shipment and generate lable
		for (final PackagingInfoModel packagingInfoModel : packages)
		{
			blCreateShipmentFacade.createBlShipmentPackages(packagingInfoModel);
		}
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
		return new ActionResult("success");
	}

	/**
	 * @return the blShipmentCreationService
	 */
	public DefaultBLShipmentCreationService getBlShipmentCreationService()
	{
		return blShipmentCreationService;
	}

	/**
	 * @param blShipmentCreationService the blShipmentCreationService to set
	 */
	public void setBlShipmentCreationService(DefaultBLShipmentCreationService blShipmentCreationService)
	{
		this.blShipmentCreationService = blShipmentCreationService;
	}

}
