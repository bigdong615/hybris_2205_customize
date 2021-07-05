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

	protected static final String SOCKET_OUT_CONTEXT = "blCreatePackageShipmentContext";

	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consignment = actionContext.getData();

		return (consignment != null && checkOrderStatus(consignment));
	}

	public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel consignment = actionContext.getData();
		modelService.refresh(consignment);
		final List<PackagingInfoModel> packages = consignment.getPackaginginfos();

		for (final PackagingInfoModel packagingInfoModel : packages)
		{
			blCreateShipmentFacade.createBlShipmentPackages(packagingInfoModel);
		}
		System.out.println("Shipment Created");
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
