package com.bl.backoffice.actions;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.List;

import javax.annotation.Resource;

import com.bl.integration.constants.BlintegrationConstants;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;

/**
 * This class is responsible to clear all packages 
 * @author Keyur
 */

public class ClearPackageAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{

	@Resource(name = "modelService")
	private ModelService modelService;

	protected static final String SOCKET_OUT_CONTEXT = "blCreatePackageContext";

	/**
	 * This method is responsible for fetch the consignment which are not in CANCELLED, CHECKED_INVALID,
	 * PAYMENT_NOT_AUTHORIZED and PAYMENT_DECLINED status
	 *
	 * @param actionContext
	 *           the action context
	 * @return the boolean
	 */

	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel order = actionContext.getData();

		return (order != null);
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
		final List<PackagingInfoModel> packages = consignment.getPackaginginfos();
		modelService.removeAll(packages);
		modelService.refresh(consignment);
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
		return new ActionResult(BlintegrationConstants.SUCCESS);
	}

}
