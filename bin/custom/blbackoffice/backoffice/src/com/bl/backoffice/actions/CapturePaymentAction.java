/**
 *
 */
package com.bl.backoffice.actions;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import org.apache.commons.lang.BooleanUtils;


/**
 * @author Aditi Sharma
 *
 */
public class CapturePaymentAction extends AbstractComponentWidgetAdapterAware
		implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
	protected static final String SOCKET_OUT_CONTEXT = "blCapturePaymentContext";

	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
	{
		final ConsignmentModel contextData = actionContext.getData();

		return contextData.getOrder() != null
				&& BooleanUtils.isFalse(contextData.getOrder().getIsCaptured());
	}

	public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
	{
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());
		return new ActionResult("success");
	}


}
