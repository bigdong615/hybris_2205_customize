/**
 *
 */
package com.bl.backoffice.actions;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;


public class BlReassignSerialAction extends AbstractComponentWidgetAdapterAware implements
		CockpitAction<ConsignmentModel, ConsignmentModel> {

	protected static final String SOCKET_OUT_CONTEXT = "reassignserialcontext";


	public BlReassignSerialAction() {
		//constructor
	}

	@Override
	public boolean canPerform(final ActionContext<ConsignmentModel> actionContext) {
		return true;
	}

	@Override
	public String getConfirmationMessage(final ActionContext<ConsignmentModel> actionContext) {
		return null;
	}

	@Override
	public boolean needsConfirmation(final ActionContext<ConsignmentModel> actionContext) {
		return false;
	}

	public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext) {
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());

		return new ActionResult<>("success");
	}

}
