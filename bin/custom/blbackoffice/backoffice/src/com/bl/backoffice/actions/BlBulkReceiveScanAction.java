/**
 *
 */
package com.bl.backoffice.actions;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;

/**
 * @author ravinder
 *
 */
public class BlBulkReceiveScanAction extends AbstractComponentWidgetAdapterAware implements CockpitAction<Object, Object>

{
	protected static final String SOCKET_OUT_CONTEXT = "blBulkReceiveScanContext";

	@Override
	public ActionResult<Object> perform(final ActionContext<Object> actionContext)
	{
		this.sendOutput(SOCKET_OUT_CONTEXT, actionContext.getData());

		return new ActionResult<>("success");
	}
}
