/**
 *
 */
package com.bl.backoffice.actions;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

/**
 * @author Krishan Vashishth
 *
 */
public class CapturePaymentAction extends AbstractComponentWidgetAdapterAware
    implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
  private static final String SOCKET_OUT_CONTEXT = "blCapturePaymentContext";
  private static final String LOCATION_PATH = "actions/blFrontCapturePayment";
  private static final String FRONT_SOCKET_OUT_CTX = "blFrontCapturePaymentContext";

  @Override
  public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
  {
    final ConsignmentModel contextData = actionContext.getData();
    return contextData != null && contextData.getOrder() != null;
  }

  public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
  {
    final String socketOuptut = actionContext.getDefinition().getLocationPath()
        .contains(LOCATION_PATH) ? FRONT_SOCKET_OUT_CTX : SOCKET_OUT_CONTEXT;
    this.sendOutput(socketOuptut, actionContext.getData());
    return new ActionResult("success");
  }


}
