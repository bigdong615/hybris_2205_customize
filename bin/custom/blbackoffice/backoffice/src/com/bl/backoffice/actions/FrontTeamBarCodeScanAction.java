package com.bl.backoffice.actions;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

/**
 * @author Krishan Vashishth
 */
public class FrontTeamBarCodeScanAction extends AbstractComponentWidgetAdapterAware
    implements CockpitAction<ConsignmentModel, ConsignmentModel> {

  private static final String SOCKET_OUT_CONTEXT = "blFrontBarCodeScanContext";

  @Override
  public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> ctx) {
    this.sendOutput(SOCKET_OUT_CONTEXT, ctx.getData());
    return new ActionResult("success");
  }

  @Override
  public boolean canPerform(final ActionContext<ConsignmentModel> ctx) {
    final ConsignmentModel consignment = ctx.getData();
    return consignment != null && consignment.getOrder() != null;
  }
}
