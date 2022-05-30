/**
 *
 */
package com.bl.backoffice.actions;

import com.bl.core.constants.GeneratedBlCoreConstants;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.actions.CockpitAction;
import com.hybris.cockpitng.engine.impl.AbstractComponentWidgetAdapterAware;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * ###################### BL-749 ################
 * The type Capture payment action. This class is being used to enable/disable the action
 * button on the editor area to capture the payment for the selected consignment.
 *
 * @author Krishan Vashishth
 */
public class CapturePaymentAction extends AbstractComponentWidgetAdapterAware
    implements CockpitAction<ConsignmentModel, ConsignmentModel>
{
  private static final String SOCKET_OUT_CONTEXT = "blCapturePaymentContext";
  private static final String LOCATION_PATH = "actions/blFrontCapturePayment";
  //private static final String LOCATION_PATH = "/widgetClasspathResource/widgets/actions/blFrontCapturePayment";
  private static final String FRONT_SOCKET_OUT_CTX = "blFrontCapturePaymentContext";

  /**
   * Can perform boolean.
   *
   * @param actionContext the action context
   * @return the boolean
   */
  @Override
  public boolean canPerform(final ActionContext<ConsignmentModel> actionContext)
  {
    final ConsignmentModel consignmentModel = actionContext.getData();
    return consignmentModel != null && consignmentModel.getOrder() != null
        && (BooleanUtils.isFalse(StringUtils.equalsIgnoreCase(consignmentModel.getStatus().getCode() , ConsignmentStatus.CANCELLED.getCode())) &&
            BooleanUtils.isFalse(StringUtils.equalsIgnoreCase(consignmentModel.getOrder().getStatus().getCode() , OrderStatus.CANCELLED.getCode())));
  }

  /**
   * Perform action result.
   *
   * @param actionContext the action context
   * @return the action result
   */
  @Override
  public ActionResult<ConsignmentModel> perform(final ActionContext<ConsignmentModel> actionContext)
  {
    final String socketOuptut = actionContext.getDefinition().getLocationPath()
        .contains(LOCATION_PATH) ? FRONT_SOCKET_OUT_CTX : SOCKET_OUT_CONTEXT;
    this.sendOutput(socketOuptut, actionContext.getData());
    return new ActionResult(ActionResult.SUCCESS);
  }


}
