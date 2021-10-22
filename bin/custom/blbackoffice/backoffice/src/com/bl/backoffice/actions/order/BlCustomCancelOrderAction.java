package com.bl.backoffice.actions.order;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.omsbackoffice.actions.order.cancel.CancelOrderAction;
import org.apache.commons.collections4.CollectionUtils;

/**
 * ################  BL-986 #######################
 * This class is being extended to the OOTB CancelOrderAction.
 * As we want to refund the amount if the order is cancelled before the delivery to the customer,
 * that's why this custom class is required extending the logic.
 *
 * @author Krishan Vashishth
 */
public class BlCustomCancelOrderAction extends CancelOrderAction {

    private static final String SOCKET_OUTPUT_CTX = "customCancelOrderContext";

    @Override
    public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext) {
        final OrderModel orderModel = actionContext.getData();
        if (orderModel.getOriginalVersion() == null && orderModel.getVersionID() == null && CollectionUtils.isNotEmpty(orderModel.getConsignments())) {
            this.sendOutput(SOCKET_OUTPUT_CTX, orderModel);
            return new ActionResult<>(ActionResult.SUCCESS);
        }
        return new ActionResult<>(ActionResult.ERROR);
    }

}