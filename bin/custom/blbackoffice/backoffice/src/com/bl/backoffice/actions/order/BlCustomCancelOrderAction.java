package com.bl.backoffice.actions.order;

import com.bl.backoffice.widget.controller.order.BlCustomCancelRefundConstants;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import com.hybris.cockpitng.util.notifications.NotificationService;
import de.hybris.platform.apiregistrybackoffice.constants.ApiregistrybackofficeConstants;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.omsbackoffice.actions.order.cancel.CancelOrderAction;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Objects;

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
    @Resource
    protected transient NotificationService notificationService;

    @Override
    public boolean canPerform(final ActionContext<OrderModel> actionContext) {
        final OrderModel order = (OrderModel)actionContext.getData();
        return Objects.nonNull(order) && Objects.nonNull(order.getStatus()) && BooleanUtils.isFalse(orderStatusAllowed(order));
    }



    @Override
    public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext) {
        final OrderModel orderModel = actionContext.getData();abels_en.properties

        if(StringUtils.equalsIgnoreCase(OrderStatus.PENDING.getCode() , orderModel.getStatus().getCode())
                || StringUtils.equalsIgnoreCase(OrderStatus.COMPLETED.getCode() , orderModel.getStatus().getCode()))
        {
            notificationService.notifyUser(notificationService.getWidgetNotificationSource(actionContext),
                    ApiregistrybackofficeConstants.NOTIFICATION_TYPE, NotificationEvent.Level.FAILURE, actionContext.getLabel(BlCustomCancelRefundConstants.FAILED_TO_REFUND_ORDER_FOR_PENDING_ORDER_ERROR));

        }
        else if(orderModel.getOriginalVersion() == null && orderModel.getVersionID() == null && CollectionUtils.isNotEmpty(
                orderModel.getConsignments()) ) {
            this.sendOutput(SOCKET_OUTPUT_CTX, orderModel);
            return new ActionResult<>(ActionResult.SUCCESS);
        }
        return new ActionResult<>(ActionResult.ERROR);
    }

    private boolean checkIsOrderFullyPlacedWithGiftCard(final OrderModel order) {
        return Double.compare(order.getTotalPrice(), 0.0) == 0 && Double.compare(order.getGiftCardAmount() , 0.0) >0;
    }

    /**
     * This method created check order status
     * @param order order
     * @return boolean
     */
    private boolean orderStatusAllowed(final OrderModel order) {
        return StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.RECEIVED_MANUAL_REVIEW.getCode());
    }



}