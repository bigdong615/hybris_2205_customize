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
import org.apache.commons.lang.StringUtils;

import javax.annotation.Resource;
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
        return Objects.nonNull(order) && Objects.nonNull(order.getStatus());
    }



    @Override
    public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext) {
        final OrderModel orderModel = actionContext.getData();

        if(isOrderPaymentCaptured(orderModel) || orderStatusAllowed(orderModel))
        {
            notificationService.notifyUser(notificationService.getWidgetNotificationSource(actionContext),
                    ApiregistrybackofficeConstants.NOTIFICATION_TYPE, NotificationEvent.Level.FAILURE,
                    actionContext.getLabel(isOrderPaymentCaptured(orderModel) ? BlCustomCancelRefundConstants.FAILED_TO_REFUND_ORDER_FOR_CAPTURED_ORDER_ERROR
                            : BlCustomCancelRefundConstants.FAILED_TO_REFUND_ORDER_FOR_PENDING_ORDER_ERROR));

        }
        else if(orderModel.getOriginalVersion() == null && orderModel.getVersionID() == null && CollectionUtils.isNotEmpty(
                orderModel.getConsignments()) ) {
            this.sendOutput(SOCKET_OUTPUT_CTX, orderModel);
            return new ActionResult<>(ActionResult.SUCCESS);
        }
        return new ActionResult<>(ActionResult.ERROR);
    }

    /**
     * This method is created to check the order status
     * @param order order
     * @return boolean
     */
    private boolean orderStatusAllowed(final OrderModel order) {
        return StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.PENDING.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.RECEIVED_MANUAL_REVIEW.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.RECEIVED_PAYMENT_DECLINED.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.CANCELLED.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.RECEIVED_SHIPPING_MANUAL_REVIEW.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.RECEIVED_READY_FOR_PICKUP.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.FRAUD_CHECKED.getCode());
    }


    /**
     * This method is created to check the PAYMENT_CAPTURED order status
     * @param order order
     * @return boolean
     */
    private boolean isOrderPaymentCaptured(final OrderModel order) {
        return StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.PAYMENT_CAPTURED.getCode());
    }


}