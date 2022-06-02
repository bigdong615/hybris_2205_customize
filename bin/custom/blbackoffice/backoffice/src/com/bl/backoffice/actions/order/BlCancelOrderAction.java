package com.bl.backoffice.actions.order;

import com.bl.backoffice.widget.controller.order.BlCustomCancelRefundConstants;
import com.bl.integration.constants.BlintegrationConstants;
import com.hybris.backoffice.widgets.notificationarea.event.NotificationEvent;
import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import de.hybris.platform.apiregistrybackoffice.constants.ApiregistrybackofficeConstants;
import de.hybris.platform.commerceservices.constants.GeneratedCommerceServicesConstants;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.omsbackoffice.actions.order.cancel.CancelOrderAction;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import com.hybris.cockpitng.util.notifications.NotificationService;

import javax.annotation.Resource;


import java.util.Objects;

public class BlCancelOrderAction extends CancelOrderAction {

    @Resource
    protected transient NotificationService notificationService;

    private static final String SOCKET_OUTPUT_CTX = "customBlCancelOrderContext";


    // return false to disable action button .
    @Override
    public boolean canPerform(ActionContext<OrderModel> actionContext) {
        OrderModel order = (OrderModel)actionContext.getData();
        return Objects.nonNull(order)
                && BooleanUtils.isFalse(orderStatusAllowed(order));
    }


    @Override
    public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext) {
        final OrderModel orderModel = actionContext.getData();
        if(StringUtils.equalsIgnoreCase(orderModel.getStatus().getCode() , OrderStatus.PENDING.getCode()))
        {
            this.sendOutput(SOCKET_OUTPUT_CTX, orderModel);
            return new ActionResult<>(ActionResult.SUCCESS);
        }
        notificationService.notifyUser(notificationService.getWidgetNotificationSource(actionContext),
                ApiregistrybackofficeConstants.NOTIFICATION_TYPE, NotificationEvent.Level.FAILURE, actionContext.getLabel(BlCustomCancelRefundConstants.FAILED_TO_CANCEL_ORDER_FOR_SHIPPEDORDER_ERROR));

        return new ActionResult<>(ActionResult.ERROR);

    }

    private boolean orderStatusAllowed(final OrderModel order) {
        return StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.CANCELLED.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.RECEIVED_MANUAL_REVIEW.getCode())
                || StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.COMPLETED.getCode());
    }

}
