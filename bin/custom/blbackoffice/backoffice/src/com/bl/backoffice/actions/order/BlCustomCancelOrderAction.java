package com.bl.backoffice.actions.order;

import com.hybris.cockpitng.actions.ActionContext;
import com.hybris.cockpitng.actions.ActionResult;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.omsbackoffice.actions.order.cancel.CancelOrderAction;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;

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
    private static ArrayList<String> arrayList = new ArrayList<String>(Collections.singleton(OrderStatus.CANCELLED.getCode()));

    @Override
    public boolean canPerform(final ActionContext<OrderModel> actionContext) {
        final OrderModel order = (OrderModel)actionContext.getData();
        return Objects.nonNull(order) && Objects.nonNull(order.getStatus()) && BooleanUtils.isTrue(orderStatusAllowed(order));
    }



    @Override
    public ActionResult<OrderModel> perform(final ActionContext<OrderModel> actionContext) {
        final OrderModel orderModel = actionContext.getData();
        if (orderModel.getOriginalVersion() == null && orderModel.getVersionID() == null && CollectionUtils.isNotEmpty(
                orderModel.getConsignments())) {
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
        return StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.UNBOXED_PARTIALLY.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.UNBOXED_COMPLETELY.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.LATE.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.INCOMPLETE.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.INCOMPLETE_BALANCE_DUE.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.INCOMPLETE_ITEMS_IN_REPAIR.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.INCOMPLETE_LOST_IN_TRANSIT.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.INCOMPLETE_MISSING_AND_BROKEN_ITEMS.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.INCOMPLETE_MISSING_ITEMS.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.INCOMPLETE_RECOVERED.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.INCOMPLETE_STOLEN.getCode()) ||
                StringUtils.equalsIgnoreCase(order.getStatus().getCode() , OrderStatus.SHIPPED.getCode());
    }



}