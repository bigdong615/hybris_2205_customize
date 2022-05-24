package com.bl.core.order.strategy;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.basecommerce.enums.OrderCancelState;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.warehousing.cancellation.strategy.impl.WarehousingOrderCancelStateMappingStrategy;

import java.util.Collection;

public class BlOrderCancelStateMappingStrategy extends WarehousingOrderCancelStateMappingStrategy {


    @Override
    public OrderCancelState getOrderCancelState(OrderModel order) {
        OrderStatus orderStatus = order.getStatus();
        if (!OrderStatus.COMPLETED.equals(orderStatus)) {
            Collection<ConsignmentModel> consignments = order.getConsignments();
            if (consignments != null && !consignments.isEmpty()) {
                return consignments.stream().allMatch((consignment) -> {
                    return consignment.getStatus().equals(ConsignmentStatus.READY) || consignment.getStatus().equals(ConsignmentStatus.READY_FOR_PICKUP) || consignment.getStatus().equals(ConsignmentStatus.READY_FOR_SHIPPING) || consignment.getStatus().equals(ConsignmentStatus.CANCELLED);
                }) ? OrderCancelState.SENTTOWAREHOUSE : this.checkConsignments(consignments);
            } else {
                return OrderCancelState.PENDINGORHOLDINGAREA;
            }
        } else {
            return OrderCancelState.CANCELIMPOSSIBLE;
        }
    }

}
