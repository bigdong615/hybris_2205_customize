package com.bl.core.assigningTrackingNumber;

import de.hybris.platform.core.model.order.OrderModel;

import java.util.List;

/**
 * This Interface assigns the Tracking Number to Consignment if the order status is shipped.
 *
 * @author Sunil
 */
public interface AssigningTrackingNumberService {

    /**
     * @param
     * @return list of orders
     */
    public List<OrderModel> assigningTracking();
}
