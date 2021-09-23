package com.bl.core.esp.service;

import de.hybris.platform.core.model.order.OrderModel;

public interface BlESPEventService {

    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderModel
     */
    public void sendOrderConfirmation(OrderModel orderModel);


    /**
     * Verify Order by calling Order verification more info ESP Event API
     *
     * @param orderModel
     */
    public void verifyOrderForMoreInfo(final OrderModel orderModel);
}
