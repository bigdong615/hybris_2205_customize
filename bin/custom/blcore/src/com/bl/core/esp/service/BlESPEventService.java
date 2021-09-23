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
     * Send Order Canceled Event by calling Order Canceled ESP Event API
     * @param orderModel
     */
    public void sendOrderCanceledEvent(final OrderModel orderModel);
}
