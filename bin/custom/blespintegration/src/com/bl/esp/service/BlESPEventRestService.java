package com.bl.esp.service;

import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.order.OrderConfirmationRequest;

public interface BlESPEventRestService<T> {
    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderConfirmationRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderConfirmation(OrderConfirmationRequest orderConfirmationRequest);
}
