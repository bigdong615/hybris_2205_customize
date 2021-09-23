package com.bl.esp.service;

import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;


public interface BlESPEventRestService<T> {
    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderConfirmationEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderConfirmation(
        final OrderConfirmationEventRequest orderConfirmationEventRequest);
}
