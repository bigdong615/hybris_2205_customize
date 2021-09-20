package com.bl.esp.service;

import com.bl.esp.dto.orderconfirmation.OrderConfirmationRequest;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationResponseWrapper;

public interface BlESPEventRestService {
    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderConfirmationRequest
     * @return
     */
    OrderConfirmationResponseWrapper sendOrderConfirmation(OrderConfirmationRequest orderConfirmationRequest);
}
