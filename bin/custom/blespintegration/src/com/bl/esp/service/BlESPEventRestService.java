package com.bl.esp.service;

import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;


public interface BlESPEventRestService<T> {
    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderConfirmationEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderConfirmation(
        final OrderConfirmationEventRequest orderConfirmationEventRequest);

    /**
     * Verify order for more info by calling verification_moreinfo ESP Event API
     *
     * @param orderVerificationMoreInfoEventRequest
     * @return
     */
    ESPEventResponseWrapper verifyOrderForMoreInfo(
        final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest);
}
