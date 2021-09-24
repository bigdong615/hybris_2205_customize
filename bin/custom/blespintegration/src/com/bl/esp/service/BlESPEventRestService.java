package com.bl.esp.service;


import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.esp.dto.paymentdeclined.OrderPaymentDeclinedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;


public interface BlESPEventRestService {
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
    ESPEventResponseWrapper sendOrderMoreInfoRequiredEvent(
        final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest);

    /**
     * Send Order Canceled Event by calling Order Canceled ESP Event API
     *
     * @param orderCanceledRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderCanceledEvent(final OrderCanceledEventRequest orderCanceledRequest);

    /**
     * Send Order Payment Declined Event by calling Order Payment Declined ESP Event API
     *
     * @param orderPaymentDeclinedEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderPaymentDeclinedEvent(final OrderPaymentDeclinedEventRequest orderPaymentDeclinedEventRequest);

}
