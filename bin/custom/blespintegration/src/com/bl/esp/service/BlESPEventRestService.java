package com.bl.esp.service;


import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.esp.dto.orderexceptions.OrderExceptionEventRequest;
import com.bl.esp.dto.orderunboxed.OrderUnBoxedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationCOIneededEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationCompletedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationRequiredEventRequest;
import com.bl.esp.dto.paymentdeclined.OrderPaymentDeclinedEventRequest;


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
     * Verify order for coi needed by calling verification_coineeded ESP Event API
     *
     * @param orderVerificationCOIneededEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderVerificationCOIRequiredEvent(
        final OrderVerificationCOIneededEventRequest orderVerificationCOIneededEventRequest);

    /**
     * Send Order Canceled Event by calling Order Canceled ESP Event API
     *
     * @param orderCanceledRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderCanceledEvent(
        final OrderCanceledEventRequest orderCanceledRequest);


    /**
     * Send Order Payment Declined Event by calling Order Payment Declined ESP Event API
     *
     * @param orderPaymentDeclinedEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderPaymentDeclinedEvent(final OrderPaymentDeclinedEventRequest orderPaymentDeclinedEventRequest);

    /**
     * Send Order Exception Event by calling Order Exception ESP Event API
     *
     * @param orderExceptionEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderException(
        final OrderExceptionEventRequest orderExceptionEventRequest);


    /**
     * Send Order Unboxed Event by calling Order Unboxed ESP Event API
     *
     * @param orderUnBoxedEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderUnboxed(
        final OrderUnBoxedEventRequest orderUnBoxedEventRequest);


    /*
     * Send Order Verification Required by calling Order Verification Required ESP Event API
     *
     * @param verificationRequiredEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderVerificationRequiredEvent(
        final OrderVerificationRequiredEventRequest verificationRequiredEventRequest);

    /**
     * Send Order Verification Completed by calling Order Verification Completed ESP Event API
     *
     * @param verificationCompletedEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderVerificationCompletedEvent(
        final OrderVerificationCompletedEventRequest verificationCompletedEventRequest);
}
