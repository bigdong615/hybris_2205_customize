package com.bl.esp.service.impl;

import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.newshipping.OrderNewShippingEventRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.esp.dto.orderexceptions.OrderExceptionEventRequest;
import com.bl.esp.dto.orderunboxed.OrderUnBoxedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationCOIneededEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationCompletedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationRequiredEventRequest;
import com.bl.esp.dto.paymentdeclined.OrderPaymentDeclinedEventRequest;
import com.bl.esp.dto.readyforpickup.OrderReadyForPickupEventRequest;
import com.bl.esp.order.ESPEventCommonRequest;
import com.bl.esp.service.AbstractESPRestService;
import com.bl.esp.service.BlESPEventRestService;

public class DefaultBlESPEventRestService extends AbstractESPRestService<ESPEventCommonRequest> implements BlESPEventRestService {

   @Override
    public ESPEventResponseWrapper sendOrderConfirmation(
        final OrderConfirmationEventRequest orderConfirmationEventRequest) {

        return super.getTokenAndTriggerEvent(orderConfirmationEventRequest);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ESPEventResponseWrapper sendOrderMoreInfoRequiredEvent(
        final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest) {

        return super.getTokenAndTriggerEvent(orderVerificationMoreInfoEventRequest);
    }

    /**
     * Verify order for coi needed by calling verification_coineeded ESP Event API
     *
     * @param orderVerificationCOIneededEventRequest
     * @return
     */
    @Override
    public ESPEventResponseWrapper sendOrderVerificationCOIRequiredEvent(
        final OrderVerificationCOIneededEventRequest orderVerificationCOIneededEventRequest) {

        return super.getTokenAndTriggerEvent(orderVerificationCOIneededEventRequest);
    }

    @Override
    public ESPEventResponseWrapper sendOrderCanceledEvent(final OrderCanceledEventRequest orderCanceledRequest) {
        return super.getTokenAndTriggerEvent(
            orderCanceledRequest);
    }

    @Override
    public ESPEventResponseWrapper sendOrderPaymentDeclinedEvent(final OrderPaymentDeclinedEventRequest orderPaymentDeclinedEventRequest) {
        return super.getTokenAndTriggerEvent(
            orderPaymentDeclinedEventRequest);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ESPEventResponseWrapper sendOrderException(
        final OrderExceptionEventRequest orderExceptionEventRequest) {
        return super.getTokenAndTriggerEvent(orderExceptionEventRequest);
    }


    public ESPEventResponseWrapper sendOrderVerificationRequiredEvent(
        final OrderVerificationRequiredEventRequest verificationRequiredEventRequest) {
        return super.getTokenAndTriggerEvent(
            verificationRequiredEventRequest);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ESPEventResponseWrapper sendOrderUnboxed(
        final OrderUnBoxedEventRequest orderUnBoxedEventRequest) {
        return super.getTokenAndTriggerEvent(orderUnBoxedEventRequest);
    }

    public ESPEventResponseWrapper sendOrderVerificationCompletedEvent(
        final OrderVerificationCompletedEventRequest verificationCompletedEventRequest) {
        return super.getTokenAndTriggerEvent(
            verificationCompletedEventRequest);
    }

  @Override
  public ESPEventResponseWrapper sendOrderReadyForPickupEvent(final OrderReadyForPickupEventRequest orderReadyForPickupEventRequest) {
    return super.getTokenAndTriggerEvent(
        orderReadyForPickupEventRequest);
  }

  @Override
  public ESPEventResponseWrapper sendOrderNewShippingEvent(final OrderNewShippingEventRequest orderNewShippingEventRequest) {
    return super.getTokenAndTriggerEvent(
        orderNewShippingEventRequest);
  }

}
