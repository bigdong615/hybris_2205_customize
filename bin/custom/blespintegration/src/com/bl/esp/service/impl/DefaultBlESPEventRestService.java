package com.bl.esp.service.impl;

import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;

import com.bl.esp.dto.paymentdeclined.OrderPaymentDeclinedEventRequest;
import com.bl.esp.order.ESPEventCommonRequest;
import com.bl.esp.service.AbstractESPRestService;
import com.bl.esp.service.BlESPEventRestService;

public class DefaultBlESPEventRestService extends AbstractESPRestService<ESPEventCommonRequest> implements BlESPEventRestService {


    @Override
    public ESPEventResponseWrapper sendOrderConfirmation(final OrderConfirmationEventRequest orderConfirmationEventRequest) {
        final ESPEventResponseWrapper espEventResponseWrapper = super.getTokenAndTriggerEvent(
            orderConfirmationEventRequest);
        return espEventResponseWrapper;
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

}
