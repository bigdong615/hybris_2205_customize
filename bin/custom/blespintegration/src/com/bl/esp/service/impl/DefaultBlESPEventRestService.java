package com.bl.esp.service.impl;

import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;
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

    @Override
    public ESPEventResponseWrapper sendOrderCanceledEvent(
        OrderCanceledEventRequest orderCanceledRequest) {

        return super.getTokenAndTriggerEvent(orderCanceledRequest);
    }

 }
