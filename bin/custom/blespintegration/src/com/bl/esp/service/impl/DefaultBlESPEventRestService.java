package com.bl.esp.service.impl;

import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;

import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;
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

    /**
     * {@inheritDoc}
     */
    @Override
    public ESPEventResponseWrapper verifyOrderForMoreInfo(
        final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest) {

        return super.getTokenAndTriggerEvent(
            orderVerificationMoreInfoEventRequest);
    }
}
