package com.bl.esp.service.impl;

import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;

import com.bl.esp.service.AbstractESPRestService;
import com.bl.esp.service.BlESPEventRestService;

public class DefaultBlESPEventRestService extends AbstractESPRestService<OrderConfirmationEventRequest> implements BlESPEventRestService {


    @Override
    public ESPEventResponseWrapper sendOrderConfirmation(final OrderConfirmationEventRequest orderConfirmationEventRequest) {

        final ESPEventResponseWrapper espEventResponseWrapper = super.getTokenAndTriggerEvent(
            orderConfirmationEventRequest);
        return espEventResponseWrapper;
    }
}
