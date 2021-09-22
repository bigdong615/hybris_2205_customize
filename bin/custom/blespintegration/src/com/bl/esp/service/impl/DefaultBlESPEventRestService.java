package com.bl.esp.service.impl;

import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.order.OrderConfirmationRequest;
import com.bl.esp.service.AbstractESPRestService;
import com.bl.esp.service.BlESPEventRestService;

public class DefaultBlESPEventRestService extends AbstractESPRestService<OrderConfirmationRequest> implements BlESPEventRestService {


    @Override
    public ESPEventResponseWrapper sendOrderConfirmation(final OrderConfirmationRequest orderConfirmationRequest) {

        final ESPEventResponseWrapper espEventResponseWrapper = super.getTokenAndTriggerEvent(orderConfirmationRequest);
        return espEventResponseWrapper;
    }
}
