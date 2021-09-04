package com.bl.core.services.esp.order;

import com.bl.core.services.esp.models.OrderConfirmationRequest;
import com.bl.core.services.esp.models.OrderConfirmationResponse;
import com.bl.core.services.esp.models.OrderConfirmationResponseWrapper;

public interface BlEspEventService {
    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderConfirmationRequest
     * @return
     */
    OrderConfirmationResponseWrapper sendOrderConfirmation(OrderConfirmationRequest orderConfirmationRequest);
}
