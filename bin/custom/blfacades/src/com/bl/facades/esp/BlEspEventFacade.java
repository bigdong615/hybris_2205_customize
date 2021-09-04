package com.bl.facades.esp;

import de.hybris.platform.core.model.order.OrderModel;

public interface BlEspEventFacade {

    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderModel
     */
    public void sendOrderConfirmation(OrderModel orderModel);

}
