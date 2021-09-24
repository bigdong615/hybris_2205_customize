package com.bl.core.esp.service;

import de.hybris.platform.core.model.order.OrderModel;

public interface BlESPEventService {

    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderModel
     */
    public void sendOrderConfirmation(OrderModel orderModel);

    /**
     * Verify Order by calling Order verification more info ESP Event API
     *
     * @param orderModel
     */
    public void sendOrderMoreInfoRequiredEvent(final OrderModel orderModel);

    /**
     * Verify Order by calling Order verification coi needed ESP Event API
     *
     * @param orderModel
     */
    public void sendOrderVerificationCOIRequiredEvent(final OrderModel orderModel);

    /**
     * Send Order Canceled Event by calling Order Canceled ESP Event API
     * @param orderModel
     */
    public void sendOrderCanceledEvent(final OrderModel orderModel);

    /**
     * Send Order Exceptions Event by calling Order Canceled ESP Event API
     * @param orderModel
     */
    void sendOrderExceptions(final OrderModel orderModel) ;

    /**
     * Send Order Unboxed Event by calling Order Canceled ESP Event API
     * @param orderModel
     */
    void sendOrderUnboxed(final OrderModel orderModel) ;


    /**
     * Send Order Payment Declined Event by calling Order Payment Declined ESP Event API
     * @param orderModel
     */
    public void sendOrderPaymentDeclinedEvent(final OrderModel orderModel);

    /**
     * Send Order Verification Required by calling Order Verification ESP Event API
     *
     * @param orderModel
     */
    public void sendOrderVerificationRequiredEvent(final OrderModel orderModel);

    /**
     * Send Order Verification Completed by calling Order Verification ESP Event API
     *
     * @param orderModel
     */
    public void sendOrderVerificationCompletedEvent(final OrderModel orderModel);
}
