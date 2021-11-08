package com.bl.core.esp.service;

import com.bl.esp.dto.billpaid.data.OrderBillPaidExtraData;
import com.bl.esp.dto.orderexceptions.data.OrderExceptionsExtraData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordercancel.OrderCancelEntry;
import java.util.List;
import java.util.Map;

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
    public void sendOrderMoreInfoRequiredEvent(final OrderModel orderModel, final String verificationText);

    /**
     * Verify Order by calling Order verification coi needed ESP Event API
     *
     * @param orderModel
     * @param amount
     */
    public void sendOrderVerificationCOIRequiredEvent(final OrderModel orderModel,
        final Double amount);

    /**
     * Send Order Canceled Event by calling Order Canceled ESP Event API
     * @param orderModel
     */
    public void sendOrderCanceledEvent(final OrderModel orderModel);

    /**
     * Send Order Exceptions Event by calling Order Exception Broken/Missing ESP Event API
     * @param orderModel
     * @param orderExceptionsExtraData
     */
    void sendOrderMissingBrokenLateEvent(final OrderModel orderModel, final OrderExceptionsExtraData orderExceptionsExtraData) ;

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

    /**
     *  Send Order Deposit by calling Order Deposit ESP Event API
     * @param orderModel ordermodel
     */
    void sendOrderDepositEvent(final OrderModel orderModel);
    /**
     * Send Order Ready For Pickup Event by calling Order Ready For Pickup ESP Event API
     * @param orderModel
     */
    public void sendOrderReadyForPickupEvent(final OrderModel orderModel);

    /**
     * Send Order New Shipping by calling Order New Shipping ESP Event API
     * @param orderModel
     */
    public void sendOrderNewShippingEvent(final OrderModel orderModel);

    /**
     * Send Order Shipped by calling Order Shipped ESP Event API
     * @param orderModel
     */
    public void sendOrderShippedEvent(final OrderModel orderModel);

    /**
     * Send Order PickedUp by calling Order PickedUp ESP Event API
     * @param orderModel
     */
    public void sendOrderPickedUpEvent(final OrderModel orderModel);

    /**
     *  Send Extend Order by calling Extend OrderESP Event API
     * @param orderModel ordermodel
     */
    void sendExtendOrderEvent(final OrderModel orderModel);


    /**
     *  Send  Extra Order Item  by calling  Order Extra Item ESP Event API
     * @param orderModel ordermodel
     */
     void sendOrderExtraItemsEvent(final OrderModel orderModel) ;

    /**
     * Send Order Refund by calling Order Refund ESP Event API
     * @param orderModel
     * @param totalRefundAmount
     * @param refundMethod
     * @param orderCancelEntries
     */
    public void sendOrderRefundEvent(final OrderModel orderModel,final double totalRefundAmount,final String refundMethod, final List<OrderCancelEntry> orderCancelEntries);

    /**
     *  Send  bill paid  by calling  Order Bill Paid ESP Event API
     * @param orderModel OrderModel
     * @param orderBillPaidExtraData OrderBillPaidExtraData
     */
    void sendOrderBillPaidEvent(final OrderModel orderModel, final OrderBillPaidExtraData orderBillPaidExtraData) ;

    /**
     * It triggers bill paid esp event.
     * @param payBillTotal
     * @param billingChargeTypeMap
     * @param orderModel
     */
    void triggerBillPaidEspEvent(final String payBillTotal,
        final Map<String, List<String>> billingChargeTypeMap, final OrderModel orderModel);

    void sendOrderPullBackItemsAdded(final OrderModel orderModel);
}
