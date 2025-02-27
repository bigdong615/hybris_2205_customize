package com.bl.core.esp.service;

import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.GiftCardModel;
import com.bl.esp.dto.billpaid.data.OrderBillPaidExtraData;
import com.bl.esp.dto.common.data.ESPEmailCommonRequestData;
import com.bl.esp.dto.email.marketing.data.CustomerMarketingData;
import com.bl.esp.dto.orderexceptions.data.OrderExceptionsExtraData;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordercancel.OrderCancelEntry;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

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

    /**
     * This method created for Order pull back items added ESP Event
     * @param orderModel order model  to get the values
     */
    void sendOrderPullBackItemsAdded(final OrderModel orderModel , final
    AbstractOrderEntryModel abstractOrderEntryModel);

    /**
     * This method created for Order pull back items removed ESP Event
     * @param orderModel order model  to get the values
     */
    void sendOrderPullBackItemsRemoved(final OrderModel orderModel , 	final List<BlSerialProductModel> previousChangedOrderEntriesList);

    /**
     *  Send manual allocation data by calling  order manual allocation ESP Event API
     * @param orderModel OrderModel
     */
    void sendOrderManualAllocationEvent(final OrderModel orderModel) ;

    /**
     * This method created for Order Deposit required ESP Event
     * @param orderModel order model to get the values
     *
     */
     void sendOrderDepositRequired(final OrderModel orderModel , final Double amount);


    /**
     * This method created for Free Gift Card Purchase
     * @param giftCardModel giftCardModel
     */
    void sendFreeGiftCardPurchaseEvent(final GiftCardModel giftCardModel);

    /**
     * This method created for Gift Card Purchase
     * @param giftCardModel giftCardModel
     * @param abstractOrderModel abstractOrderModel
     */
    void sendGiftCardPurchaseEvent(final GiftCardModel giftCardModel , final AtomicReference<AbstractOrderModel> abstractOrderModel);

    /**
     * This method created for reset password request ESP Event.
     * @param espEventCommonRequestData requested password required data.
     */
     void sendForgotPasswordRequest(final ESPEmailCommonRequestData espEventCommonRequestData);

    /**
     * This method created for sending notify me email related esp Event.
     * @param emailRequestData requested email required data.
     */
    void sendNotifyMeConfirmEmailRequest(final ESPEmailCommonRequestData emailRequestData);

    /**
     * This method created for sending back in stock notification email related esp Event.
     * @param emailRequestData requested email required data.
     * @param requestedDate requestedDate
     */
    void sendBackInStockEmailRequest(final ESPEmailCommonRequestData emailRequestData,final Date requestedDate);

    /**
     * Send Order Verification Required by calling Order Verification ESP Event API
     *
     * @param orderModel
     */
     void sendOrderPendingVerificationsEvent(final OrderModel orderModel);
     
     void sendOrderVerificationReminderEvent(final OrderModel orderModel);
    void sendReplacementProductEvent(final OrderModel orderModel,final String oldProductName,final String newProductName,final String customerNotes);
    void sendCustomerMarketingEvent(final String orderCode,final CustomerMarketingData customerMarketingData) ;

    }
