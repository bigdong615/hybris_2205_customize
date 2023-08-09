package com.bl.esp.service;


import com.bl.esp.dto.billpaid.OrderBillPaidEventRequest;
import com.bl.esp.dto.billpaid.OrderBillReceiptEventRequest;
import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.common.ESPEmailCommonEventRequest;
import com.bl.esp.dto.depositrequired.OrderDepositRequiredEventRequest;
import com.bl.esp.dto.extraItem.OrderExtraItemRequest;
import com.bl.esp.dto.giftcard.FreeGiftCardPurchaseEventRequest;
import com.bl.esp.dto.giftcard.GiftCardPurchaseEventRequest;
import com.bl.esp.dto.manualallocation.OrderManualAllocationEventRequest;
import com.bl.esp.dto.newshipping.OrderNewShippingEventRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.esp.dto.orderdeposit.OrderDepositRequest;
import com.bl.esp.dto.orderexceptions.OrderExceptionEventRequest;
import com.bl.esp.dto.orderextension.OrderExtensionRequest;
import com.bl.esp.dto.orderpullback.OrderPullBackRequest;
import com.bl.esp.dto.orderunboxed.OrderUnBoxedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationCOIneededEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationCompletedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationRequiredEventRequest;
import com.bl.esp.dto.paymentdeclined.OrderPaymentDeclinedEventRequest;
import com.bl.esp.dto.pickedup.OrderPickedUpEventRequest;
import com.bl.esp.dto.readyforpickup.OrderReadyForPickupEventRequest;
import com.bl.esp.dto.refund.OrderRefundEventRequest;
import com.bl.esp.dto.shipped.OrderShippedEventRequest;


public interface BlESPEventRestService {
    /**
     * Send Order Confirmation by calling Order confirmation ESP Event API
     *
     * @param orderConfirmationEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderConfirmation(
        final OrderConfirmationEventRequest orderConfirmationEventRequest);

    /**
     * Verify order for more info by calling verification_moreinfo ESP Event API
     *
     * @param orderVerificationMoreInfoEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderMoreInfoRequiredEvent(
        final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest);

    /**
     * Verify order for coi needed by calling verification_coineeded ESP Event API
     *
     * @param orderVerificationCOIneededEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderVerificationCOIRequiredEvent(
        final OrderVerificationCOIneededEventRequest orderVerificationCOIneededEventRequest);

    /**
     * Send Order Canceled Event by calling Order Canceled ESP Event API
     *
     * @param orderCanceledRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderCanceledEvent(
        final OrderCanceledEventRequest orderCanceledRequest);


    /**
     * Send Order Payment Declined Event by calling Order Payment Declined ESP Event API
     *
     * @param orderPaymentDeclinedEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderPaymentDeclinedEvent(final OrderPaymentDeclinedEventRequest orderPaymentDeclinedEventRequest);

    /**
     * Send Order Exception Event by calling Order Exception ESP Event API
     *
     * @param orderExceptionEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderException(
        final OrderExceptionEventRequest orderExceptionEventRequest);


    /**
     * Send Order Unboxed Event by calling Order Unboxed ESP Event API
     *
     * @param orderUnBoxedEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderUnboxed(
        final OrderUnBoxedEventRequest orderUnBoxedEventRequest);


    /*
     * Send Order Verification Required by calling Order Verification Required ESP Event API
     *
     * @param verificationRequiredEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderVerificationRequiredEvent(
        final OrderVerificationRequiredEventRequest verificationRequiredEventRequest);

    /**
     * Send Order Verification Completed by calling Order Verification Completed ESP Event API
     *
     * @param verificationCompletedEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderVerificationCompletedEvent(
        final OrderVerificationCompletedEventRequest verificationCompletedEventRequest);



  /**
   * Send Order deposit Event by calling Order Deposit ESP Event API
   * @param  orderDepositRequest
   */
   ESPEventResponseWrapper sendOrderDepositEvent(final OrderDepositRequest orderDepositRequest);
    /**
     * Send Order Ready For pickup Event by calling Order Ready For pickup ESP Event API
     *
     * @param orderReadyForPickupEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderReadyForPickupEvent(final OrderReadyForPickupEventRequest orderReadyForPickupEventRequest);

    /**
     * Send Order New Shipping Event by calling Order New Shipping ESP Event API
     *
     * @param orderNewShippingEventRequest
     * @return
     */
    ESPEventResponseWrapper sendOrderNewShippingEvent(final OrderNewShippingEventRequest orderNewShippingEventRequest);

  /*
   * Send Order Shipped by calling Order Shipped ESP Event API
   * @param orderShippedEventRequest
   * @return ESPEventResponseWrapper object
   */
  ESPEventResponseWrapper sendOrderShippedEvent(
      final OrderShippedEventRequest orderShippedEventRequest);

  /*
   * Send Order PickedUp by calling Order PickedUp ESP Event API
   * @param orderPickedUpEventRequest
   * @return ESPEventResponseWrapper object
   */
  ESPEventResponseWrapper sendOrderPickedUpEvent(
      final OrderPickedUpEventRequest orderPickedUpEventRequest);



  /**
   * Send Extend Order Event by calling Extend Order ESP Event API
   * @param  orderExtensionRequest
   */
  ESPEventResponseWrapper sendExtendOrderEvent(final OrderExtensionRequest orderExtensionRequest);

  /**
   * Send  Order Extra Item Event by calling Order Extra Item ESP Event API
   * @param  orderExtraItemRequest
   */
   ESPEventResponseWrapper sendOrderExtraItemEvent(
      final OrderExtraItemRequest orderExtraItemRequest) ;

  /**
   * Send Order Refund Event by calling Order Refund ESP Event API
   *
   * @param orderRefundEventRequest
   * @return
   */
  ESPEventResponseWrapper sendOrderRefundEvent(final OrderRefundEventRequest orderRefundEventRequest);

  /**
   * Send Order Bill Paid Event by calling Order Bill Paid ESP Event API
   *
   * @param orderBillPaidEventRequest the OrderBillPaidEventRequest
   * @return
   */
  ESPEventResponseWrapper sendOrderBillPaidEvent(final OrderBillPaidEventRequest orderBillPaidEventRequest);

  /**
   * Send Order Pull Back Items Added Event by calling Order Pull Back Items Added ESP Event API
   *
   * @param orderPullBackRequest the orderPullBackRequest
   * @return
   */
  ESPEventResponseWrapper sendOrderPullBackItemsAdded(final OrderPullBackRequest orderPullBackRequest);

  /**
   * Send Order Pull Back Items Removed Event by calling Order Pull Back Items Removed ESP Event API
   *
   * @param orderPullBackRequest the orderPullBackRequest
   * @return
   */
  ESPEventResponseWrapper sendOrderPullBackItemsRemoved(final OrderPullBackRequest orderPullBackRequest);


  /** Send Order Manual Allocation by calling Order Manual Allocation ESP Event API
   *
   * @param orderManualAllocationEventRequest the OrderManualAllocationEventRequest
   * @return ESPEventResponseWrapper
   */
  ESPEventResponseWrapper sendOrderManualAllocationEvent(
      final OrderManualAllocationEventRequest orderManualAllocationEventRequest);

  /**
   * Send Order Deposit Required by calling Order Deposit Required ESP Event API
   * @param orderDepositRequiredEventRequest order deposit request
   * @return ESPEventResponseWrapper
   */
  ESPEventResponseWrapper sendOrderDepositRequired(
      final OrderDepositRequiredEventRequest orderDepositRequiredEventRequest);

  /**
   * Send Gift Card Purchase by calling Gift Card Purchase ESP Event API
   * @param giftCardPurchaseEventRequest giftCardPurchaseEventRequest
   * @return ESPEventResponseWrapper
   */
  ESPEventResponseWrapper sendGiftCardPurchase(
      final GiftCardPurchaseEventRequest giftCardPurchaseEventRequest);

  /**
   * Send Gift Card Purchase by calling Gift Card Purchase ESP Event API
   * @param freeGiftCardPurchaseEventRequest freeGiftCardPurchaseEventRequest
   * @return ESPEventResponseWrapper
   */
  ESPEventResponseWrapper sendFreeGiftCardPurchase(
      final FreeGiftCardPurchaseEventRequest freeGiftCardPurchaseEventRequest);

   /**
   * BL-1813,1814: send email request ESP Event API
   * @param  emailRequiredEventRequest emailRequiredEventRequest
   * @return espEventResponseWrapper
   */
  ESPEventResponseWrapper sendESPEmailEventRequest(
      ESPEmailCommonEventRequest emailRequiredEventRequest);

  ESPEventResponseWrapper sendBillPaidESP( final OrderBillReceiptEventRequest orderBillReceiptEventRequest);
}
