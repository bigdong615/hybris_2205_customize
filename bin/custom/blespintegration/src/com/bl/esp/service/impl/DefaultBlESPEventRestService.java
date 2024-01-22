package com.bl.esp.service.impl;

import com.bl.esp.dto.billpaid.OrderBillPaidEventRequest;
import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.common.ESPEmailCommonEventRequest;
import com.bl.esp.dto.depositrequired.OrderDepositRequiredEventRequest;
import com.bl.esp.dto.email.marketing.CustomerMarketingEventRequest;
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
import com.bl.esp.dto.pendingverification.PendingVerificationEventRequest;
import com.bl.esp.dto.pendingverification.VerificationReminderEventRequest;
import com.bl.esp.dto.pickedup.OrderPickedUpEventRequest;
import com.bl.esp.dto.product.replacement.ProductReplacementEventRequest;
import com.bl.esp.dto.readyforpickup.OrderReadyForPickupEventRequest;
import com.bl.esp.dto.refund.OrderRefundEventRequest;
import com.bl.esp.dto.shipped.OrderShippedEventRequest;
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

    /**
     * Verify order for coi needed by calling verification_coineeded ESP Event API
     *
     * @param orderVerificationCOIneededEventRequest
     * @return
     */
    @Override
    public ESPEventResponseWrapper sendOrderVerificationCOIRequiredEvent(
        final OrderVerificationCOIneededEventRequest orderVerificationCOIneededEventRequest) {

        return super.getTokenAndTriggerEvent(orderVerificationCOIneededEventRequest);
    }

    @Override
    public ESPEventResponseWrapper sendOrderCanceledEvent(final OrderCanceledEventRequest orderCanceledRequest) {
        return super.getTokenAndTriggerEvent(
            orderCanceledRequest);
    }

    @Override
    public ESPEventResponseWrapper sendOrderPaymentDeclinedEvent(final OrderPaymentDeclinedEventRequest orderPaymentDeclinedEventRequest) {
        return super.getTokenAndTriggerEvent(
            orderPaymentDeclinedEventRequest);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ESPEventResponseWrapper sendOrderException(
        final OrderExceptionEventRequest orderExceptionEventRequest) {
        return super.getTokenAndTriggerEvent(orderExceptionEventRequest);
    }


    public ESPEventResponseWrapper sendOrderVerificationRequiredEvent(
        final OrderVerificationRequiredEventRequest verificationRequiredEventRequest) {
        return super.getTokenAndTriggerEvent(
            verificationRequiredEventRequest);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ESPEventResponseWrapper sendOrderUnboxed(
        final OrderUnBoxedEventRequest orderUnBoxedEventRequest) {
        return super.getTokenAndTriggerEvent(orderUnBoxedEventRequest);
    }

    public ESPEventResponseWrapper sendOrderVerificationCompletedEvent(
        final OrderVerificationCompletedEventRequest verificationCompletedEventRequest) {
        return super.getTokenAndTriggerEvent(
            verificationCompletedEventRequest);
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public ESPEventResponseWrapper sendOrderDepositEvent(final OrderDepositRequest orderDepositRequest) {
        return super.getTokenAndTriggerEvent(orderDepositRequest);
    }

  @Override
  public ESPEventResponseWrapper sendOrderReadyForPickupEvent(final OrderReadyForPickupEventRequest orderReadyForPickupEventRequest) {
    return super.getTokenAndTriggerEvent(
        orderReadyForPickupEventRequest);
  }

  @Override
  public ESPEventResponseWrapper sendOrderNewShippingEvent(final OrderNewShippingEventRequest orderNewShippingEventRequest) {
    return super.getTokenAndTriggerEvent(
        orderNewShippingEventRequest);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderShippedEvent(
      final OrderShippedEventRequest orderShippedEventRequest) {
    return super.getTokenAndTriggerEvent(
        orderShippedEventRequest);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderPickedUpEvent(
      final OrderPickedUpEventRequest orderPickedUpEventRequest) {
    return super.getTokenAndTriggerEvent(
        orderPickedUpEventRequest);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendExtendOrderEvent(final OrderExtensionRequest orderExtensionRequest) {
    return super.getTokenAndTriggerEvent(orderExtensionRequest);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderRefundEvent(
      OrderRefundEventRequest orderRefundEventRequest) {
    return super.getTokenAndTriggerEvent(orderRefundEventRequest);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderBillPaidEvent(
      final OrderBillPaidEventRequest orderBillPaidEventRequest) {
    return super.getTokenAndTriggerEvent(orderBillPaidEventRequest);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderManualAllocationEvent(
      final OrderManualAllocationEventRequest orderManualAllocationEventRequest) {
    return super.getTokenAndTriggerEvent(orderManualAllocationEventRequest);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderExtraItemEvent(
      final OrderExtraItemRequest orderExtraItemRequest) {

    return super.getTokenAndTriggerEvent(orderExtraItemRequest);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderPullBackItemsAdded(
      final OrderPullBackRequest orderPullBackRequest) {

    return super.getTokenAndTriggerEvent(orderPullBackRequest);
  }



  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderPullBackItemsRemoved(
      final OrderPullBackRequest orderPullBackRequest) {

    return super.getTokenAndTriggerEvent(orderPullBackRequest);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendOrderDepositRequired(
      final OrderDepositRequiredEventRequest orderDepositRequiredEventRequest) {

    return super.getTokenAndTriggerEvent(orderDepositRequiredEventRequest);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendGiftCardPurchase(
      final GiftCardPurchaseEventRequest giftCardPurchaseEventRequest) {

    return super.getTokenAndTriggerEvent(giftCardPurchaseEventRequest);
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public ESPEventResponseWrapper sendFreeGiftCardPurchase(
      final FreeGiftCardPurchaseEventRequest freeGiftCardPurchaseEventRequest) {

    return super.getTokenAndTriggerEvent(freeGiftCardPurchaseEventRequest);
  }

  public ESPEventResponseWrapper sendESPEmailEventRequest(
      ESPEmailCommonEventRequest emailRequiredEventRequest){
    return super.getTokenAndTriggerEvent(emailRequiredEventRequest);
  }
  
    public ESPEventResponseWrapper sendOrderPendingVerification(PendingVerificationEventRequest pendingVerificationEventRequest) {
        return super.getTokenAndTriggerEvent(pendingVerificationEventRequest);
    }
    
    @Override
    public ESPEventResponseWrapper sendOrderVerificationReminder(VerificationReminderEventRequest verificationReminderEventRequest) {
        return super.getTokenAndTriggerEvent(verificationReminderEventRequest);
    }
    @Override
    public ESPEventResponseWrapper sendProductReplacementEsp(ProductReplacementEventRequest productReplacementEventRequest) {
        return super.getTokenAndTriggerEvent(productReplacementEventRequest);
    }
    @Override
   public ESPEventResponseWrapper sendCustomerMarketingESP(final CustomerMarketingEventRequest customerMarketingEventRequest){
        return super.getTokenAndTriggerEvent(customerMarketingEventRequest);
    }
}
