package com.bl.core.esp.service.impl;


import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordercancel.OrderCancelEntry;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.util.Utilities;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.esp.populators.BlESPEmailCommonRequestPopulator;
import com.bl.core.esp.populators.BlExtendOrderRequestPopulator;
import com.bl.core.esp.populators.BlExtraItemRequestPopulator;
import com.bl.core.esp.populators.BlFreeGiftCardPurchaseEventPopulator;
import com.bl.core.esp.populators.BlOrderBillPaidRequestPopulator;
import com.bl.core.esp.populators.BlOrderCanceledRequestPopulator;
import com.bl.core.esp.populators.BlOrderConfirmationRequestPopulator;
import com.bl.core.esp.populators.BlOrderDepositRequestPopulator;
import com.bl.core.esp.populators.BlOrderDepositRequiredRequestPopulator;
import com.bl.core.esp.populators.BlOrderExceptionsRequestPopulator;
import com.bl.core.esp.populators.BlOrderGiftCardPurchaseEventPopulator;
import com.bl.core.esp.populators.BlOrderManualAllocationRequestPopulator;
import com.bl.core.esp.populators.BlOrderNewShippingRequestPopulator;
import com.bl.core.esp.populators.BlOrderPaymentDeclinedRequestPopulator;
import com.bl.core.esp.populators.BlOrderPickedUpRequestPopulator;
import com.bl.core.esp.populators.BlOrderPullBackItemRemovedRequestPopulator;
import com.bl.core.esp.populators.BlOrderPullBackItemsAddedRequestPopulator;
import com.bl.core.esp.populators.BlOrderReadyForPickupRequestPopulator;
import com.bl.core.esp.populators.BlOrderRefundRequestPopulator;
import com.bl.core.esp.populators.BlOrderShippedRequestPopulator;
import com.bl.core.esp.populators.BlOrderUnboxedRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationCOIneededRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationCompletedRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationMoreInfoRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationRequiredRequestPopulator;
import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.BlStoredEspEventModel;
import com.bl.core.model.GiftCardModel;
import com.bl.esp.dto.billpaid.OrderBillPaidEventRequest;
import com.bl.esp.dto.billpaid.data.OrderBillPaidExtraData;
import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.common.ESPEmailCommonEventRequest;
import com.bl.esp.dto.common.data.ESPEmailCommonRequestData;
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
import com.bl.esp.dto.orderexceptions.data.OrderExceptionsExtraData;
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
import com.bl.esp.dto.refund.data.OrderRefundData;
import com.bl.esp.dto.shipped.OrderShippedEventRequest;
import com.bl.esp.enums.ESPEventStatus;
import com.bl.esp.enums.EspEventTypeEnum;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.esp.service.BlESPEventRestService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;

/**
 * This Common class used for preparing request and resposne for ESP Events
 * @author Manikandan
 */
public class DefaultBlESPEventService implements BlESPEventService {

    private static final Logger LOG = Logger.getLogger(DefaultBlESPEventService.class);
    private BlOrderConfirmationRequestPopulator blOrderConfirmationRequestPopulator;
    private BlOrderVerificationMoreInfoRequestPopulator blOrderVerificationMoreInfoRequestPopulator;
    private BlOrderVerificationCOIneededRequestPopulator blOrderVerificationCOIneededRequestPopulator;
    private BlOrderReadyForPickupRequestPopulator blOrderReadyForPickupRequestPopulator;
    private BlOrderNewShippingRequestPopulator blOrderNewShippingRequestPopulator;
    private BlOrderCanceledRequestPopulator blOrderCanceledRequestPopulator;
    private BlOrderExceptionsRequestPopulator blOrderExceptionsRequestPopulator;
    private BlOrderUnboxedRequestPopulator blOrderUnboxedRequestPopulator;
    private BlOrderPaymentDeclinedRequestPopulator blOrderPaymentDeclinedRequestPopulator;
    private BlOrderVerificationRequiredRequestPopulator blOrderVerificationRequiredRequestPopulator;
    private BlOrderVerificationCompletedRequestPopulator blOrderVerificationCompletedRequestPopulator;
    private BlOrderDepositRequestPopulator blOrderDepositRequestPopulator;
    private BlOrderShippedRequestPopulator blOrderShippedRequestPopulator;
    private BlOrderPickedUpRequestPopulator blOrderPickedUpRequestPopulator;
    private BlExtendOrderRequestPopulator blExtendOrderRequestPopulator;
    private BlExtraItemRequestPopulator blExtraItemRequestPopulator;
    private BlOrderRefundRequestPopulator blOrderRefundRequestPopulator;
    private BlESPEventRestService blESPEventRestService;
    private BlOrderBillPaidRequestPopulator blOrderBillPaidRequestPopulator;
    private BlOrderPullBackItemsAddedRequestPopulator blOrderPullBackItemsAddedRequestPopulator;
    private BlOrderDepositRequiredRequestPopulator blOrderDepositRequiredRequestPopulator;
    private BlOrderPullBackItemRemovedRequestPopulator blOrderPullBackItemRemovedRequestPopulator;
    private ModelService modelService;
    private BlOrderManualAllocationRequestPopulator blOrderManualAllocationRequestPopulator;
    private BlOrderGiftCardPurchaseEventPopulator blOrderGiftCardPurchaseEventPopulator;
    private BlFreeGiftCardPurchaseEventPopulator blFreeGiftCardPurchaseEventPopulator;
    private BlESPEmailCommonRequestPopulator blESPEmailCommonRequestPopulator;
    @Value("${back.in.stock.email.request.event.template.key}")
    private String backInStockTemplate;
    /**
     * This method created to prepare the request and response from ESP service
     * @param orderModel ordermodel
     */
    @Override
    public void sendOrderConfirmation(final OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderConfirmationEventRequest orderConfirmationEventRequest = new OrderConfirmationEventRequest();
            getBlOrderConfirmationRequestPopulator().populate(orderModel,
                orderConfirmationEventRequest);
            ESPEventResponseWrapper espEventResponseWrapper = null;
            try
            {
                // Call send order confirmation ESP Event API
                espEventResponseWrapper = getBlESPEventRestService().sendOrderConfirmation(
                    orderConfirmationEventRequest);
            }catch (final BlESPIntegrationException exception){
                persistESPEventDetail(null, EspEventTypeEnum.ORDER_CONFIRM,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
            }
            // Save send order confirmation ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_CONFIRM,orderModel.getCode(),null, null);
        }
    }

    /**
     * Verify Order by calling Order verification more info ESP Event API
     *
     * @param orderModel
     */
    @Override
    public void sendOrderMoreInfoRequiredEvent(final OrderModel orderModel, final String verificationText) {
        if (Objects.nonNull(orderModel)) {
            final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest = new OrderVerificationMoreInfoEventRequest();
            getBlOrderVerificationMoreInfoRequestPopulator().populate(orderModel,
                orderVerificationMoreInfoEventRequest);
          orderVerificationMoreInfoEventRequest.getData().setVerificationtext(verificationText);
            ESPEventResponseWrapper espEventResponseWrapper = null;
            try
            {
                // Call send order verification more info ESP Event API
                espEventResponseWrapper = getBlESPEventRestService().sendOrderMoreInfoRequiredEvent(
                    orderVerificationMoreInfoEventRequest);
            }catch (final BlESPIntegrationException exception){
                persistESPEventDetail(null, EspEventTypeEnum.VERIFICATION_MOREINFO,
                    orderModel.getCode(), exception.getMessage(), exception.getRequestString());
            }
            // Save send order verification more info ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.VERIFICATION_MOREINFO,
                orderModel.getCode(), null, null);
        }
    }

    /**
     * Verify Order by calling Order verification coi needed ESP Event API
     *
     * @param orderModel
     */
    @Override
    public void sendOrderVerificationCOIRequiredEvent(final OrderModel orderModel, final Double amount) {
        if (Objects.nonNull(orderModel)) {
          orderModel.setCoiAmount(BigDecimal.valueOf(amount));
          getModelService().save(orderModel);
          getModelService().refresh(orderModel);
          BlLogger.logFormattedMessage(LOG , Level.DEBUG , "COI amount {} deposited for orderCode {}" , String.valueOf(amount) , orderModel.getCode());
          final OrderVerificationCOIneededEventRequest orderVerificationCOIneededEventRequest = new OrderVerificationCOIneededEventRequest();
            getBlOrderVerificationCOIneededRequestPopulator().populate(orderModel,
                orderVerificationCOIneededEventRequest);
          orderVerificationCOIneededEventRequest.getData().setCoiamount(formatAmount(amount));

            ESPEventResponseWrapper espEventResponseWrapper = null;
            try
            {
                // Call send order verification more info ESP Event API
                espEventResponseWrapper = getBlESPEventRestService().sendOrderVerificationCOIRequiredEvent(
                    orderVerificationCOIneededEventRequest);
            }catch (final BlESPIntegrationException exception){
                persistESPEventDetail(null, EspEventTypeEnum.VERIFICATION_COINEEDED,
                    orderModel.getCode(), exception.getMessage(), exception.getRequestString());
            }
            // Save send order verification more info ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.VERIFICATION_COINEEDED,
                orderModel.getCode(), null, null);
        }
    }

    /**
     * This method created to prepare the request and response from ESP service
     * @param orderModel ordermodel
     */
    @Override
    public void sendOrderCanceledEvent(final OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderCanceledEventRequest orderCanceledEventRequest = new OrderCanceledEventRequest();
            getBlOrderCanceledRequestPopulator().populate(orderModel,orderCanceledEventRequest);
            ESPEventResponseWrapper espEventResponseWrapper = null;
            try
            {
                // Call send order Canceled ESP Event API
                espEventResponseWrapper = getBlESPEventRestService().sendOrderCanceledEvent(orderCanceledEventRequest);
            }catch (final BlESPIntegrationException exception){
                persistESPEventDetail(null, EspEventTypeEnum.ORDER_CANCELED, orderModel.getCode(),
                    exception.getMessage(), exception.getRequestString());
            }
            // Save send order Canceled ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_CANCELED,
                orderModel.getCode(), null, null);

        }
    }
    /**
     * Send Order Payment Declined Event by calling Order Payment Declined ESP Event API
     * @param orderModel
     */
    @Override
    public void sendOrderPaymentDeclinedEvent(final OrderModel orderModel) {
            if (Objects.nonNull(orderModel)) {
                final OrderPaymentDeclinedEventRequest orderPaymentDeclinedEventRequest = new OrderPaymentDeclinedEventRequest();
                getBlOrderPaymentDeclinedRequestPopulator().populate(orderModel, orderPaymentDeclinedEventRequest);
                ESPEventResponseWrapper espEventResponseWrapper = null;
                try
                {
                    // Call send order Payment Declined ESP Event API
                   espEventResponseWrapper = getBlESPEventRestService().sendOrderPaymentDeclinedEvent(orderPaymentDeclinedEventRequest);
                }catch (final BlESPIntegrationException exception){
                    persistESPEventDetail(null, EspEventTypeEnum.ORDER_PAYMENTDECLINED,orderModel.getCode(), exception.getMessage(),exception.getRequestString());
                }
                // Save send order Payment Declined ESP Event Detail
                persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_PAYMENTDECLINED,orderModel.getCode(),null,null);

            }

    }
    /**
     * Send Order Ready For Pickup Event by calling Order Ready For Pickup ESP Event API
     * @param orderModel
     */
    @Override
    public void sendOrderReadyForPickupEvent(final OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderReadyForPickupEventRequest orderReadyForPickupEventRequest = new OrderReadyForPickupEventRequest();
            getBlOrderReadyForPickupRequestPopulator().populate(orderModel,orderReadyForPickupEventRequest);
            ESPEventResponseWrapper espEventResponseWrapper = null;
            try
            {
                // Call send order Ready For Pickup ESP Event API
                espEventResponseWrapper = getBlESPEventRestService().sendOrderReadyForPickupEvent(orderReadyForPickupEventRequest);
            }catch (final BlESPIntegrationException exception){
                persistESPEventDetail(null, EspEventTypeEnum.ORDER_READYFORPICKUP,orderModel.getCode(), exception.getMessage(),exception.getRequestString());
            }
            // Save send order Ready For Pickup ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_READYFORPICKUP,orderModel.getCode(),null,null);

        }
    }
    /**
     * Send Order New Shipping by calling Order New Shipping ESP Event API
     * @param orderModel
     */
    @Override
    public void sendOrderNewShippingEvent(final OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderNewShippingEventRequest orderNewShippingEventRequest = new OrderNewShippingEventRequest();
            getBlOrderNewShippingRequestPopulator().populate(orderModel , orderNewShippingEventRequest);
            ESPEventResponseWrapper espEventResponseWrapper = null;
            try
            {
                // Call send order New Shipping ESP Event API
               espEventResponseWrapper = getBlESPEventRestService().sendOrderNewShippingEvent(orderNewShippingEventRequest);
            }catch (final BlESPIntegrationException exception){
                persistESPEventDetail(null, EspEventTypeEnum.ORDER_NEWSHIPPING,orderModel.getCode(), exception.getMessage(),exception.getRequestString());
            }
            // Save send order New Shipping ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_NEWSHIPPING,orderModel.getCode(),null,null);

        }
    }

  /**
   * Send Order Shipped by calling Order Shipped ESP Event API
   *
   * @param orderModel
   */
  @Override
  public void sendOrderShippedEvent(final OrderModel orderModel) {
    if (Objects.nonNull(orderModel)) {
      final OrderShippedEventRequest orderShippedEventRequest = new OrderShippedEventRequest();
      getBlOrderShippedRequestPopulator().populate(orderModel,
          orderShippedEventRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try {
        // Call send order shipped ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderShippedEvent(
            orderShippedEventRequest);
      } catch (final BlESPIntegrationException exception) {
        persistESPEventDetail(null, EspEventTypeEnum.ORDER_SHIPPED,orderModel.getCode(), exception.getMessage(),exception.getRequestString());
      }
      // Save send order shipped ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_SHIPPED,orderModel.getCode(),null,null);
    }
  }

  /**
   * Send Order PickedUp by calling Order PickedUp ESP Event API
   *
   * @param orderModel
   */
  @Override
  public void sendOrderPickedUpEvent(final OrderModel orderModel) {
    if (Objects.nonNull(orderModel)) {
      final OrderPickedUpEventRequest orderPickedUpEventRequest = new OrderPickedUpEventRequest();
      getBlOrderPickedUpRequestPopulator().populate(orderModel,
          orderPickedUpEventRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try {
        // Call send order pickedup ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderPickedUpEvent(
            orderPickedUpEventRequest);
      } catch (final BlESPIntegrationException exception) {
        persistESPEventDetail(null, EspEventTypeEnum.ORDER_PICKEDUP,orderModel.getCode(), exception.getMessage(),exception.getRequestString());
      }
      // Save send order pickedup ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_PICKEDUP,orderModel.getCode(),null,null);
    }
  }


  /**
   * This method created to prepare the request and response from Order Exception missing/broken ESP service
   * @param orderModel ordermodel
   */
  @Override
  public void sendOrderMissingBrokenLateEvent(final OrderModel orderModel, final OrderExceptionsExtraData orderExceptionsExtraData) {
    if (Objects.nonNull(orderModel)) {
      final OrderExceptionEventRequest orderExceptionEventRequest = new OrderExceptionEventRequest();
      orderExceptionEventRequest.setExtraData(orderExceptionsExtraData);
      getBlOrderExceptionsRequestPopulator().populate(orderModel,
          orderExceptionEventRequest);

      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send order Exception ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderException(
            orderExceptionEventRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.EXCEPTION_ISSUE,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save send order exception ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.EXCEPTION_ISSUE,orderModel.getCode(),null, null);
    }
  }




  /**
   * This method created to prepare the request and response from Order Unboxed ESP service
   * @param orderModel ordermodel
   */
  @Override
  public void sendOrderUnboxed(final OrderModel orderModel) {
    if (Objects.nonNull(orderModel)) {
      final OrderUnBoxedEventRequest orderUnBoxedEventRequest = new OrderUnBoxedEventRequest();

      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        getBlOrderUnboxedRequestPopulator().populate(orderModel,
            orderUnBoxedEventRequest);
        // Call send order Unboxed ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderUnboxed(
            orderUnBoxedEventRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.ORDER_UNBOXED,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save send order Unboxed ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_UNBOXED,orderModel.getCode(),null, null);
    }
  }


    /**
     * {@inheritDoc}
     */
    @Override
    public void sendOrderVerificationRequiredEvent(final OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderVerificationRequiredEventRequest verificationRequiredEventRequest = new OrderVerificationRequiredEventRequest();
            getBlOrderVerificationRequiredRequestPopulator().populate(orderModel,
                verificationRequiredEventRequest);
            ESPEventResponseWrapper espEventResponseWrapper = null;
            try {
                // Call send order verification required ESP Event API
                espEventResponseWrapper = getBlESPEventRestService().sendOrderVerificationRequiredEvent(
                    verificationRequiredEventRequest);
            } catch (final BlESPIntegrationException exception) {
                persistESPEventDetail(null, EspEventTypeEnum.VERIFICATION_REQUIRED,orderModel.getCode(), exception.getMessage(),exception.getRequestString());
            }
            // Save send order verification required ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.VERIFICATION_REQUIRED,orderModel.getCode(),null,null);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void sendOrderVerificationCompletedEvent(final OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderVerificationCompletedEventRequest verificationCompletedEventRequest = new OrderVerificationCompletedEventRequest();
            getBlOrderVerificationCompletedRequestPopulator().populate(orderModel,
                verificationCompletedEventRequest);
            ESPEventResponseWrapper espEventResponseWrapper = null;
            try {
                // Call send order verification completed ESP Event API
                espEventResponseWrapper = getBlESPEventRestService().sendOrderVerificationCompletedEvent(
                    verificationCompletedEventRequest);
            } catch (final BlESPIntegrationException exception) {
                persistESPEventDetail(null, EspEventTypeEnum.VERIFICATION_COMPLETED,orderModel.getCode(), exception.getMessage(),exception.getRequestString());
            }
            // Save send order verification completed ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.VERIFICATION_COMPLETED,orderModel.getCode(),null,null);
        }
    }



  /**
   * This method created to prepare the request and response from Order Deposit ESP service
   * @param orderModel ordermodel
   */
  @Override
  public void sendOrderDepositEvent(final OrderModel orderModel) {
    if (Objects.nonNull(orderModel)) {
      final OrderDepositRequest orderDepositRequest = new OrderDepositRequest();
      getBlOrderDepositRequestPopulator().populate(orderModel,
          orderDepositRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send order deposit ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderDepositEvent(orderDepositRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.VERIFICATION_DEPOSIT,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save send order deposit ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.VERIFICATION_DEPOSIT,orderModel.getCode(),null, null);
    }
  }


  /**
   * This method created to prepare the request and response from Extended Order ESP service
   * @param orderModel order model
   */
  @Override
  public void sendExtendOrderEvent(final OrderModel orderModel) {
    if (Objects.nonNull(orderModel)) {
      final OrderExtensionRequest orderExtensionRequest = new OrderExtensionRequest();
      getBlExtendOrderRequestPopulator().populate(orderModel, orderExtensionRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send order deposit ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendExtendOrderEvent(orderExtensionRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.ORDER_EXTENDED,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save send order deposit ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_EXTENDED,orderModel.getCode(),null, null);
    }
  }
  /**
   * Send Order Refund by calling Order Refund ESP Event API
   * @param orderModel
   * @param totalRefundAmount
   * @param refundMethod
   * @param orderCancelEntries
   */

  @Override
  public void sendOrderRefundEvent(final OrderModel orderModel,final double totalRefundAmount,final String refundMethod, final List<OrderCancelEntry> orderCancelEntries) {
    if (Objects.nonNull(orderModel)) {
      final OrderRefundEventRequest orderRefundEventRequest = new OrderRefundEventRequest();
      getBlOrderRefundRequestPopulator().populate(orderModel, orderRefundEventRequest );
      orderCancelEntries(totalRefundAmount, refundMethod, orderCancelEntries, orderRefundEventRequest.getData());
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send order deposit ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderRefundEvent(orderRefundEventRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.ORDER_REFUND,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save send order deposit ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_REFUND,orderModel.getCode(),null, null);

    }
  }

  /**
   * Send  bill paid data by calling  Order Bill Paid ESP Event API
   *
   * @param orderModel ordermodel
   */
  @Override
  public void sendOrderBillPaidEvent(final OrderModel orderModel, final OrderBillPaidExtraData orderBillPaidExtraData) {
    if (Objects.nonNull(orderModel)) {
      final OrderBillPaidEventRequest orderBillPaidEventRequest = new OrderBillPaidEventRequest();
      orderBillPaidEventRequest.setExtraData(orderBillPaidExtraData);
      getBlOrderBillPaidRequestPopulator().populate(orderModel,
          orderBillPaidEventRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try {
        // Call send order bill paid ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderBillPaidEvent(
            orderBillPaidEventRequest);
      } catch (final BlESPIntegrationException exception) {
        persistESPEventDetail(null, EspEventTypeEnum.BILL_PAID, orderModel.getCode(),
            exception.getMessage(), exception.getRequestString());
      }
      // Save send order bill paid ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.BILL_PAID,
          orderModel.getCode(), null, null);
    }
  }

  /**
   * It triggers bill paid esp event.
   *
   * @param payBillTotal
   * @param billingChargeTypeMap
   * @param orderModel
   */
  @Override
  public void triggerBillPaidEspEvent(final String payBillTotal,
      final Map<String, List<String>> billingChargeTypeMap, final OrderModel orderModel) {
    final OrderBillPaidExtraData orderBillPaidExtraData = new OrderBillPaidExtraData();
    if (MapUtils.isNotEmpty(billingChargeTypeMap)) {
      orderBillPaidExtraData.setBillPaidTypesMap(billingChargeTypeMap);
    }
    orderBillPaidExtraData.setTotalBillPaidAmount(payBillTotal);
    try{
      sendOrderBillPaidEvent(orderModel, orderBillPaidExtraData);
    }catch (final Exception exception){
      BlLogger.logMessage(LOG, Level.ERROR, "Failed to trigger Bill Paid ESP Event",
          exception);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void sendOrderManualAllocationEvent(final OrderModel orderModel) {
    if (Objects.nonNull(orderModel)) {
      final OrderManualAllocationEventRequest orderManualAllocationEventRequest = new OrderManualAllocationEventRequest();
      getBlOrderManualAllocationRequestPopulator().populate(orderModel,
          orderManualAllocationEventRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try {
        // Call send order manual allocation ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderManualAllocationEvent(
            orderManualAllocationEventRequest);
      } catch (final BlESPIntegrationException exception) {
        persistESPEventDetail(null, EspEventTypeEnum.MANUAL_ALLOCATION,orderModel.getCode(), exception.getMessage(),exception.getRequestString());
      }
      // Save send order manual allocation ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.MANUAL_ALLOCATION,orderModel.getCode(),null,null);
    }
  }

  /**
   * Order Cancel Entries
   * @param totalRefundAmount
   * @param refundMethod
   * @param orderCancelEntries
   */
  private void orderCancelEntries(final double totalRefundAmount, final String refundMethod,
      final List<OrderCancelEntry> orderCancelEntries,
      final OrderRefundData data) {
    data.setRefundamount(formatAmount(totalRefundAmount));
    data.setRefundmethod(refundMethod);
    populateOrderItemXMLData(orderCancelEntries, data);
  }

  /**
   * THis method populates order items in xml
   * @param orderCancelEntries orderCancelEntry
   * @param data data
   */
  private void populateOrderItemXMLData(final List<OrderCancelEntry> orderCancelEntries , final OrderRefundData data) {
    try {
      final Document orderItemsInXMLDocument = createNewXMLDocument();
      final Element rootOrderItems = createRootElementForDocument(orderItemsInXMLDocument, BlCoreConstants.ORDER_ITEMS_ROOT_ELEMENT);


      if (Objects.nonNull(orderCancelEntries)) {
        for (final OrderCancelEntry orderCancelEntry :orderCancelEntries) {
          final Element rootOrderItem = createRootElementForRootElement(orderItemsInXMLDocument,
              rootOrderItems, BlCoreConstants.ORDER_ITEM_ROOT_ELEMENT);
          if (Objects.nonNull(orderCancelEntry.getOrderEntry().getProduct())) {
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
                BlCoreConstants.ORDER_ITEM_PRODUCT_CODE,
                getRequestValue(orderCancelEntry.getOrderEntry().getProduct().getCode()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
                BlCoreConstants.ORDER_ITEM_PRODUCT_TITLE,
                getRequestValue(orderCancelEntry.getOrderEntry().getProduct().getName()));
            createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
                BlCoreConstants.ORDER_ITEM_PRODUCT_CANCEL_REASON,
                getRequestValue(orderCancelEntry.getCancelReason().getCode()));
          }
          createElementForRootElement(orderItemsInXMLDocument, rootOrderItem,
              BlCoreConstants.ORDER_ITEM_PRODUCT_PHOTO,
              getProductURL(orderCancelEntry));
        }
        }


      final Transformer transformer = getTransformerFactoryObject();
      final StringWriter writer = new StringWriter();

      //transform document to string
      transformer.transform(new DOMSource(orderItemsInXMLDocument), new StreamResult(writer));
      data.setOrderitemsinfo(writer.getBuffer().toString());

    } catch (final Exception exception) {
      throw new BlESPIntegrationException(exception.getMessage() , LogErrorCodeEnum.ORDER_INVALID.getCode() , exception);
    }
  }
  /**
   * This method created to get transformer factory object
   * @return transformer
   * @throws TransformerConfigurationException TransformerConfigurationException
   */
  protected Transformer getTransformerFactoryObject() throws TransformerConfigurationException {
	  final TransformerFactory factory = Utilities.getTransformerFactory(); //NOSONAR
	  return factory.newTransformer();
  }

  /**
   * This method created to prepare the request and response from ESP service
   * @param orderModel ordermodel
   */
  @Override
  public void sendOrderExtraItemsEvent(final OrderModel orderModel) {
    if (Objects.nonNull(orderModel)) {
      final OrderExtraItemRequest orderExtraItemRequest = new OrderExtraItemRequest();
      getBlExtraItemRequestPopulator().populate(orderModel,
          orderExtraItemRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send order Extra Item ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderExtraItemEvent(
            orderExtraItemRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.EXCEPTION_EXTRAITEM,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save send order Extra Item ESP ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.EXCEPTION_EXTRAITEM,orderModel.getCode(),null, null);
    }
  }

   /*
   * To check whether media is empty of not
   * @param abstractOrderEntryModel abstractOrderEntryModel
   * @return string
   */
  protected String getProductURL(final OrderCancelEntry abstractOrderEntryModel){
    return Objects.nonNull(abstractOrderEntryModel.getOrderEntry().getProduct().getPicture()) &&
        org.apache.commons.lang3.StringUtils
            .isNotBlank(abstractOrderEntryModel.getOrderEntry().getProduct().getPicture().getURL()) ?
        abstractOrderEntryModel.getOrderEntry().getProduct().getPicture().getURL() : org.apache.commons.lang3.StringUtils.EMPTY;
  }

  /**
   * To get the request value based
   * @param value value get from order
   * @return value to set on request
   */
  protected String getRequestValue(final String value){
    return org.apache.commons.lang3.StringUtils
        .isBlank(value) ? org.apache.commons.lang3.StringUtils.EMPTY :value;
  }
  /**
   * This method created to add the root element
   * @param document document to be add
   * @param rootElement root element to be set
   * @return element which append
   */
  protected Element createRootElementForRootElement(final Document document, final Element rootElement, final String rootElementName) {
    final Element childElement = document.createElement(rootElementName);
    rootElement.appendChild(childElement);
    return childElement;
  }
  /**
   * This method created to add the root element
   * @param document document to be add
   * @param rootElement root element to be set
   * @param value value to add
   * @return element which append
   */

  protected Element createElementForRootElement(final Document document, final Element rootElement, final String element, final String value) {
    final Element childElement = document.createElement(element);
    childElement.appendChild(document.createTextNode(value));
    rootElement.appendChild(childElement);
    return childElement;
  }


  /**
   * This method created to add the root element
   * @param document document to be add
   * @param rootElement root element to be set
   * @return element which append
   */
  protected Element createRootElementForDocument(final Document document, final String rootElement) {
    final Element root = document.createElement(rootElement);
    document.appendChild(root);
    return root;
  }

  /**
   * This method created to populate data
   * @return data which converted
   * @throws ParserConfigurationException parserConfigurationException
   */
  protected Document createNewXMLDocument() throws ParserConfigurationException {
    final DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
    final DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
    return documentBuilder.newDocument();
  }

  /**
     * This method created to store the response from ESP for all type of events
     * @param espEventResponseWrapper espEventResponseWrapper
     * @param eventTypeEnum enum type based on events
     * @param orderCode orderCode
     * @param errorMessage error message to store on model
     */
    private void persistESPEventDetail(final ESPEventResponseWrapper espEventResponseWrapper, final EspEventTypeEnum eventTypeEnum,
        final String orderCode, final String errorMessage, final String requestString) {

        if (null == espEventResponseWrapper)
        {
            final BlStoredEspEventModel blStoredEspEventModel = new BlStoredEspEventModel();
            blStoredEspEventModel.setOrderCode(orderCode);
            blStoredEspEventModel.setStatus(ESPEventStatus.FAILURE);
            blStoredEspEventModel.setEventType(eventTypeEnum);
            blStoredEspEventModel.setRequestString(requestString);
            blStoredEspEventModel.setResponseString(errorMessage);
            getModelService().save(blStoredEspEventModel);
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"No response received from {} event for order {} and setting status {} ",eventTypeEnum,orderCode,ESPEventStatus.FAILURE);
        }
        else if (StringUtils.isNotBlank(espEventResponseWrapper.getRequestString())
                && StringUtils.isNotBlank(espEventResponseWrapper.getResponseString())) {

            final BlStoredEspEventModel blStoredEspEventModel = new BlStoredEspEventModel();
            blStoredEspEventModel.setEventInstanceId(espEventResponseWrapper.getEventInstanceId());
            blStoredEspEventModel.setRequestString(espEventResponseWrapper.getRequestString());
            blStoredEspEventModel.setResponseString(espEventResponseWrapper.getResponseString());
            blStoredEspEventModel.setEventType(eventTypeEnum);
            blStoredEspEventModel.setStatus(ESPEventStatus.SUCCESS);
            blStoredEspEventModel.setOrderCode(orderCode);
            getModelService().save(blStoredEspEventModel);
            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,"Response received from {} event for order {} and setting status {} ",eventTypeEnum,orderCode,ESPEventStatus.SUCCESS);
        }

    }


  /**
   * This method created to prepare the request and response from ESP service
   * @param orderModel ordermodel
   */
  @Override
  public void sendOrderPullBackItemsAdded(final OrderModel orderModel , final
      AbstractOrderEntryModel abstractOrderEntryModel) {
    if (Objects.nonNull(orderModel)) {
      final OrderPullBackRequest orderPullBackRequest = new OrderPullBackRequest();
      orderPullBackRequest.setOrderEntry(abstractOrderEntryModel);
      getBlOrderPullBackItemsAddedRequestPopulator().populate(orderModel, orderPullBackRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send order Pull Back Added Items  ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderPullBackItemsAdded(orderPullBackRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.ORDER_PULL_BACK_ITEMS_ADDED,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save snd order Pull Back Added Items  ESP Event API
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_PULL_BACK_ITEMS_ADDED,orderModel.getCode(),null, null);
    }
  }



  /**
   * This method created to prepare the request and response from ESP service
   * @param orderModel to get the values
   */
  @Override
  public void sendOrderPullBackItemsRemoved(final OrderModel orderModel , final List<BlSerialProductModel> blSerialProductModels) {
    if (Objects.nonNull(orderModel)) {
      final OrderPullBackRequest orderPullBackRequest = new OrderPullBackRequest();
     orderPullBackRequest.setSerialProducts(blSerialProductModels);
      getBlOrderPullBackItemRemovedRequestPopulator().populate(orderModel, orderPullBackRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send order Pull Back Added Removed  ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderPullBackItemsRemoved(orderPullBackRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.ORDER_PULL_BACK_ITEMS_REMOVED,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save snd order Pull Back Added Removed  ESP Event API
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_PULL_BACK_ITEMS_REMOVED,orderModel.getCode(),null, null);
    }
  }


  /**
   * This method created to prepare the request and response from Order Deposit Required ESP service
   * @param orderModel ordermodel
   */
  @Override
  public void sendOrderDepositRequired(final OrderModel orderModel , final Double amount) {
    if (Objects.nonNull(orderModel)) {
      orderModel.setDepositAmount(amount);
      getModelService().save(orderModel);
      getModelService().refresh(orderModel);
      BlLogger.logFormattedMessage(LOG , Level.DEBUG , "Deposit Required Amount {} deposited for orderCode {}" , String.valueOf(amount) , orderModel.getCode());
      final OrderDepositRequiredEventRequest orderDepositRequiredEventRequest = new OrderDepositRequiredEventRequest();
      getBlOrderDepositRequiredRequestPopulator().populate(orderModel,
          orderDepositRequiredEventRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send order deposit required ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendOrderDepositRequired(
            orderDepositRequiredEventRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.DEPOSIT_REQUIRED,orderModel.getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save send order confirmation ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.DEPOSIT_REQUIRED,orderModel.getCode(),null, null);
    }
  }

  /**
   * This method created to prepare the request and response from Gift Card Purchase ESP service
   * @param giftCardModel giftCardMovementModel
   */
  @Override
  public void sendGiftCardPurchaseEvent(final GiftCardModel giftCardModel ,
      final AtomicReference<AbstractOrderModel> abstractOrderModel) {
      final GiftCardPurchaseEventRequest giftCardPurchaseEventRequest = new GiftCardPurchaseEventRequest();
      giftCardPurchaseEventRequest.setOrderModel(abstractOrderModel.get());
      getBlOrderGiftCardPurchaseEventPopulator().populate(giftCardModel,
          giftCardPurchaseEventRequest);
      ESPEventResponseWrapper espEventResponseWrapper = null;
      try
      {
        // Call send Gift Card Purchase ESP Event API
        espEventResponseWrapper = getBlESPEventRestService().sendGiftCardPurchase(giftCardPurchaseEventRequest);
      }catch (final BlESPIntegrationException exception){
        persistESPEventDetail(null, EspEventTypeEnum.GIFT_CARD_PURCHASE,abstractOrderModel.get().getCode(), exception.getMessage(), exception.getRequestString());
      }
      // Save send Gift Card Purchase ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.GIFT_CARD_PURCHASE,abstractOrderModel.get().getCode(),null, null);

  }

   /**
   * {@inheritDoc}
   */
  @Override
  public void sendForgotPasswordRequest(final ESPEmailCommonRequestData emailRequestData) {
    final ESPEmailCommonEventRequest emailRequiredEventRequest = new ESPEmailCommonEventRequest();
    getBlESPEmailCommonRequestPopulator()
        .populate(emailRequestData, emailRequiredEventRequest);
    final ESPEventResponseWrapper espEventResponseWrapper;
    try {
      // Call send forgot password required ESP Event API
      espEventResponseWrapper = getBlESPEventRestService()
          .sendESPEmailEventRequest(emailRequiredEventRequest);
      // Save send forgot password request ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.FORGOT_PASSWORD,
          emailRequestData.getEmailAddress(), null, null);
    } catch (final BlESPIntegrationException exception) {
      persistESPEventDetail(null, EspEventTypeEnum.FORGOT_PASSWORD,
          emailRequestData.getEmailAddress(), exception.getMessage(),
          exception.getRequestString());
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void sendNotifyMeConfirmEmailRequest(final ESPEmailCommonRequestData emailRequestData){
    final ESPEmailCommonEventRequest emailRequiredEventRequest = new ESPEmailCommonEventRequest();
    getBlESPEmailCommonRequestPopulator()
        .populate(emailRequestData, emailRequiredEventRequest);
    final ESPEventResponseWrapper espEventResponseWrapper;
    try {
      // Call notify me required ESP Event API
      espEventResponseWrapper = getBlESPEventRestService()
          .sendESPEmailEventRequest(emailRequiredEventRequest);
      // Save notify me email request ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.NOTIFY_ME_EMAIL,
          emailRequestData.getEmailAddress(), null, null);
    } catch (final BlESPIntegrationException exception) {
      persistESPEventDetail(null, EspEventTypeEnum.NOTIFY_ME_EMAIL,
          emailRequestData.getEmailAddress(), exception.getMessage(),
          exception.getRequestString());
    }
  }


  /**
   * This method created to prepare the request and response from Free Gift Card ESP service
   * @param giftCardModel giftCardMovementModel
   */
  @Override
  public void sendFreeGiftCardPurchaseEvent(final GiftCardModel giftCardModel) {
    final FreeGiftCardPurchaseEventRequest freeGiftCardPurchaseEventRequest = new FreeGiftCardPurchaseEventRequest();
    getBlFreeGiftCardPurchaseEventPopulator().populate(giftCardModel, freeGiftCardPurchaseEventRequest);
    ESPEventResponseWrapper espEventResponseWrapper = null;
    try
    {
      // Call send Free Gift Card Purchase ESP Event API
      espEventResponseWrapper = getBlESPEventRestService().sendFreeGiftCardPurchase(freeGiftCardPurchaseEventRequest);
    }catch (final BlESPIntegrationException exception){
      persistESPEventDetail(null, EspEventTypeEnum.FREE_GIFT_CARD_PURCHASE,giftCardModel.getCode(), exception.getMessage(), exception.getRequestString());
    }
    // Save send Free Gift Card Purchase ESP Event Detail
    persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.FREE_GIFT_CARD_PURCHASE,giftCardModel.getCode(),null, null);

  }


  /**
   * {@inheritDoc}
   */
  @Override
  public void sendBackInStockEmailRequest(final ESPEmailCommonRequestData emailRequestData,
      final Date requestedDate){
    final ESPEmailCommonEventRequest emailRequiredEventRequest = new ESPEmailCommonEventRequest();
    getBlESPEmailCommonRequestPopulator()
        .populate(emailRequestData, emailRequiredEventRequest);
    emailRequestData.setTemplate(backInStockTemplate);
    final SimpleDateFormat formatTime = new SimpleDateFormat(BlCoreConstants.DATE_PATTERN);
    emailRequestData.setRequestedDate(formatTime.format(requestedDate));
    final ESPEventResponseWrapper espEventResponseWrapper;
    try {
      // Call back in stock required ESP Event API
      espEventResponseWrapper = getBlESPEventRestService()
          .sendESPEmailEventRequest(emailRequiredEventRequest);
      // Save back in stock email request ESP Event Detail
      persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.BACK_IN_STOCK_EMAIL,
          emailRequestData.getEmailAddress(), null, null);
    } catch (final BlESPIntegrationException exception) {
      persistESPEventDetail(null, EspEventTypeEnum.BACK_IN_STOCK_EMAIL,
          emailRequestData.getEmailAddress(), exception.getMessage(),
          exception.getRequestString());
    }
  }
  /**
   * Format amount string.
   * @param amount the amount
   * @return the string
   */
  protected String formatAmount(final Double amount) {
    final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat.getNumberInstance(Locale.ENGLISH);
    decimalFormat.applyPattern(BlCoreConstants.FORMAT_STRING);
    return decimalFormat.format(amount);
  }


  public BlOrderConfirmationRequestPopulator getBlOrderConfirmationRequestPopulator() {
        return blOrderConfirmationRequestPopulator;
    }

    public void setBlOrderConfirmationRequestPopulator(
        final BlOrderConfirmationRequestPopulator blOrderConfirmationRequestPopulator) {
        this.blOrderConfirmationRequestPopulator = blOrderConfirmationRequestPopulator;
    }

    public BlOrderVerificationMoreInfoRequestPopulator getBlOrderVerificationMoreInfoRequestPopulator() {
        return blOrderVerificationMoreInfoRequestPopulator;
    }

    public void setBlOrderVerificationMoreInfoRequestPopulator(
        final BlOrderVerificationMoreInfoRequestPopulator blOrderVerificationMoreInfoRequestPopulator) {
        this.blOrderVerificationMoreInfoRequestPopulator = blOrderVerificationMoreInfoRequestPopulator;
    }

    public BlOrderVerificationCOIneededRequestPopulator getBlOrderVerificationCOIneededRequestPopulator() {
        return blOrderVerificationCOIneededRequestPopulator;
    }

    public void setBlOrderVerificationCOIneededRequestPopulator(
        final BlOrderVerificationCOIneededRequestPopulator blOrderVerificationCOIneededRequestPopulator) {
        this.blOrderVerificationCOIneededRequestPopulator = blOrderVerificationCOIneededRequestPopulator;
    }

    public BlESPEventRestService getBlESPEventRestService() {
        return blESPEventRestService;
    }

    public void setBlESPEventRestService(final BlESPEventRestService blESPEventRestService) {
        this.blESPEventRestService = blESPEventRestService;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(final ModelService modelService) {
        this.modelService = modelService;
    }

    public BlOrderCanceledRequestPopulator getBlOrderCanceledRequestPopulator() {
        return blOrderCanceledRequestPopulator;
    }

    public void setBlOrderCanceledRequestPopulator(
        final BlOrderCanceledRequestPopulator blOrderCanceledRequestPopulator) {
        this.blOrderCanceledRequestPopulator = blOrderCanceledRequestPopulator;
    }


  public BlOrderExceptionsRequestPopulator getBlOrderExceptionsRequestPopulator() {
    return blOrderExceptionsRequestPopulator;
  }

  public void setBlOrderExceptionsRequestPopulator(
      final BlOrderExceptionsRequestPopulator blOrderExceptionsRequestPopulator) {
    this.blOrderExceptionsRequestPopulator = blOrderExceptionsRequestPopulator;
  }

  public BlOrderUnboxedRequestPopulator getBlOrderUnboxedRequestPopulator() {
    return blOrderUnboxedRequestPopulator;
  }

  public void setBlOrderUnboxedRequestPopulator(
      final BlOrderUnboxedRequestPopulator blOrderUnboxedRequestPopulator) {
    this.blOrderUnboxedRequestPopulator = blOrderUnboxedRequestPopulator;
  }

    public BlOrderPaymentDeclinedRequestPopulator getBlOrderPaymentDeclinedRequestPopulator() {
        return blOrderPaymentDeclinedRequestPopulator;
    }

    public void setBlOrderPaymentDeclinedRequestPopulator(
        final BlOrderPaymentDeclinedRequestPopulator blOrderPaymentDeclinedRequestPopulator) {
        this.blOrderPaymentDeclinedRequestPopulator = blOrderPaymentDeclinedRequestPopulator;
    }


    public BlOrderVerificationRequiredRequestPopulator getBlOrderVerificationRequiredRequestPopulator() {
        return blOrderVerificationRequiredRequestPopulator;
    }

    public BlOrderReadyForPickupRequestPopulator getBlOrderReadyForPickupRequestPopulator() {
        return blOrderReadyForPickupRequestPopulator;
    }

    public void setBlOrderReadyForPickupRequestPopulator(
        final BlOrderReadyForPickupRequestPopulator blOrderReadyForPickupRequestPopulator) {
        this.blOrderReadyForPickupRequestPopulator = blOrderReadyForPickupRequestPopulator;
    }


    public BlOrderNewShippingRequestPopulator getBlOrderNewShippingRequestPopulator() {
        return blOrderNewShippingRequestPopulator;
    }

    public void setBlOrderNewShippingRequestPopulator(
        final BlOrderNewShippingRequestPopulator blOrderNewShippingRequestPopulator) {
        this.blOrderNewShippingRequestPopulator = blOrderNewShippingRequestPopulator;
    }

    public void setBlOrderVerificationRequiredRequestPopulator(
        final BlOrderVerificationRequiredRequestPopulator blOrderVerificationRequiredRequestPopulator) {
        this.blOrderVerificationRequiredRequestPopulator = blOrderVerificationRequiredRequestPopulator;
    }

    public BlOrderVerificationCompletedRequestPopulator getBlOrderVerificationCompletedRequestPopulator() {
        return blOrderVerificationCompletedRequestPopulator;
    }

    public void setBlOrderVerificationCompletedRequestPopulator(
        final BlOrderVerificationCompletedRequestPopulator blOrderVerificationCompletedRequestPopulator) {
        this.blOrderVerificationCompletedRequestPopulator = blOrderVerificationCompletedRequestPopulator;
    }

  public BlOrderDepositRequestPopulator getBlOrderDepositRequestPopulator() {
    return blOrderDepositRequestPopulator;
  }

  public void setBlOrderDepositRequestPopulator(
      final BlOrderDepositRequestPopulator blOrderDepositRequestPopulator) {
    this.blOrderDepositRequestPopulator = blOrderDepositRequestPopulator;
  }

  public BlOrderShippedRequestPopulator getBlOrderShippedRequestPopulator() {
    return blOrderShippedRequestPopulator;
  }

  public void setBlOrderShippedRequestPopulator(
      final BlOrderShippedRequestPopulator blOrderShippedRequestPopulator) {
    this.blOrderShippedRequestPopulator = blOrderShippedRequestPopulator;
  }

  public BlOrderPickedUpRequestPopulator getBlOrderPickedUpRequestPopulator() {
    return blOrderPickedUpRequestPopulator;
  }

  public void setBlOrderPickedUpRequestPopulator(
      final BlOrderPickedUpRequestPopulator blOrderPickedUpRequestPopulator) {
    this.blOrderPickedUpRequestPopulator = blOrderPickedUpRequestPopulator;
  }

  public BlExtendOrderRequestPopulator getBlExtendOrderRequestPopulator() {
    return blExtendOrderRequestPopulator;
  }

  public void setBlExtendOrderRequestPopulator(final BlExtendOrderRequestPopulator blExtendOrderRequestPopulator) {
    this.blExtendOrderRequestPopulator = blExtendOrderRequestPopulator;
  }



  public BlExtraItemRequestPopulator getBlExtraItemRequestPopulator() {
    return blExtraItemRequestPopulator;
  }

  public void setBlExtraItemRequestPopulator(
      final BlExtraItemRequestPopulator blExtraItemRequestPopulator) {
    this.blExtraItemRequestPopulator = blExtraItemRequestPopulator;
  }

  public BlOrderRefundRequestPopulator getBlOrderRefundRequestPopulator() {
    return blOrderRefundRequestPopulator;
  }

  public void setBlOrderRefundRequestPopulator(
      final BlOrderRefundRequestPopulator blOrderRefundRequestPopulator) {
    this.blOrderRefundRequestPopulator = blOrderRefundRequestPopulator;
  }

  public BlOrderBillPaidRequestPopulator getBlOrderBillPaidRequestPopulator() {
    return blOrderBillPaidRequestPopulator;
  }

  public void setBlOrderBillPaidRequestPopulator(
      final BlOrderBillPaidRequestPopulator blOrderBillPaidRequestPopulator) {
    this.blOrderBillPaidRequestPopulator = blOrderBillPaidRequestPopulator;
  }

  public BlOrderPullBackItemsAddedRequestPopulator getBlOrderPullBackItemsAddedRequestPopulator() {
    return blOrderPullBackItemsAddedRequestPopulator;
  }

  public void setBlOrderPullBackItemsAddedRequestPopulator(
      final BlOrderPullBackItemsAddedRequestPopulator blOrderPullBackItemsAddedRequestPopulator) {
    this.blOrderPullBackItemsAddedRequestPopulator = blOrderPullBackItemsAddedRequestPopulator;
  }


  public BlOrderPullBackItemRemovedRequestPopulator getBlOrderPullBackItemRemovedRequestPopulator() {
    return blOrderPullBackItemRemovedRequestPopulator;
  }

  public void setBlOrderPullBackItemRemovedRequestPopulator(
      final BlOrderPullBackItemRemovedRequestPopulator blOrderPullBackItemRemovedRequestPopulator) {
    this.blOrderPullBackItemRemovedRequestPopulator = blOrderPullBackItemRemovedRequestPopulator;
  }



  public BlOrderManualAllocationRequestPopulator getBlOrderManualAllocationRequestPopulator() {
    return blOrderManualAllocationRequestPopulator;
  }

  public void setBlOrderManualAllocationRequestPopulator(
      final BlOrderManualAllocationRequestPopulator blOrderManualAllocationRequestPopulator) {
    this.blOrderManualAllocationRequestPopulator = blOrderManualAllocationRequestPopulator;
  }

  public BlOrderDepositRequiredRequestPopulator getBlOrderDepositRequiredRequestPopulator() {
    return blOrderDepositRequiredRequestPopulator;
  }

  public void setBlOrderDepositRequiredRequestPopulator(
      final BlOrderDepositRequiredRequestPopulator blOrderDepositRequiredRequestPopulator) {
    this.blOrderDepositRequiredRequestPopulator = blOrderDepositRequiredRequestPopulator;
  }

  public BlOrderGiftCardPurchaseEventPopulator getBlOrderGiftCardPurchaseEventPopulator() {
    return blOrderGiftCardPurchaseEventPopulator;
  }

  public void setBlOrderGiftCardPurchaseEventPopulator(
      final BlOrderGiftCardPurchaseEventPopulator blOrderGiftCardPurchaseEventPopulator) {
    this.blOrderGiftCardPurchaseEventPopulator = blOrderGiftCardPurchaseEventPopulator;
  }


  public BlFreeGiftCardPurchaseEventPopulator getBlFreeGiftCardPurchaseEventPopulator() {
    return blFreeGiftCardPurchaseEventPopulator;
  }

  public void setBlFreeGiftCardPurchaseEventPopulator(
      final BlFreeGiftCardPurchaseEventPopulator blFreeGiftCardPurchaseEventPopulator) {
    this.blFreeGiftCardPurchaseEventPopulator = blFreeGiftCardPurchaseEventPopulator;
  }


  public BlESPEmailCommonRequestPopulator getBlESPEmailCommonRequestPopulator() {
    return blESPEmailCommonRequestPopulator;
  }

  public void setBlESPEmailCommonRequestPopulator(
      final BlESPEmailCommonRequestPopulator blESPEmailCommonRequestPopulator) {
    this.blESPEmailCommonRequestPopulator = blESPEmailCommonRequestPopulator;
  }
}
