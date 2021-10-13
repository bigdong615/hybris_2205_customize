package com.bl.core.esp.service.impl;


import com.bl.core.esp.populators.BlExtendOrderRequestPopulator;
import com.bl.core.esp.populators.BlExtraItemRequestPopulator;
import com.bl.core.esp.populators.BlOrderNewShippingRequestPopulator;
import com.bl.core.esp.populators.BlOrderPaymentDeclinedRequestPopulator;
import com.bl.core.esp.populators.BlOrderPickedUpRequestPopulator;
import com.bl.core.esp.populators.BlOrderReadyForPickupRequestPopulator;
import com.bl.core.esp.populators.BlOrderShippedRequestPopulator;
import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.extraItem.OrderExtraItemRequest;
import com.bl.esp.dto.newshipping.OrderNewShippingEventRequest;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.core.esp.populators.BlOrderCanceledRequestPopulator;
import com.bl.core.esp.populators.BlOrderConfirmationRequestPopulator;
import com.bl.core.esp.populators.BlOrderDepositRequestPopulator;
import com.bl.core.esp.populators.BlOrderExceptionsRequestPopulator;
import com.bl.core.esp.populators.BlOrderUnboxedRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationCOIneededRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationCompletedRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationMoreInfoRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationRequiredRequestPopulator;
import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.model.BlStoredEspEventModel;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.orderdeposit.OrderDepositRequest;
import com.bl.esp.dto.orderexceptions.OrderExceptionEventRequest;
import com.bl.esp.dto.orderexceptions.data.OrderExceptionsExtraData;
import com.bl.esp.dto.orderextension.OrderExtensionRequest;
import com.bl.esp.dto.orderunboxed.OrderUnBoxedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationCOIneededEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationCompletedEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationRequiredEventRequest;
import com.bl.esp.dto.paymentdeclined.OrderPaymentDeclinedEventRequest;
import com.bl.esp.dto.pickedup.OrderPickedUpEventRequest;
import com.bl.esp.dto.readyforpickup.OrderReadyForPickupEventRequest;
import com.bl.esp.dto.shipped.OrderShippedEventRequest;
import com.bl.esp.enums.ESPEventStatus;
import com.bl.esp.enums.EspEventTypeEnum;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.esp.service.BlESPEventRestService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Objects;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

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
    private BlESPEventRestService blESPEventRestService;
    private ModelService modelService;

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
    public void sendOrderMoreInfoRequiredEvent(final OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderVerificationMoreInfoEventRequest orderVerificationMoreInfoEventRequest = new OrderVerificationMoreInfoEventRequest();
            getBlOrderVerificationMoreInfoRequestPopulator().populate(orderModel,
                orderVerificationMoreInfoEventRequest);

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
            final OrderVerificationCOIneededEventRequest orderVerificationCOIneededEventRequest = new OrderVerificationCOIneededEventRequest();
            getBlOrderVerificationCOIneededRequestPopulator().populate(orderModel,
                orderVerificationCOIneededEventRequest);
          orderVerificationCOIneededEventRequest.getData().setCoiamount((double) amount);

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

    public BlOrderConfirmationRequestPopulator getBlOrderConfirmationRequestPopulator() {
        return blOrderConfirmationRequestPopulator;
    }

    public void setBlOrderConfirmationRequestPopulator(
        BlOrderConfirmationRequestPopulator blOrderConfirmationRequestPopulator) {
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

    public void setBlESPEventRestService(BlESPEventRestService blESPEventRestService) {
        this.blESPEventRestService = blESPEventRestService;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

    public BlOrderCanceledRequestPopulator getBlOrderCanceledRequestPopulator() {
        return blOrderCanceledRequestPopulator;
    }

    public void setBlOrderCanceledRequestPopulator(
        BlOrderCanceledRequestPopulator blOrderCanceledRequestPopulator) {
        this.blOrderCanceledRequestPopulator = blOrderCanceledRequestPopulator;
    }


  public BlOrderExceptionsRequestPopulator getBlOrderExceptionsRequestPopulator() {
    return blOrderExceptionsRequestPopulator;
  }

  public void setBlOrderExceptionsRequestPopulator(
      BlOrderExceptionsRequestPopulator blOrderExceptionsRequestPopulator) {
    this.blOrderExceptionsRequestPopulator = blOrderExceptionsRequestPopulator;
  }

  public BlOrderUnboxedRequestPopulator getBlOrderUnboxedRequestPopulator() {
    return blOrderUnboxedRequestPopulator;
  }

  public void setBlOrderUnboxedRequestPopulator(
      BlOrderUnboxedRequestPopulator blOrderUnboxedRequestPopulator) {
    this.blOrderUnboxedRequestPopulator = blOrderUnboxedRequestPopulator;
  }

    public BlOrderPaymentDeclinedRequestPopulator getBlOrderPaymentDeclinedRequestPopulator() {
        return blOrderPaymentDeclinedRequestPopulator;
    }

    public void setBlOrderPaymentDeclinedRequestPopulator(
        BlOrderPaymentDeclinedRequestPopulator blOrderPaymentDeclinedRequestPopulator) {
        this.blOrderPaymentDeclinedRequestPopulator = blOrderPaymentDeclinedRequestPopulator;
    }


    public BlOrderVerificationRequiredRequestPopulator getBlOrderVerificationRequiredRequestPopulator() {
        return blOrderVerificationRequiredRequestPopulator;
    }

    public BlOrderReadyForPickupRequestPopulator getBlOrderReadyForPickupRequestPopulator() {
        return blOrderReadyForPickupRequestPopulator;
    }

    public void setBlOrderReadyForPickupRequestPopulator(
        BlOrderReadyForPickupRequestPopulator blOrderReadyForPickupRequestPopulator) {
        this.blOrderReadyForPickupRequestPopulator = blOrderReadyForPickupRequestPopulator;
    }


    public BlOrderNewShippingRequestPopulator getBlOrderNewShippingRequestPopulator() {
        return blOrderNewShippingRequestPopulator;
    }

    public void setBlOrderNewShippingRequestPopulator(
        BlOrderNewShippingRequestPopulator blOrderNewShippingRequestPopulator) {
        this.blOrderNewShippingRequestPopulator = blOrderNewShippingRequestPopulator;
    }

    public void setBlOrderVerificationRequiredRequestPopulator(
        BlOrderVerificationRequiredRequestPopulator blOrderVerificationRequiredRequestPopulator) {
        this.blOrderVerificationRequiredRequestPopulator = blOrderVerificationRequiredRequestPopulator;
    }

    public BlOrderVerificationCompletedRequestPopulator getBlOrderVerificationCompletedRequestPopulator() {
        return blOrderVerificationCompletedRequestPopulator;
    }

    public void setBlOrderVerificationCompletedRequestPopulator(
        BlOrderVerificationCompletedRequestPopulator blOrderVerificationCompletedRequestPopulator) {
        this.blOrderVerificationCompletedRequestPopulator = blOrderVerificationCompletedRequestPopulator;
    }

  public BlOrderDepositRequestPopulator getBlOrderDepositRequestPopulator() {
    return blOrderDepositRequestPopulator;
  }

  public void setBlOrderDepositRequestPopulator(
      BlOrderDepositRequestPopulator blOrderDepositRequestPopulator) {
    this.blOrderDepositRequestPopulator = blOrderDepositRequestPopulator;
  }

  public BlOrderShippedRequestPopulator getBlOrderShippedRequestPopulator() {
    return blOrderShippedRequestPopulator;
  }

  public void setBlOrderShippedRequestPopulator(
      BlOrderShippedRequestPopulator blOrderShippedRequestPopulator) {
    this.blOrderShippedRequestPopulator = blOrderShippedRequestPopulator;
  }

  public BlOrderPickedUpRequestPopulator getBlOrderPickedUpRequestPopulator() {
    return blOrderPickedUpRequestPopulator;
  }

  public void setBlOrderPickedUpRequestPopulator(
      BlOrderPickedUpRequestPopulator blOrderPickedUpRequestPopulator) {
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
      BlExtraItemRequestPopulator blExtraItemRequestPopulator) {
    this.blExtraItemRequestPopulator = blExtraItemRequestPopulator;
  }
}
