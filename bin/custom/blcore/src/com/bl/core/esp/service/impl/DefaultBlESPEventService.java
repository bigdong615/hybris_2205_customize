package com.bl.core.esp.service.impl;

import com.bl.core.esp.populators.BlOrderPaymentDeclinedRequestPopulator;
import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.core.esp.populators.BlOrderCanceledRequestPopulator;
import com.bl.core.esp.populators.BlOrderConfirmationRequestPopulator;
import com.bl.core.esp.populators.BlOrderVerificationMoreInfoRequestPopulator;
import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.model.BlStoredEspEventModel;
import com.bl.esp.dto.canceledEvent.OrderCanceledEventRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.dto.paymentdeclined.OrderPaymentDeclinedEventRequest;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationEventRequest;
import com.bl.esp.dto.orderverification.OrderVerificationMoreInfoEventRequest;
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
    private BlOrderCanceledRequestPopulator blOrderCanceledRequestPopulator;
    private BlOrderPaymentDeclinedRequestPopulator blOrderPaymentDeclinedRequestPopulator;
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
        {
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

    public BlOrderPaymentDeclinedRequestPopulator getBlOrderPaymentDeclinedRequestPopulator() {
        return blOrderPaymentDeclinedRequestPopulator;
    }

    public void setBlOrderPaymentDeclinedRequestPopulator(
        BlOrderPaymentDeclinedRequestPopulator blOrderPaymentDeclinedRequestPopulator) {
        this.blOrderPaymentDeclinedRequestPopulator = blOrderPaymentDeclinedRequestPopulator;
    }


}
