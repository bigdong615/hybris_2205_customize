package com.bl.core.esp.service.impl;

import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.model.BlStoredEspEventModel;
import com.bl.core.esp.populators.BlOrderConfirmationRequestPopulator;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.enums.ESPEventStatus;
import com.bl.esp.enums.EspEventTypeEnum;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.esp.order.OrderConfirmationRequest;
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
    private BlESPEventRestService blESPEventRestService;
    private ModelService modelService;

    /**
     * This method created to prepare the request and response from ESP service
     * @param orderModel ordermodel
     */
    @Override
    public void sendOrderConfirmation(final OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderConfirmationRequest orderConfirmationRequest = new OrderConfirmationRequest();
            getBlOrderConfirmationRequestPopulator().populate(orderModel, orderConfirmationRequest);
            ESPEventResponseWrapper espEventResponseWrapper = null;
            try
            {
                // Call send order confirmation ESP Event API
                espEventResponseWrapper = getBlESPEventRestService().sendOrderConfirmation(orderConfirmationRequest);
            }catch (final BlESPIntegrationException exception){
                persistESPEventDetail(null, EspEventTypeEnum.ORDER_CONFIRM,orderModel.getCode(), exception.getMessage());
            }
            // Save send order confirmation ESP Event Detail
            persistESPEventDetail(espEventResponseWrapper, EspEventTypeEnum.ORDER_CONFIRM,orderModel.getCode(),null);
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
        final String orderCode, final String errorMessage) {

        if (null == espEventResponseWrapper)
        {
            final BlStoredEspEventModel blStoredEspEventModel = new BlStoredEspEventModel();
            blStoredEspEventModel.setOrderCode(orderCode);
            blStoredEspEventModel.setStatus(ESPEventStatus.FAILURE);
            blStoredEspEventModel.setEventType(eventTypeEnum);
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

    public void setBlOrderConfirmationRequestPopulator(BlOrderConfirmationRequestPopulator blOrderConfirmationRequestPopulator) {
        this.blOrderConfirmationRequestPopulator = blOrderConfirmationRequestPopulator;
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
}
