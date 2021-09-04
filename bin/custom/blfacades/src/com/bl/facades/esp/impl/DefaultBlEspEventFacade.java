package com.bl.facades.esp.impl;

import com.bl.core.enums.EspEventTypeEnum;
import com.bl.core.model.BlStoredEspEventModel;
import com.bl.core.services.esp.models.OrderConfirmationRequest;
import com.bl.core.services.esp.models.OrderConfirmationResponseWrapper;
import com.bl.core.services.esp.order.BlEspEventService;
import com.bl.facades.esp.BlEspEventFacade;
import com.bl.facades.populators.BlOrderConfirmationRequestPopulator;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.lang.StringUtils;

import java.util.Objects;

public class DefaultBlEspEventFacade implements BlEspEventFacade {

    private BlOrderConfirmationRequestPopulator blOrderConfirmationRequestPopulator;
    private BlEspEventService blEspEventService;
    private ModelService modelService;

    @Override
    public void sendOrderConfirmation(OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            OrderConfirmationRequest orderConfirmationRequest = new OrderConfirmationRequest();
            getBlOrderConfirmationRequestPopulator().populate(orderModel, orderConfirmationRequest);
            // Call send order confirmation ESP Event API
            OrderConfirmationResponseWrapper orderConfirmationResponseWrapper = getBlEspEventService().sendOrderConfirmation(orderConfirmationRequest);
            // Save send order confirmation ESP Event Detail
            persistESPEventDetail(orderConfirmationResponseWrapper, EspEventTypeEnum.ORDER_CONFIRMATION_EVENT);
        }
    }

    private void persistESPEventDetail(OrderConfirmationResponseWrapper orderConfirmationResponseWrapper, EspEventTypeEnum eventTypeEnum) {
        if (Objects.nonNull(orderConfirmationResponseWrapper)
                && StringUtils.isNotBlank(orderConfirmationResponseWrapper.getRequestString())
                && StringUtils.isNotBlank(orderConfirmationResponseWrapper.getResponseString())) {
            BlStoredEspEventModel blStoredEspEventModel = new BlStoredEspEventModel();
            blStoredEspEventModel.setEventInstanceId(orderConfirmationResponseWrapper.getEventInstanceId());
            blStoredEspEventModel.setRequestString(orderConfirmationResponseWrapper.getRequestString());
            blStoredEspEventModel.setResponseString(orderConfirmationResponseWrapper.getResponseString());
            blStoredEspEventModel.setEventType(eventTypeEnum);
            getModelService().save(blStoredEspEventModel);
        }
    }

    public BlOrderConfirmationRequestPopulator getBlOrderConfirmationRequestPopulator() {
        return blOrderConfirmationRequestPopulator;
    }

    public void setBlOrderConfirmationRequestPopulator(BlOrderConfirmationRequestPopulator blOrderConfirmationRequestPopulator) {
        this.blOrderConfirmationRequestPopulator = blOrderConfirmationRequestPopulator;
    }

    public BlEspEventService getBlEspEventService() {
        return blEspEventService;
    }

    public void setBlEspEventService(BlEspEventService blEspEventService) {
        this.blEspEventService = blEspEventService;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }
}
