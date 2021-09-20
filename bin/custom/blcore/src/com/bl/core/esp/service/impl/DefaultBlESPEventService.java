package com.bl.core.esp.service.impl;

import com.bl.core.enums.EspEventTypeEnum;
import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.model.BlStoredEspEventModel;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationRequest;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationResponseWrapper;
import com.bl.esp.service.BlESPEventRestService;
import com.bl.core.populators.BlOrderConfirmationRequestPopulator;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.commons.lang.StringUtils;

import java.util.Objects;

public class DefaultBlESPEventService implements BlESPEventService {

    private BlOrderConfirmationRequestPopulator blOrderConfirmationRequestPopulator;
    private BlESPEventRestService blESPEventRestService;
    private ModelService modelService;

    @Override
    public void sendOrderConfirmation(OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            OrderConfirmationRequest orderConfirmationRequest = new OrderConfirmationRequest();
            getBlOrderConfirmationRequestPopulator().populate(orderModel, orderConfirmationRequest);
            // Call send order confirmation ESP Event API
            OrderConfirmationResponseWrapper orderConfirmationResponseWrapper = getBlESPEventRestService().sendOrderConfirmation(orderConfirmationRequest);
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
