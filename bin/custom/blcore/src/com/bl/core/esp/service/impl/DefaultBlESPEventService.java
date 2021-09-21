package com.bl.core.esp.service.impl;

import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.model.BlStoredEspEventModel;
import com.bl.core.populators.BlOrderConfirmationRequestPopulator;
import com.bl.esp.dto.orderconfirmation.OrderConfirmationRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.enums.ESPEventStatus;
import com.bl.esp.enums.EspEventTypeEnum;
import com.bl.esp.service.BlESPEventRestService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.Objects;
import org.apache.commons.lang.StringUtils;

public class DefaultBlESPEventService implements BlESPEventService {

    private BlOrderConfirmationRequestPopulator blOrderConfirmationRequestPopulator;
    private BlESPEventRestService blESPEventRestService;
    private ModelService modelService;

    @Override
    public void sendOrderConfirmation(OrderModel orderModel) {
        if (Objects.nonNull(orderModel)) {
            final OrderConfirmationRequest orderConfirmationRequest = new OrderConfirmationRequest();
            getBlOrderConfirmationRequestPopulator().populate(orderModel, orderConfirmationRequest);
            // Call send order confirmation ESP Event API
            final ESPEventResponseWrapper ESPEventResponseWrapper = getBlESPEventRestService().sendOrderConfirmation(orderConfirmationRequest);
            // Save send order confirmation ESP Event Detail
            persistESPEventDetail(ESPEventResponseWrapper, EspEventTypeEnum.ORDER_CONFIRM,orderModel.getCode(),
                ESPEventStatus.SUCCESS);
        }
    }

    private void persistESPEventDetail(final ESPEventResponseWrapper ESPEventResponseWrapper, final EspEventTypeEnum eventTypeEnum, final String orderCode, final ESPEventStatus status) {
        if (Objects.nonNull(ESPEventResponseWrapper)
                && StringUtils.isNotBlank(ESPEventResponseWrapper.getRequestString())
                && StringUtils.isNotBlank(ESPEventResponseWrapper.getResponseString())) {
            BlStoredEspEventModel blStoredEspEventModel = new BlStoredEspEventModel();
            blStoredEspEventModel.setEventInstanceId(ESPEventResponseWrapper.getEventInstanceId());
            blStoredEspEventModel.setRequestString(ESPEventResponseWrapper.getRequestString());
            blStoredEspEventModel.setResponseString(ESPEventResponseWrapper.getResponseString());
            blStoredEspEventModel.setEventType(eventTypeEnum);
            blStoredEspEventModel.setStatus(status);
            blStoredEspEventModel.setOrderCode(orderCode);
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
