package com.bl.esp.service.impl;

import com.bl.esp.dto.orderconfirmation.OrderConfirmationRequest;
import com.bl.esp.dto.orderconfirmation.ESPEventResponse;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.service.AbstractESPRestService;
import com.bl.esp.service.BlESPEventRestService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

public class DefaultBlESPEventRestService extends AbstractESPRestService implements BlESPEventRestService {

    private static final Logger LOG = Logger.getLogger(DefaultBlESPEventRestService.class);

    private ConfigurationService configurationService;

    @Override
    public ESPEventResponseWrapper sendOrderConfirmation(
        OrderConfirmationRequest orderConfirmationRequest) {

        final String accessToken;
        try {

            //1. call first api to get accesstoken
            accessToken = getAccessToken();

            //2. call second api to create contact
            if (StringUtils.isNotBlank(accessToken)) {
                return callOrderConfirmationEventAPI(accessToken, orderConfirmationRequest);
            } else {
                BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                        "No access token received from Access Token API.");
            }
        } catch (final Exception ex) {
            BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
                    LogErrorCodeEnum.EMAIL_SUBSCRIPTION_INTEGRATION_ERROR.getCode(),
                    "Error while fetching access token.",
                    ex);
        }
        return null;
    }


    /**
     * Call Order confirmation ESP Event API
     *
     * @param accessToken
     * @param orderConfirmationRequest
     * @return response - OrderConfirmationResponseWrapper
     */
    private ESPEventResponseWrapper callOrderConfirmationEventAPI(final String accessToken,
                                                                           final OrderConfirmationRequest orderConfirmationRequest) {

        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                "Order Confirmation event restful service starts.");

        try {

            final HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.setBearerAuth(accessToken);
            final HttpEntity<OrderConfirmationRequest> orderConfirmationEventRequest = new HttpEntity<>(
                    orderConfirmationRequest, headers);

            final String sendOrderConfirmationUrl = getEventApiURL();

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Send Order Confirmation Event API request URL : {} ",
                    sendOrderConfirmationUrl);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                    "Create Order Confirmation Event API request Object : {}", getMapper().writerWithDefaultPrettyPrinter()
                            .writeValueAsString(orderConfirmationEventRequest));

            final ESPEventResponse response = getRestTemplate()
                    .postForObject(sendOrderConfirmationUrl, orderConfirmationEventRequest,
                            ESPEventResponse.class);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                    "Create Order Confirmation Event API response Object : {}",
                    getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(response));

            ESPEventResponseWrapper ESPEventResponseWrapper = new ESPEventResponseWrapper();
            ESPEventResponseWrapper.setRequestString(getMapper().writerWithDefaultPrettyPrinter()
                    .writeValueAsString(orderConfirmationEventRequest));
            if (null != response) {
                ESPEventResponseWrapper.setResponseString(getMapper().writerWithDefaultPrettyPrinter()
                        .writeValueAsString(response));
                ESPEventResponseWrapper.setEventInstanceId(response.getEventInstanceId());
            }

            return ESPEventResponseWrapper;

        } catch (final Exception e) {

            BlLogger.logMessage(LOG, Level.ERROR,
                    LogErrorCodeEnum.EMAIL_SUBSCRIPTION_INTEGRATION_ERROR.getCode(),
                    "Order confirmation event API call failed.", e);
        }

        return null;
    }

    public ConfigurationService getConfigurationService() {
        return configurationService;
    }

    public void setConfigurationService(ConfigurationService configurationService) {
        this.configurationService = configurationService;
    }

}
