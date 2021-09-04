package com.bl.core.services.esp.order.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.esp.models.*;
import com.bl.core.services.esp.order.BlEspEventService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.web.client.RestTemplate;

public class DefaultBlEspEventService implements BlEspEventService {

    private static final Logger LOG = Logger.getLogger(DefaultBlEspEventService.class);

    private ConfigurationService configurationService;

    @Override
    public OrderConfirmationResponseWrapper sendOrderConfirmation(OrderConfirmationRequest orderConfirmationRequest) {

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
    private OrderConfirmationResponseWrapper callOrderConfirmationEventAPI(final String accessToken,
                                                                           final OrderConfirmationRequest orderConfirmationRequest) {

        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                "Order Confirmation event restful service starts.");

        try {

            final HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.setBearerAuth(accessToken);
            final HttpEntity<OrderConfirmationRequest> orderConfirmationEventRequest = new HttpEntity<>(
                    orderConfirmationRequest, headers);

            final String sendOrderConfirmationUrl =
                    getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_REST_BASE_URL)
                            + getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_EVENT_API);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Send Order Confirmation Event API request URL : {} ",
                    sendOrderConfirmationUrl);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                    "Create Order Confirmation Event API request Object : {}", getMapper().writerWithDefaultPrettyPrinter()
                            .writeValueAsString(orderConfirmationEventRequest));

            final OrderConfirmationResponse response = getRestTemplate()
                    .postForObject(sendOrderConfirmationUrl, orderConfirmationEventRequest,
                            OrderConfirmationResponse.class);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                    "Create Order Confirmation Event API response Object : {}",
                    getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(response));

            OrderConfirmationResponseWrapper orderConfirmationResponseWrapper = new OrderConfirmationResponseWrapper();
            orderConfirmationResponseWrapper.setRequestString(getMapper().writerWithDefaultPrettyPrinter()
                    .writeValueAsString(orderConfirmationEventRequest));
            if (null != response) {
                orderConfirmationResponseWrapper.setResponseString(getMapper().writerWithDefaultPrettyPrinter()
                        .writeValueAsString(response));
                orderConfirmationResponseWrapper.setEventInstanceId(response.getEventInstanceId());
            }

            return orderConfirmationResponseWrapper;

        } catch (final Exception e) {

            BlLogger.logMessage(LOG, Level.ERROR,
                    LogErrorCodeEnum.EMAIL_SUBSCRIPTION_INTEGRATION_ERROR.getCode(),
                    "Order confirmation event API call failed.", e);
        }

        return null;
    }

    /**
     * Get access token api call
     *
     * @return accesToken
     */
    private String getAccessToken() {

        String accessToken = BlCoreConstants.EMPTY_STRING;
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                "Get access token call ESP Event API starts.");

        try {

            final HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            final HttpEntity<EspAccessTokenRequest> espAccessTokenRequest = new HttpEntity<>(
                    getESPAccessTokenRequest(),
                    headers);

            final String accessTokenUrl =
                    getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_AUTH_BASE_URL)
                            + getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_ACCESS_TOKEN_API);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Get Access Token API request URL : {} ",
                    accessTokenUrl);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Get Access Token API request Object: {}",
                    getMapper().writerWithDefaultPrettyPrinter()
                            .writeValueAsString(espAccessTokenRequest));

            final EspAccessTokenResponse response = getRestTemplate()
                    .postForObject(accessTokenUrl,
                            espAccessTokenRequest,
                            EspAccessTokenResponse.class);

            BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
                    "Get Access Token API response Object : {} ",
                    getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(response));

            if (null != response && StringUtils.isBlank(response.getErrorDescription())) {
                accessToken = response.getAccessToken();

            } else if (null != response && StringUtils.isNotBlank(response.getErrorDescription())) {
                BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
                        "Error while getting the access token : {}", response.getErrorDescription());
            }

        } catch (final Exception e) {
            BlLogger.logMessage(LOG, Level.ERROR,
                    LogErrorCodeEnum.ESP_EVENT_INTEGRATION_ERROR.getCode(),
                    "Get access token call for ESP API failed.", e);
        }

        return accessToken;
    }

    /**
     * Return access token request object by populating values configured from local.properties.
     *
     * @return EspAccessTokenRequest
     */
    private EspAccessTokenRequest getESPAccessTokenRequest() {

        final EspAccessTokenRequest request = new EspAccessTokenRequest();
        request.setAccountId(
                getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_ACCOUNT_ID));
        request.setClientId(
                getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_CLIENT_ID));
        request.setClientSecret(
                getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_CLIENT_SECRET));
        request.setGrantType(
                getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_GRANT_TYPE));
        request.setScope(
                getConfigurationService().getConfiguration().getString(BlCoreConstants.ESP_EVENT_ACCESS_SCOPE));

        return request;
    }

    /**
     * To get RestTemplate
     *
     * @return RestTemplate
     */
    private RestTemplate getRestTemplate() {

        final RestTemplate restTemplate = new RestTemplate();
        final MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
        converter.setObjectMapper(new ObjectMapper());
        restTemplate.getMessageConverters().add(converter);

        return restTemplate;
    }


    public ObjectMapper getMapper() {
        return new ObjectMapper();
    }


    public ConfigurationService getConfigurationService() {
        return configurationService;
    }

    public void setConfigurationService(ConfigurationService configurationService) {
        this.configurationService = configurationService;
    }

}
