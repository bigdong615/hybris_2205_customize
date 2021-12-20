package com.bl.esp.service;

import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.dto.EspAccessTokenRequest;
import com.bl.esp.dto.EspAccessTokenResponse;
import com.bl.esp.dto.orderconfirmation.ESPEventResponse;
import com.bl.esp.dto.orderconfirmation.ESPEventResponseWrapper;
import com.bl.esp.exception.BlESPIntegrationException;
import com.bl.esp.order.ESPEventCommonRequest;
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

/**
 * This class provides common functionalities to ESP events.
 *
 * @author Neeraj Singh
 */
public abstract class AbstractESPRestService<T extends ESPEventCommonRequest> {

  private static final Logger LOG = Logger.getLogger(AbstractESPRestService.class);
  private ConfigurationService configurationService;

  /**
   * To get RestTemplate
   *
   * @return RestTemplate
   */
  protected RestTemplate getRestTemplate() {

    final RestTemplate restTemplate = new RestTemplate();
    final MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
    converter.setObjectMapper(new ObjectMapper());
    restTemplate.getMessageConverters().add(converter);
    return restTemplate;
  }

  protected ObjectMapper getMapper() {
    return new ObjectMapper();
  }

  /**
   * Get Event api url.
   * @return Event api url.
   */
  protected String getEventApiURL() {
    final String espEventRestBaseURL = getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_REST_BASE_URL);
    final String espEventRestEndpointURL = getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_EVENT_API);
    if (StringUtils.isBlank(espEventRestBaseURL) || StringUtils.isBlank(espEventRestEndpointURL))
    {
      BlLogger.logMessage(LOG , Level.ERROR , "ESP Event Rest API URL And Endpoint is missing");
      throw new BlESPIntegrationException("ESP Event Rest API URL And Endpoint is missing" , LogErrorCodeEnum.ESP_EVENT_REST_API_URL_AND_ENDPOINT_IS_MISSING.getCode());
    }
    return new StringBuilder(espEventRestBaseURL).append(espEventRestEndpointURL).toString();
  }

  /**
   * It makes access token api call to to fetch the access token.
   * @return accesToken
   */
  protected String getAccessToken() {

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Get access token ESP Event API starts.");

    try {

      final HttpHeaders headers = new HttpHeaders();
      headers.setContentType(MediaType.APPLICATION_JSON);
      final EspAccessTokenRequest espAccessTokenRequestObjects = getESPAccessTokenRequest();
      if (StringUtils.isBlank(espAccessTokenRequestObjects.getAccountId()) ||  StringUtils.isBlank(espAccessTokenRequestObjects.getClientId()) || StringUtils.isBlank(espAccessTokenRequestObjects.getClientSecret()) || StringUtils.isBlank(espAccessTokenRequestObjects.getGrantType()) || StringUtils.isBlank(espAccessTokenRequestObjects.getScope())){
        BlLogger.logMessage(LOG , Level.ERROR , "Access token objects are  empty" );
        throw new BlESPIntegrationException("Access token objects are  empty" , LogErrorCodeEnum.ESP_EVENT_ACCESS_TOKEN_OBJECT_IS_MISSING
            .getCode()); //
      }
      final HttpEntity<EspAccessTokenRequest> espAccessTokenRequest = new HttpEntity<>(espAccessTokenRequestObjects, headers);

      final String espEventBaseURL = getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_AUTH_BASE_URL);
      final String espEventAccessTokenURL = getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_ACCESS_TOKEN_API);
      if (StringUtils.isBlank(espEventBaseURL) || StringUtils.isBlank(espEventAccessTokenURL))
      {
        BlLogger.logMessage(LOG , Level.ERROR , "Event Base URL Or Access token URL  are  empty" );
        throw new BlESPIntegrationException("Event Base URL Or Access token URL  are  empty" , LogErrorCodeEnum.ESP_EVENT_BASE_URL_OR_ACCESS_TOKEN_URL_IS_MISSING
            .getCode());
      }
      final String accessTokenUrl = new StringBuilder(espEventBaseURL).append(espEventAccessTokenURL).toString();

      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Access Token API request URL : {} ", accessTokenUrl);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Access Token API request Object: {}", getMapper().writerWithDefaultPrettyPrinter()
              .writeValueAsString(espAccessTokenRequest));

      final EspAccessTokenResponse response = getRestTemplate().postForObject(accessTokenUrl, espAccessTokenRequest, EspAccessTokenResponse.class);

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Access Token API response Object : {} ", getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(response));

      if (null != response && StringUtils.isBlank(response.getErrorDescription())) {
       return response.getAccessToken();

      } else if (null != response && StringUtils.isNotBlank(response.getErrorDescription())) {
        BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
            "Error while getting the access token : {}", response.getErrorDescription());
        throw new BlESPIntegrationException("Event Access token is empty" , LogErrorCodeEnum.ESP_EVENT_ACCESS_TOKEN_EMPTY
            .getCode());
      }

    } catch (final Exception e) {
      BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.ESP_EVENT_INTEGRATION_ERROR.getCode(), "Get access token call for ESP API failed.", e);
      throw new BlESPIntegrationException("Event access token call for ESP API failed." , LogErrorCodeEnum.ESP_EVENT_ACCESS_TOKEN_API_FAILED_ERROR.getCode());
    }
    return null;
  }

  /**
   * Return access token request object by populating values configured from project.properties.
   * @return EspAccessTokenRequest
   */
  private EspAccessTokenRequest getESPAccessTokenRequest() {

    final EspAccessTokenRequest request = new EspAccessTokenRequest();
    request.setAccountId(getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_ACCOUNT_ID));
    request.setClientId(getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_CLIENT_ID));
    request.setClientSecret(getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_CLIENT_SECRET));
    request.setGrantType(getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_GRANT_TYPE));
    request.setScope(getConfigurationService().getConfiguration().getString(BlespintegrationConstants.ESP_EVENT_ACCESS_SCOPE));
    return request;
  }

  public ESPEventResponseWrapper getTokenAndTriggerEvent(T eventRequest) {
      final String accessToken = getAccessToken();
      if (StringUtils.isBlank(accessToken)){
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "No access token received from Access Token API.");
        throw new BlESPIntegrationException("No access token received from Access Token API." , LogErrorCodeEnum.ESP_EVENT_NO_ACCESS_TOKEN_RECEIVED_FROM_API_ERROR.getCode());
      }else{
        return triggerEvent(accessToken, eventRequest);
      }
  }


  /**
   * This method triggers the ESP Event API
   *
   * @param accessToken
   * @param eventRequest
   * @return response - ESPEventResponseWrapper
   */
  private ESPEventResponseWrapper triggerEvent(final String accessToken,
      final T eventRequest) {

    BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Event restful service starts.");

    String requestString = StringUtils.EMPTY;
    try {

      final HttpHeaders headers = new HttpHeaders();
      headers.setContentType(MediaType.APPLICATION_JSON);
      headers.setBearerAuth(accessToken);
      final HttpEntity<T> orderConfirmationEventRequest = new HttpEntity<T>(eventRequest, headers);

      requestString = getMapper().writerWithDefaultPrettyPrinter()
          .writeValueAsString(orderConfirmationEventRequest);
      System.out.println(requestString);

      final String sendOrderConfirmationUrl = getEventApiURL();
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Triggering Event API request URL : {} ", sendOrderConfirmationUrl);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Create Event API request Object : {}", getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(orderConfirmationEventRequest));

      final ESPEventResponse response = getRestTemplate().postForObject(sendOrderConfirmationUrl, orderConfirmationEventRequest, ESPEventResponse.class);

      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Create Event API response Object : {}", getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(response));

      final ESPEventResponseWrapper espEventResponseWrapper = new ESPEventResponseWrapper();
      espEventResponseWrapper.setRequestString(requestString);

      if (null == response) {
        BlLogger.logMessage(LOG , Level.ERROR , "Event Service Response is null" );
        throw new BlESPIntegrationException("Event Service Response is null" , LogErrorCodeEnum.ESP_EVENT_SERVICE_RESPONSE_NULL
            .getCode(), requestString);
      }else {
        espEventResponseWrapper.setResponseString(getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(response));
        espEventResponseWrapper.setEventInstanceId(response.getEventInstanceId());
        return espEventResponseWrapper;
      }

    } catch (final Exception e) {
      BlLogger.logMessage(LOG, Level.ERROR,  "Event API call failed.", e);
      throw new BlESPIntegrationException("Event API call failed." , LogErrorCodeEnum.ESP_EVENT_API_FAILED_ERROR.getCode(), requestString);
    }
  }

  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  public void setConfigurationService(
      ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }
}
