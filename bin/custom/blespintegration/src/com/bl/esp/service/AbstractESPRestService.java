package com.bl.esp.service;

import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.dto.EspAccessTokenRequest;
import com.bl.esp.dto.EspAccessTokenResponse;
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
public abstract class AbstractESPRestService {

  private static final Logger LOGGER = Logger.getLogger(AbstractESPRestService.class);
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
    return getConfigurationService().getConfiguration()
        .getString(BlespintegrationConstants.ESP_EVENT_REST_BASE_URL)
        + getConfigurationService().getConfiguration()
        .getString(BlespintegrationConstants.ESP_EVENT_EVENT_API);
  }

  /**
   * It makes access token api call to to fetch the access token.
   * @return accesToken
   */
  protected String getAccessToken() {

    String accessToken = BlespintegrationConstants.EMPTY_STRING;
    BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG,
        "Get access token call ESP Event API starts.");

    try {

      final HttpHeaders headers = new HttpHeaders();
      headers.setContentType(MediaType.APPLICATION_JSON);
      final HttpEntity<EspAccessTokenRequest> espAccessTokenRequest = new HttpEntity<>(
          getESPAccessTokenRequest(),
          headers);

      final String accessTokenUrl =
          getConfigurationService().getConfiguration()
              .getString(BlespintegrationConstants.ESP_EVENT_AUTH_BASE_URL)
              + getConfigurationService().getConfiguration()
              .getString(BlespintegrationConstants.ESP_EVENT_ACCESS_TOKEN_API);

      BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Get Access Token API request URL : {} ",
          accessTokenUrl);

      BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Get Access Token API request Object: {}",
          getMapper().writerWithDefaultPrettyPrinter()
              .writeValueAsString(espAccessTokenRequest));

      final EspAccessTokenResponse response = getRestTemplate()
          .postForObject(accessTokenUrl,
              espAccessTokenRequest,
              EspAccessTokenResponse.class);

      BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG,
          "Get Access Token API response Object : {} ",
          getMapper().writerWithDefaultPrettyPrinter().writeValueAsString(response));

      if (null != response && StringUtils.isBlank(response.getErrorDescription())) {
        accessToken = response.getAccessToken();

      } else if (null != response && StringUtils.isNotBlank(response.getErrorDescription())) {
        BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR,
            "Error while getting the access token : {}", response.getErrorDescription());
      }

    } catch (final Exception e) {
      BlLogger.logMessage(LOGGER, Level.ERROR,
          LogErrorCodeEnum.ESP_EVENT_INTEGRATION_ERROR.getCode(),
          "Get access token call for ESP API failed.", e);
    }

    return accessToken;
  }

  /**
   * Return access token request object by populating values configured from local.properties.
   * @return EspAccessTokenRequest
   */
  private EspAccessTokenRequest getESPAccessTokenRequest() {

    final EspAccessTokenRequest request = new EspAccessTokenRequest();
    request.setAccountId(
        getConfigurationService().getConfiguration()
            .getString(BlespintegrationConstants.ESP_EVENT_ACCOUNT_ID));
    request.setClientId(
        getConfigurationService().getConfiguration()
            .getString(BlespintegrationConstants.ESP_EVENT_CLIENT_ID));
    request.setClientSecret(
        getConfigurationService().getConfiguration()
            .getString(BlespintegrationConstants.ESP_EVENT_CLIENT_SECRET));
    request.setGrantType(
        getConfigurationService().getConfiguration()
            .getString(BlespintegrationConstants.ESP_EVENT_GRANT_TYPE));
    request.setScope(
        getConfigurationService().getConfiguration()
            .getString(BlespintegrationConstants.ESP_EVENT_ACCESS_SCOPE));

    return request;
  }

  public ConfigurationService getConfigurationService() {
    return configurationService;
  }

  public void setConfigurationService(
      ConfigurationService configurationService) {
    this.configurationService = configurationService;
  }
}
