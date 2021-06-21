/**
 *
 */
package com.bl.core.subscription.service.impl;

import de.hybris.platform.servicelayer.config.ConfigurationService;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.web.client.RestTemplate;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.subscription.models.ContactRequest;
import com.bl.core.subscription.models.ContactResponse;
import com.bl.core.subscription.models.SubscriptionAccessTokenRequest;
import com.bl.core.subscription.models.SubscriptionAccessTokenResponse;
import com.bl.core.subscription.service.BlEmailSubscriptionRestService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import com.fasterxml.jackson.databind.ObjectMapper;


/**
 * Restful service to subscribe emails.
 *
 * @author Sunil Sahu
 */
public class DefaultBlEmailSubscriptionRestService implements BlEmailSubscriptionRestService {

	private static final Logger LOG = Logger.getLogger(DefaultBlEmailSubscriptionRestService.class);
	private static final String SUBSCRIPTION_AUTH_BASE_URL = "email.subscription.auth.base.url";//   https://mcz111jg0kwv-qyxpw8rh1dff6j8.auth.marketingcloudapis.com
	private static final String SUBSCRIPTION_REST_BASE_URL = "email.subscription.rest.base.url";//   https://mcz111jg0kwv-qyxpw8rh1dff6j8.rest.marketingcloudapis.com
	private static final String GET_ACCESS_TOKEN_API = "email.subscription.accesstoken.endpoint.url"; //   /v2/token
	private static final String CREATE_CONTACT_API = "email.subscription.create.contact.endpoint.url";//   /contacts/v1/contacts
	private static final String SUBSCRIPTION_CLIENT_ID = "email.subscription.client.id";//   lvnx2e631tweqcvn6uup7fjl
	private static final String SUBSCRIPTION_CLIENT_SECRET = "email.subscription.client.secret";//   82YtBWAokGenYTQf1UuZldBJ
	private static final String SUBSCRIPTION_ACCOUNT_ID = "email.subscription.account.id";//   515009598
	private static final String SUBSCRIPTION_GRANT_TYPE = "email.subscription.grant.type";//   client_credentials
	private static final String SUBSCRIPTION_ACCESS_SCOPE = "email.subscription.access.scope";//  email_read email_write email_send

	private ConfigurationService configurationService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public ContactResponse subscribeEmail(final ContactRequest contactRequest) {

		//1. call first api to get accesstoken
		final String accessToken = getAccessToken();

		//2. call second api to create contact
		return createContact(accessToken, contactRequest);
	}

	/**
	 * Get access token api call
	 *
	 * @return accesToken
	 */
	private String getAccessToken() {

		String accessToken = "";
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
				"Get access token call for email subscription starts.");

		try {

			final HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			final HttpEntity<SubscriptionAccessTokenRequest> subscriptionAccessTokenRequest = new HttpEntity<>(
					getSubscriptionAccessTokenRequest(),
					headers);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Request for get access token api call :- " + subscriptionAccessTokenRequest);

			final String accessTokenUrl =
					getConfigurationService().getConfiguration().getString(SUBSCRIPTION_AUTH_BASE_URL)
							+ getConfigurationService().getConfiguration().getString(GET_ACCESS_TOKEN_API);

			BlLogger
					.logFormatMessageInfo(LOG, Level.DEBUG, "Get access token rest url :- " + accessTokenUrl);

			final SubscriptionAccessTokenResponse response = restTemplate().postForObject(accessTokenUrl,
					subscriptionAccessTokenRequest,
					SubscriptionAccessTokenResponse.class);

			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Response from get access token api call :- " + response);

			if (null != response && StringUtils.isBlank(response.getErrorDescription())) {
				accessToken = response.getAccessToken();

			} else if (null != response && StringUtils.isNotBlank(response.getErrorDescription())){
				BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
						"Got error response for getting acceess token, error is : [{%s}]",
						response.getErrorDescription());
			}

		} catch (final Exception e) {
			BlLogger.logMessage(LOG, Level.ERROR,
					LogErrorCodeEnum.EMAIL_SUBSCRIPTION_INTEGRATION_ERROR.getCode(),
					"Get access token call for email subscription failed.", e);
		}

		return accessToken;
	}

	/**
	 * Create contact api call
	 *
	 * @param accessToken
	 * @param contactRequest
	 * @return response - ContactResponse
	 */
	private ContactResponse createContact(final String accessToken,
			final ContactRequest contactRequest) {

		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
				"Create contact restful service for email subscription starts.");

		try {

			final HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.APPLICATION_JSON);
			headers.setBearerAuth(accessToken);
			final HttpEntity<ContactRequest> subscriptionCreateContactRequest = new HttpEntity<>(
					contactRequest, headers);

			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Request for create contact api call :- " + subscriptionCreateContactRequest);

			final String createContactUrl =
					getConfigurationService().getConfiguration().getString(SUBSCRIPTION_REST_BASE_URL)
							+ getConfigurationService().getConfiguration().getString(CREATE_CONTACT_API);

			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Create contact for subscription rest url :- " + createContactUrl);

			final ContactResponse response = restTemplate()
					.postForObject(createContactUrl, subscriptionCreateContactRequest,
							ContactResponse.class);

			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Response got from create contact api call :- " + response);

			if (null != response
					&& StringUtils.equalsIgnoreCase(BlCoreConstants.SUBSCRIPTION_API_OPERATION_STATUS,
					response.getOperationStatus())) {

				return response;
			} else {

				BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Got error response for rest url [{%s}]",
						createContactUrl);
			}

		} catch (final Exception e) {

			BlLogger.logMessage(LOG, Level.ERROR,
					LogErrorCodeEnum.EMAIL_SUBSCRIPTION_INTEGRATION_ERROR.getCode(),
					"Create contact call for email subscription failed.", e);
		}

		return null;
	}

	/**
	 * To get RestTemplate
	 *
	 * @return RestTemplate
	 */
	private RestTemplate restTemplate() {

		final RestTemplate restTemplate = new RestTemplate();
		final MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
		converter.setObjectMapper(new ObjectMapper());
		restTemplate.getMessageConverters().add(converter);

		return restTemplate;
	}


	/**
	 * Return access token request object by populating values configured from local.properties.
	 *
	 * @return SubscriptionAccessTokenRequest
	 */
	private SubscriptionAccessTokenRequest getSubscriptionAccessTokenRequest() {

		final SubscriptionAccessTokenRequest request = new SubscriptionAccessTokenRequest();
		request.setAccountId(
				getConfigurationService().getConfiguration().getString(SUBSCRIPTION_ACCOUNT_ID));
		request.setClientId(
				getConfigurationService().getConfiguration().getString(SUBSCRIPTION_CLIENT_ID));
		request.setClientSecret(
				getConfigurationService().getConfiguration().getString(SUBSCRIPTION_CLIENT_SECRET));
		request.setGrantType(
				getConfigurationService().getConfiguration().getString(SUBSCRIPTION_GRANT_TYPE));
		request.setScope(
				getConfigurationService().getConfiguration().getString(SUBSCRIPTION_ACCESS_SCOPE));

		return request;
	}

	protected ConfigurationService getConfigurationService() {
		return configurationService;
	}

	public void setConfigurationService(final ConfigurationService configurationService) {
		this.configurationService = configurationService;
	}

}
