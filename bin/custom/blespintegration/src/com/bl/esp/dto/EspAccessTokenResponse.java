/**
 *
 */
package com.bl.esp.dto;

import com.fasterxml.jackson.annotation.JsonProperty;


/**
 * ESPAccessTokenResponse to ESP API.
 *
 *
 */
public class EspAccessTokenResponse {

	@JsonProperty(value = "access_token")
	private String accessToken;

	@JsonProperty(value = "expires_in")
	private int expiresIn;

	@JsonProperty(value = "token_type")
	private String tokenType;

	@JsonProperty(value = "rest_instance_url")
	private String restInstanceUrl;

	@JsonProperty(value = "soap_instance_url")
	private String soapInstanceUrl;

	private String scope;

	private String error;

	@JsonProperty(value = "error_description")
	private String errorDescription;

	@JsonProperty(value = "error_uri")
	private String errorUri;

	/**
	 * @return the accessToken
	 */
	public String getAccessToken() {
		return accessToken;
	}

	/**
	 * @return the expiresIn
	 */
	public int getExpiresIn() {
		return expiresIn;
	}

	/**
	 * @return the tokenType
	 */
	public String getTokenType() {
		return tokenType;
	}

	/**
	 * @return the restInstanceUrl
	 */
	public String getRestInstanceUrl() {
		return restInstanceUrl;
	}

	/**
	 * @return the soapInstanceUrl
	 */
	public String getSoapInstanceUrl() {
		return soapInstanceUrl;
	}

	/**
	 * @return the scope
	 */
	public String getScope() {
		return scope;
	}

	/**
	 * @return the error
	 */
	public String getError() {
		return error;
	}

	/**
	 * @return the errorDescription
	 */
	public String getErrorDescription() {
		return errorDescription;
	}

	/**
	 * @return the errorUri
	 */
	public String getErrorUri() {
		return errorUri;
	}

	/**
	 * @param accessToken the accessToken to set
	 */
	public void setAccessToken(final String accessToken) {
		this.accessToken = accessToken;
	}

	/**
	 * @param expiresIn the expiresIn to set
	 */
	public void setExpiresIn(final int expiresIn) {
		this.expiresIn = expiresIn;
	}

	/**
	 * @param tokenType the tokenType to set
	 */
	public void setTokenType(final String tokenType) {
		this.tokenType = tokenType;
	}

	/**
	 * @param restInstanceUrl the restInstanceUrl to set
	 */
	public void setRestInstanceUrl(final String restInstanceUrl) {
		this.restInstanceUrl = restInstanceUrl;
	}

	/**
	 * @param soapInstanceUrl the soapInstanceUrl to set
	 */
	public void setSoapInstanceUrl(final String soapInstanceUrl) {
		this.soapInstanceUrl = soapInstanceUrl;
	}

	/**
	 * @param scope the scope to set
	 */
	public void setScope(final String scope) {
		this.scope = scope;
	}

	/**
	 * @param error the error to set
	 */
	public void setError(final String error) {
		this.error = error;
	}

	/**
	 * @param errorDescription the errorDescription to set
	 */
	public void setErrorDescription(final String errorDescription) {
		this.errorDescription = errorDescription;
	}

	/**
	 * @param errorUri the errorUri to set
	 */
	public void setErrorUri(final String errorUri) {
		this.errorUri = errorUri;
	}

}
