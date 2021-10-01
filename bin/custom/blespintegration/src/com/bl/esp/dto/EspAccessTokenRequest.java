/**
 *
 */
package com.bl.esp.dto;

import com.fasterxml.jackson.annotation.JsonProperty;


/**
 * ESPAccessTokenRequest to ESP API.
 *
 *
 */
public class EspAccessTokenRequest {

	@JsonProperty(value = "grant_type")
	private String grantType;

	@JsonProperty(value = "client_id")
	private String clientId;

	@JsonProperty(value = "client_secret")
	private String clientSecret;

	private String scope;

	@JsonProperty(value = "account_id")
	private String accountId;

	/**
	 * @return the grantType
	 */
	public String getGrantType() {
		return grantType;
	}

	/**
	 * @return the clientId
	 */
	public String getClientId() {
		return clientId;
	}

	/**
	 * @return the clientSecret
	 */
	public String getClientSecret() {
		return clientSecret;
	}

	/**
	 * @return the scope
	 */
	public String getScope() {
		return scope;
	}

	/**
	 * @return the accountId
	 */
	public String getAccountId() {
		return accountId;
	}

	/**
	 * @param grantType the grantType to set
	 */
	public void setGrantType(final String grantType) {
		this.grantType = grantType;
	}

	/**
	 * @param clientId the clientId to set
	 */
	public void setClientId(final String clientId) {
		this.clientId = clientId;
	}

	/**
	 * @param clientSecret the clientSecret to set
	 */
	public void setClientSecret(final String clientSecret) {
		this.clientSecret = clientSecret;
	}

	/**
	 * @param scope the scope to set
	 */
	public void setScope(final String scope) {
		this.scope = scope;
	}

	/**
	 * @param accountId the accountId to set
	 */
	public void setAccountId(final String accountId) {
		this.accountId = accountId;
	}

}
