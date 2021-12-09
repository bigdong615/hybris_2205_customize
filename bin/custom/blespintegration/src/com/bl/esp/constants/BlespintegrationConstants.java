/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.esp.constants;

/**
 * Global class for all Blespintegration constants. You can add global constants for your extension into this class.
 */
public class BlespintegrationConstants extends GeneratedBlespintegrationConstants
{
	public static final String EXTENSIONNAME = "blespintegration";
	public static final String EMPTY_STRING = "";
	public static final String ESP_EVENT_REST_BASE_URL = "esp.event.rest.base.url";//   https://mcz111jg0kwv-qyxpw8rh1dff6j8.rest.marketingcloudapis.com
	public static final String ESP_EVENT_AUTH_BASE_URL = "esp.event.auth.base.url";//   https://mcz111jg0kwv-qyxpw8rh1dff6j8.auth.marketingcloudapis.com
	public static final String ESP_EVENT_ACCESS_TOKEN_API = "esp.event.accesstoken.endpoint.url"; //   /v2/token
	public static final String ESP_EVENT_EVENT_API = "esp.event.event.endpoint.url";//   /interaction/v1/events
	public static final String ESP_EVENT_CLIENT_ID = "esp.event.client.id";//   lvnx2e631tweqcvn6uup7fjl
	public static final String ESP_EVENT_CLIENT_SECRET = "esp.event.client.secret";//   82YtBWAokGenYTQf1UuZldBJ
	public static final String ESP_EVENT_ACCOUNT_ID = "esp.event.account.id";//   515009598
	public static final String ESP_EVENT_GRANT_TYPE = "esp.event.grant.type";//   client_credentials
	public static final String ESP_EVENT_ACCESS_SCOPE = "esp.event.access.scope"; //  list_and_subscribers_read list_and_subscribers_write journeys_read


	public static final String SFTPHOST = "host:IP";
	public static final int SFTPPORT = 22;
	public static final String SFTPUSER = "ftp.user.name";
	public static final String SFTPPASS = "ftp.user.password";
	public static final String SFTPWORKINGDIR = "ftp.file.path";


	private BlespintegrationConstants()
	{
		//empty to avoid instantiating this constant class
	}

	// implement here constants used by this extension

	public static final String PLATFORM_LOGO_CODE = "blespintegrationPlatformLogo";
}
