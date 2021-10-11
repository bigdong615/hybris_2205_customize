/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.integration.constants;

/**
 * Global class for all Blintegration constants. You can add global constants for your extension into this class.
 */
public final class BlintegrationConstants extends GeneratedBlintegrationConstants
{
	public static final String EXTENSIONNAME = "blintegration";
	public static final String AUTHORIZATION_SCOPE_PROPERTY = EXTENSIONNAME + ".oauth.scope";
	public static final String LICENSE_URL_PROPERTY = EXTENSIONNAME + ".license.url";
	public static final String TERMS_OF_SERVICE_URL_PROPERTY = EXTENSIONNAME + ".terms.of.service.url";
	public static final String LICENSE_PROPERTY = EXTENSIONNAME + ".licence";
	public static final String DOCUMENTATION_DESC_PROPERTY = EXTENSIONNAME + ".documentation.desc";
	public static final String DOCUMENTATION_TITLE_PROPERTY = EXTENSIONNAME + ".documentation.title";
	public static final String API_VERSION = "1.0.0";

	public static final String AUTHORIZATION_URL = "/authorizationserver/oauth/token";
	public static final String PASSWORD_AUTHORIZATION_NAME = "oauth2_password";
	public static final String CLIENT_CREDENTIAL_AUTHORIZATION_NAME = "oauth2_client_credentials";

	public static final String SAMPLE_MAP_STRING_KEY = "StringKey";
	public static final String SAMPLE_MAP_STRING_VALUE = "StringValue";
	public static final String SAMPLE_MAP_INTEGER_KEY = "integerKey";
	public static final int SAMPLE_MAP_INTEGER_VALUE = 10001;

	public static final String SAMPLE_LIST_STRING_VALUE = "new String";
	public static final double SAMPLE_LIST_DOUBLE_VALUE = 0.123d;

	public static final String HOST = "blintegration.host";
	public static final String HOST_DEFAULT = "hostname";

	public static final String PICKUP_ZIP_CODE = "pickUpZipCode";
	public static final String DROPOFF_ZIP_CODE = "dropOffZipCode";
	public static final String X_API_KEY = "X-Api-Key";
	public static final String CONTENT_TYPE = "Content-Type";
	public static final String AUTHORIZATION = "Authorization";
	public static final String Q_NAME_CODE = "ShipService";

	public static final String INTERNAL_SERVER_ERROR_CODE = "500";
	public static final String FAILURE_STRING = "Failure";
	public static final String ADDRESS_TYPE_UNKNOWN = "UNKNOWN";
	public static final String RESIDENTIAL_ADDRESS_TYPE_CODE = "2";
	public static final String BUSINESS_ADDRESS_TYPE_CODE = "1";

	public static final String REQUEST_OPTION = "nonvalidate";
	public static final String CUSTOMER_CONTEXT = "JAX-WS Test Client";

	public static final String CLIENT_SIDE_ERROR = "400";
	public static final String CLIENT_SIDE_ERROR_DESCRIPTION = "Client Trasportation Error";
	public static final String LABEL_IMAGE_FORMAT_CODE = "ZPL";
	public static final String LABEL_IMAGE_FORMAT_DESCRIPTION = "ZPL";
	public static final String LABEL_SPECIFICATION_HTTPUSERAGENT = "Mozilla/4.5";
	public static final String LABEL_INDICATOR = "1";
	public static final String PACKAGE_DESCRIPTION = "Package 1";
	public static final String SHIPMENT_DESCRIPTION = "Some Goods";
	public static final String UTF_8_CODE = "utf-8";
	public static final String PACAKAGING_TYPE_CODE = "02";
	public static final String PACAKAGING_TYPE_DESCIPTION = "Customer Supplied Package";
	public static final String SIGNATURE_SERVICE_CODE = "SERVICE_DEFAULT";
	public static final String CATEGORY_CODE = "CONSUMER_GOODS";
	public static final String RECIPIENT_TYPE = "RECIPIENT";
	public static final String SUCCESS = "success";
	public static final String UPS_SHIPMENT_MSG = "Shipment Call Started.....";
	public static final String RETURN_SHIPMENT_MSG = "Return Shipment Call Started.....";
	public static final String DEFAULT_WAREHOUSE_CODE = "OPTIMISED";
	public static final String WAREHOUSE_MA = "warehouse_ma";
	public static final String WAREHOUSE_CA = "warehouse_ca";
	public static final String WEIGHT_UNIT = "LB";
	public static final String DIMENSION_UNIT = "IN";
	public static final String SIMPLE_DATE_FORMAT = "hh";


	public static final String FEDEX_ACCOUNT_NUMBER = "ups.scrape.fedex.acount.number";
	public static final String FEDEX_METER_NUMBER = "ups.scrape.fedex.meter.number";
	public static final String FEDEX_PARENT_API_KEY = "fedex.parent.api.key";
	public static final String FEDEX_PARENT_API_PASSWORD = "fedex.parent.api.password";
	public static final String FEDEX_USER_API_KEY = "fedex.user.api.key";
	public static final String FEDEX_USER_API_PASSWORD = "fedex.user.api.password";
	public static final String IN_BOUND_OR_OUT_BOUND = "inboundOrOutbound";
	public static final String HYPHEN = "-";
	public static final String TRCK = "trck";
	public static final String FEDEX_API_URL = "fedex.api.url";


	private BlintegrationConstants()
	{
		//empty to avoid instantiating this constant class
	}

	// implement here constants used by this extension
}
