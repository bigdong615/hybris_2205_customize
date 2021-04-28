/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.tax.constants;

/**
 * Global class for all Bltaxapi constants. You can add global constants for your extension into this class.
 */
public final class BltaxapiConstants extends GeneratedBltaxapiConstants
{
	public static final String EXTENSIONNAME = "bltaxapi";

	private BltaxapiConstants()
	{
		//empty to avoid instantiating this constant class
	}

	// implement here constants used by this extension

	public static final String PLATFORM_LOGO_CODE = "bltaxapiPlatformLogo";

	public static final String BL_TAX_REST_CLIENT_LOGGING_ENABLED = "Bl.tax.rest.client.logging.enabled";

	public static final String DATE_FORMAT = "yyyy-MM-dd";
	public static final String SALESORDER = "SalesOrder";
	public static final String SALESINVOICE = "SalesInvoice";
	public static final String CA = "CA";
	public static final String USD = "USD";
	public static final String CAD = "CAD";
	public static final String RENTAL_TAX_CODE = "PRO60298";
	public static final String SALES_TAX_CODE = "PH403706";
	public static final String ORIGIN  = "Origin";
	public static final String DESTINATION = "Dest";
	public static final String COMPANY_CODE = "BL-TEST";

}
