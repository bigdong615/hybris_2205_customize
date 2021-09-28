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

	public static final String DATE_FORMAT = "dd-MM-yyyy";
	public static final String LOCAL_DATE_FORMAT = "yyyy-MM-dd";
	public static final String SALESORDER = "SalesOrder";
	public static final String SALESINVOICE = "SalesInvoice";
	public static final String USD = "USD";
	public static final String RENTAL_TAX_CODE = "PRO60298";
	public static final String SALES_TAX_CODE = "PH403706";
	public static final String LATE_FEE_TAX_CODE = "OF040005";
	public static final String REPAIR_TAX_CODE = "SR060200";
	public static final String MISSING_TAX_CODE = "P0000000";
	public static final String SHIPPING_SALES_TAX_CODE = "FR020100";
	public static final String DISCOUNT_TAX_CODE = "OD010000";
	public static final String ORIGIN  = "Origin";
	public static final String DESTINATION = "Dest";
	public static final String COMPANY_CODE = "BL-TEST";
	public static final String EMPTY_STRING = "";
	public static final String SHIPPING = "shipping";
	public static final int QTY  = 1;
	public static final String ISO_CODE = "tax.calcualtion.country.isocode";
	public static final String CAPTURE = "Capture";
	public static final String IS_AVALARA_EXCEPTION = "isAvalaraException";

}
