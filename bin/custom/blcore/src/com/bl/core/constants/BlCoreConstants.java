/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.constants;

/**
 * Global class for all BlCore constants. You can add global constants for your extension into this class.
 */
public final class BlCoreConstants
{
	public static final String EXTENSIONNAME = "blcore";

	// implement here constants used by this extension
	public static final String QUOTE_BUYER_PROCESS = "quote-buyer-process";
	public static final String QUOTE_SALES_REP_PROCESS = "quote-salesrep-process";
	public static final String QUOTE_USER_TYPE = "QUOTE_USER_TYPE";
	public static final String QUOTE_SELLER_APPROVER_PROCESS = "quote-seller-approval-process";
	public static final String QUOTE_TO_EXPIRE_SOON_EMAIL_PROCESS = "quote-to-expire-soon-email-process";
	public static final String QUOTE_EXPIRED_EMAIL_PROCESS = "quote-expired-email-process";
	public static final String QUOTE_POST_CANCELLATION_PROCESS = "quote-post-cancellation-process";
	public static final String EMPTY_STRING = "";
	public static final String CODE = "code";
	public static final String BRANDS = "Brands";
	public static final String DEFAULT_SORT_CODE = "default.sorting.code";
	public static final String FACTED_CATEGORY_NAME = "facted.plp.category.name";

	//Inventory Availability
	public static final String ACTIVE = "active";
	public static final String PRODUCT_CODE = "productCode";
	public static final String START_DATE = "startDate";
	public static final String END_DATE = "endDate";
	public static final String WAREHOUSES = "warehouses";
	public static final int END_HOURS = 23;
	public static final int END_MINUTES = 59;
	public static final int END_SECONDS = 59;
	public static final int START_HOURS = 0;
	public static final int START_MINUTES = 0;
	public static final int START_SECONDS = 0;
	public static final long MAX_TOTAL = 20L;
	public static final long MIN_TOTAL = 5L;
	public static final long LOW_AVAILABILITY = 2L;
	public static final long ZERO_AVAILABILITY = 0L;

	private BlCoreConstants()
	{
		//empty
	}
}
