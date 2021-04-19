/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.constants;

import java.math.RoundingMode;


/**
 * Global class for all BlCore constants. You can add global constants for your extension into this class.
 */
public final class BlCoreConstants extends GeneratedBlCoreConstants
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
	public static final String BL_PAGE_TYPE = "blPageType";
	public static final String USED_GEAR = "Used Gear";
	public static final String USED_GEAR_CODE = "usedGear";
	public static final String RATIO = ":";
	public static final String DELIMETER = "|";
	public static final String ALL_CATEGORIES = "allCategories";
	public static final String USED_NEW_ARRIVALS = "Used-NewArrivals";
	public static final String DEFAULT_SORT_NEWEST_CODE = "default.sort.newest";
	public static final String USED_VIDEO = "Used-Video";
	public static final String FOR_RENT = "forRent";
	public static final String FOR_SALE = "forSale";
	public static final String ITEM_TYPE = "itemtype";
	public static final String BLPRODUCT = "BlProduct";
	public static final String USED = "Used";
	public static final String CATEGORY_MAP = "usedgear.rentalgear.map";
	public static final String RENTAL_GEAR = "rentalgear";
	public static final String IS_VIDEO = "isVideo";
	public static final String IS_NEW = "isNew";
	public static final String TRUE = "true";
	public static final String FALSE = "false";
	public static final String BASE_SITE_UID = "{baseSite-uid}";
	public static final String CATEGORY_PATTERN_CODE = "{category-code}";
	public static final String CATALOG_ID = "{catalog-id}";
	public static final String CATALOG_VERSION = "{catalogVersion}";
	public static final String REPLACE_STRING = "%20";
	public static final String MOST_POPULAR = "mostPopular";
	public static final String STAFF_PICK = "staffPick";
	public static final String UPCOMING = "upComing";
	public static final String NEW = "New";
	public static final String POPULAR = "Popular";
	public static final String GREAT_VALUE = "greatValue";
	public static final String GREAT_VALUE_STRING = "Great Value";
	public static final String STAFF_PICK_STRING = "Staff Pick";
	public static final String CLEAR_ALL_QUERY = "clearAllQuery";
	public static final String PARENT_CATEGORY = "{parentcategory}";
	public static final String RENTAL_CLEAR_ALL = "/rent/category/rentalgear";
	public static final String RENTAL_GEAR_MAP = "key.rentalgear.new";
	public static final String CLEAR_BRAND = "brandClear";
	public static final String SUPER_CATEGORY = "superCategory";

	public static final String SEVEN_DAY_PRICE = "7";
	public static final String PIECES = "pieces";
	public static final String USD = "USD";

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

	public static final int MINIMUM_RENTAL_DAYS = 3;
	public static final int MAXIMUM_RENTAL_DAYS = 90;
	public static final int DEFAULT_RENTAL_DAY = 7;
	public static final String SELECTED_RENTAL_DAYS = "numberOfDays";

	public static final RoundingMode ROUNDING_MODE = RoundingMode.DOWN;
	public static final String PRICE_ROW = "pricerow";
	public static final int DECIMAL_PRECISION = 2;

	public static final String DEFAULT_ZONE_ID = "UTC";

	public static final String FACTED_USED_CATEGORY_NAME = "facted.used.plp.category.name";
	public static final String USED_CLEAR_ALL = "/buy/category/usedgear";
	public static final String USED_SUPER_CATEGORY = "usedsuperCategory";
	public static final String CLEAR_USED = "usedClear";


	private BlCoreConstants()
	{
		//empty
	}
}
