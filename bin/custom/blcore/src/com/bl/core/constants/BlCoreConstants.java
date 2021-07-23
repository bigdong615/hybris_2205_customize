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

	public static final String COUPON_APPLIED_MSG = "coupon_applied_msg";


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
	public static final String RENTAL_SUMMARY_DATE = "rentalSummary";
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
	public static final String SERIAL_PRODUCT_CODE = "serialProductCode";
	public static final String START_DATE = "startDate";
	public static final String END_DATE = "endDate";
	public static final String WAREHOUSES = "warehouses";
	public static final String RESERVED_STATUS = "reservedStatus";
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
	public static final String COOKIE_NAME_FOR_DATE = "selectedDate";
	public static final int PAIR_OF_DATES = 2;
	public static final String SEPARATOR = "\\|";
	public static final String SELECTED_DATE_MAP = "selectedDateMap";
	public static final String DATE_FORMAT = "dd-MM-yyyy";
	public static final String SELECTED_DATE = "selectedDate";

	public static final String FACTED_USED_GEAR_CATEGORY_NAME = "facted.usedgear.plp.category.name";
	public static final String USED_GEAR_CLEAR_ALL = "/buy/category/usedgear";
	public static final String USED_GEAR_SUPER_CATEGORY = "usedsuperCategory";
	public static final String CLEAR_USED_GEAR_CATEGORY = "usedClear";

	public static final String DEFAULT_DAYS = "7";
	public static final String DISCONTINUED = "discontinued";
	public static final String APPROVED = "approved";

	public static final String RENTAL_DATE_FORMAT = "MMM dd";
	public static final String PRODUCT_CATALOG = "catalog";
	public static final String VERSION = "version";
	public static final String STAGED = "Staged";
	public static final String CATALOG_VALUE = "blProductCatalog";

	public static final String VIDEO = "VIDEO";
	public static final String PHOTO = "PHOTO";
	public static final String GEAR_GUARD_PRO = "GEAR_GUARD_PRO";
	public static final String GEAR_GUARD_PRO_FULL = "gearguardpro";
	public static final String GEAR_GUARD = "gearguard";
	public static final String NO_GEAR_GUARD = "nogearguard";

	public static final int ONE_RENTAL_DAY = 1;
	public static final int THREE_RENTAL_DAYS = 3;

	public static final int DIVIDE_BY_HUNDRED = 100;

	public static final int CONDITION_RATING_HIGH = 9;
	public static final int CONDITION_RATING_MEDIUM = 8;
	public static final int CONDITION_RATING_LOW = 7;

	public static final String PRICE_ASC = "price-asc";
	public static final String PRICE_DESC = "price-desc";
	public static final String MIN_SERIAL_PRICE = "minSerialfinalSalePrice";
	public static final String PRICE_VALUE = "priceValue";

	public static final String INTERNAL_SERVER_ERROR_CODE = "500";
	public static final String FAILURE_STRING = "Failure";
	public static final String ADDRESS_TYPE_UNKNOWN = "UNKNOWN";
	public static final String RESIDENTIAL_ADDRESS_TYPE_CODE = "2";
	public static final String BUSINESS_ADDRESS_TYPE_CODE = "1";

	public static final String BL_IMAGE = "blimage";
	public static final String MEDIA_FORMAT = "300Wx300H";

	public static final String ISO_CODE_SHORT = "isoCodeShort";
	public static final String CODES = "codes";
	public static final String ONLINE = "Online";
	public static final String SERIAL_PRODUCT_CODES = "serialProductCodes";
	public static final String PRODUCT_CODES = "productCodes";
	public static final String CONSIGNMENT_PROCESS_PREFIX = "cons";

	public static final String LENSES = "lenses";
	public static final String CAMERAS = "cameras";
	public static final String PRODUCTION = "production";
	public static final int SKIP_TWO_DAYS = 2;
	public static final String GIFT_CARD = "giftcard-";
	public static final String GIFT_CARD_EMAIL_PROCESS = "giftCardEmailProcess";
	public static final String HYPHEN = "-";
	public static final String IS_AUTHORISED = "isAuthorized";
	public static final String COUPON_ID = "couponId";

	//Promotion Condition Translator
	public static final String RENTAL_ARRIVAL_DATE = "rentalArrivalDate";
	public static final String OPERATOR = "operator";
	public static final String IS_RENTAL_CART = "rentalCart";
	public static final String RENTAL_VALUE = "value";
	public static final String RENTAL_DURATION = "rentalDuration";
	public static final String RENTAL_DURATION_OPERATOR = "rentalDurationOperator";
	public static final String RENTAL_DURATION_DAYS = "rentalDurationDays";
	public static final String SUCCESS = "Success";
	public static final String ERROR = "Error";
	public static final String FREE_RENTAL_DATES = "freeRentalDates";
	public static final String TOTAL_INCLUDING_DAMAGE_WAIVER = "totalIncludingDamageWaiver";
	public static final String TOTAL_WITH_DAMAGE_WAIVER = "totalwithDamageWaiver";
	public static final String CURRENCY_ISOCODE = "currencyIsoCode";
	public static final String FREE_DELIVERY_MODES = "freeDeliveryModes";
	public static final String GIFT_CARD_CODE = "giftCardCode";

	public static final String PHONE_REGEX_PATTERN = "^[+]*[(]{0,1}[0-9]{1,4}[)]{0,1}[-\\s\\./0-9]*$";
	public static final String IS_DISCONTINUED = "isDiscontinued";

	public static final String COMPLETED = "Completed";
	public static final String CONFIRM_OUTPUT = "confirmOutput";
	
	public static final String ZERO_RATING = "0";
	public static final String REVIEW_PAGE_DATE_FORMAT = "EEEE, MMM d";
	
	public static final int STATUS_LIST_SIZE_ONE = 1;
	public static final int STATUS_LIST_SIZE_TWO = 2;
	public static final int STATUS_LIST_SIZE_THREE = 3;

	public static final String EXTEND_ORDER = "extendOrder";

	public static final String DAMAGE_WAIVER_ERROR =  "Total Damage Waiver Cost : {}";

	public static final String DRIVING_LICENSE = "drivingLicense";
	public static final String UTILITY_BILL = "utilityBill";
	public static final String INSURANCE_CERTIFICATE = "insuranceCertificate";
	public static final String EXTRA_DOCUMENT1 = "extraDocument1";
	public static final String EXTRA_DOCUMENT2 = "extraDocument2";

	private BlCoreConstants()
	{
		//empty
	}
}
