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
	public static final int PRECISION = 2;

	public static final String CODE = "code";
	public static final String BRANDS = "Brands";

	public static final String COUPON_APPLIED_MSG = "coupon_applied_msg";
	public static final String RECEIVED = "Received";
	public static final String PO = "PO";
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
	public static final String IS_GIFT_CARD = "giftCard";
	public static final String GIFTCARD = "GIFTCARD";
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
	public static final String ON_SALE = "ON SALE";
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
	public static final String BUFFER_INVENTORY = "bufferInventory";
	public static final String BASE_STORE_ID = "bl";
	public static final int END_HOURS = 23;
	public static final int END_MINUTES = 59;
	public static final int END_SECONDS = 59;
	public static final int END_MILLI_SECONDS = 999;
	public static final int START_HOURS = 0;
	public static final int START_MINUTES = 0;
	public static final int START_SECONDS = 0;
	public static final int START_MILLI_SECONDS = 0;
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
	public static final String COOKIE_NAME_FOR_DURATION = "selectedDuration";
	public static final String SELECTED_DURATION = "selectedDuration";
	public static final int PAIR_OF_DATES = 2;
	public static final String SEPARATOR = "\\|";
	public static final String SELECTED_DATE_MAP = "selectedDateMap";
	public static final String SELECTED_DURATION_MAP = "selectedDurationMap";
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
	public static final String PACKAGE= "packages";
	public static final String GEAR_PACKAGE="gear packages";

	public static final int SKIP_THREE_DAYS = 3;
	public static final int SKIP_TWO_DAYS = 2;
	public static final int SKIP_ONE_DAYS = 1;

	public static final String GIFT_CARD = "giftcard-";
	public static final String GIFT_CARD_EMAIL_PROCESS = "giftCardEmailProcess";
	public static final String UNDERSCORE = "_";
	public static final String DOUBLE_HYPHEN = "--";

	public static final String RUSH_SAN_CARLOS = "SAME_DAY_DELIVERY";
	public static final String RUSH_NYC_NEXT_DAY = "NEXT_DAY_RUSH_DELIVERY";

	public static final String IS_AUTHORISED = "isAuthorized";
	public static final String COUPON_ID = "couponId";

	//Promotion Condition Translator
	public static final String RENTAL_ARRIVAL_DATE = "rentalArrivalDate";
	public static final String OPERATOR = "operator";
	public static final String IS_RENTAL_CART = "rentalCart";
	public static final String RENTAL_VALUE = "value";
	public static final String UG_PERCENTAGE_DISCOUNT_VALUE = "value";
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

	public static final String SHIP_DATE = "shipDate";
	public static final String STATUS = "status";

	public static final String WAREHOUSE = "warehouse";
	public static final String PULL_JOB_ERROR_OCCURRED = "Error occurred while performing PullReadyToShipOrdersJob";
	public static final String EMPLOYEE = "Employee ";
	public static final int DEFAULT_MEMBERS_COUNT = 1;
	public static final String DEFAULT_PRODUCT_QUANTITY = "1";


	public static final String PHONE_REGEX_PATTERN = "^[+]*[(]{0,1}[0-9]{1,4}[)]{0,1}[-\\s\\./0-9]*$";

	public static final String IS_USED_GEAR_PODUCT = "usedGearProduct";

	public static final String ITEMS_ON_SALE = "Items On Sale";

	public static final String IS_DISCONTINUED = "isDiscontinued";

	public static final String COMPLETED = "Completed";
	public static final String CONFIRM_OUTPUT = "confirmOutput";

	public static final String ZERO_RATING = "0";
	public static final String REVIEW_PAGE_DATE_FORMAT = "EEEE, MMM d";
	public static final String BL_PRODUCTCATALOG = "blProductCatalog";

	public static final int STATUS_LIST_SIZE_ONE = 1;
	public static final int STATUS_LIST_SIZE_TWO = 2;
	public static final int STATUS_LIST_SIZE_THREE = 3;

	public static final String EXTEND_ORDER = "extendOrder";

	public static final String DAMAGE_WAIVER_ERROR =  "Total Damage Waiver Cost : {}";
	public static final String NEW_LINE_CHARACTER =  "\n";
	public static final String CATALOG_VERSION_NAME = "Online";

	public static final String SUBSCRIPTION_API_OPERATION_STATUS = "OK";
	public static final String SUBSCRIPTION_CONTACT_KEY = "contactKey";
	public static final String EMAIL_ADDRESS = "Email Address";
	public static final String EMAIL_ADDRESSES = "Email Addresses";
	public static final String HTML_ENABLED = "HTML Enabled";

	public static final String ALPHANUMERIC_VALUES = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
	public static final String BL_SAN_CARLOS = "BL_SAN_CARLOS";
	public static final String BL_WALTHAM = "BL_WALTHAM";

	public static final String IN_HOUSE_REPAIR = "IN_HOUSE_REPAIR";
	public static final String VENDOR_REPAIR = "VENDOR_REPAIR";
	public static final String CUSTOMER_RESPONSIBLE_REPAIR = "CUSTOMER_RESPONSIBLE_REPAIR";
	public static final String PARTS_NEEDED_REPAIR = "PARTS_NEEDED_REPAIR";
	public static final String CREATING_REPAIR_LOG_MESSAGE = "Creating Repair Log for Type : {}";
	public static final String ACCEPTED = "ACCEPTED";

	public static final String HARD_ASSIGNED = "hardAssign";
	public static final String SOFT_ASSIGNED = "softAssign";
	public static final String NEW_GEAR = "newgear";
	public static final String RETAILGEAR = "retailGear" ;
	public static final String BUNDLEPRODUCT = "bundleProduct" ;
	public static final String BARCODE = "barcode";
	public static final String IS_NEW_GEAR_INSTOCK = "newGearInStock";

	public static final String RETURN_REQUEST = "returnRequest";
	public static final String ASAGENT = "asagent";
	public static final String REQUEST = "request";
	public static final String ACTING_USER_UID = "ACTING_USER_UID";
	public static final String ASM_SESSION_PARAMETER = "ASM";
	public static final String MISSING_CHARGE = "MISSING_CHARGE";
	public static final String AQUATECH_BRAND_ID ="9";
	public static final String SQL_DATE_FORMAT = "MM-dd-yyyy";


	public static final String DRIVING_LICENSE = "drivingLicense";
	public static final String UTILITY_BILL = "utilityBill";
	public static final String INSURANCE_CERTIFICATE = "insuranceCertificate";
	public static final String EXTRA_DOCUMENT1 = "extraDocument1";
	public static final String EXTRA_DOCUMENT2 = "extraDocument2";

	public static final String TOTAL_PRICE = "Total Price : {}";

	public static final String COMMON_ERROR_MESSAGE = "unexpected.update.error";
	public static final String BL_SERIAL_PRODUCT_VALIDATE_INTERCEPTOR = "BlSerialProductValidateInterceptor";

	public static final String ZERO = "0";

	public static final String BY_DATE = "byDate";
	public static final String BY_ORDER_NUMBER = "byOrderNumber";
	public static final String CUSTOMER = "customer";
	public static final String STORE = "store";
	public static final String FILTER_STATUS_LIST = "filterStatusList";
	public static final String STATUS_LIST = "statusList";

	public static final int TWO_DAYS = 2;
	public static final int ONE_DAY = 1;

	public static final String STATUS1 = "status1";
	public static final String STATUS2 = "status2";

	public static final String BL_PROMO_GROUP = "blPromoGrp";
	public static final String BL_EXTENDED_RENTAL_DAYS_PROMOCODE = "EXTENDEDRENTALDAYS";
	public static final String QUALIFYING_COUPONS = "y_qualifying_coupons";
	public static final String CONTACTUS_NAV_LINK = "ContactUsNavLink";

	public static final String BL_BLACKOUT_DATE_MODEL = "BlBlackoutDateModel";
	public static final String FROM_DATE = "fromDate";
	public static final String SERIAL = "serial";
	public static final String SERIAL_CODE = "serialCode";
	public static final String ORDER_CODE = "orderCode";
	public static final String RETURN_DATE = "returnDate";
	public static final String CONTACTUS_LINK = "contactus";

	public static final String QUERY_DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";
	public static final String HYPHEN = "-";

	public static final String RENTAL_ORDER = "isRentalCart";
	public static final String SHARE_A_SALE = "shareASaleSent";
	public static final String ORDER_STATUS = "code";
	public static final String START_SHARE_A_SALE_JOB = "Start performing BlShareASaleJob...";
	public static final String SHARE_A_SALE_URL = "https://shareasale.com";
	public static final String SHARE_A_SALE_MERCHANT_ID = "/w.cfm?merchantId=";
	public static final String SHARE_A_SALE_TOKEN = "&token=";
	public static final String SHARE_A_SALE_VERSION = "&version=";
	public static final String SHARE_A_SALE_ACTION = "&action=";
	public static final String SHARE_A_SALE_DATE = "&date=";
	public static final String SHARE_A_SALE_ORDER_NUMBER = "&ordernumber=";
	public static final String SHARE_A_SALE_TRANS_TYPE = "&transtype=";
	public static final String SHARE_A_SALE_AMOUNT = "&amount=";
	public static final String SHARE_A_SALE_SUBTOTAL_FORMAT = "%.2f";
	public static final String SHARE_A_SALE_TRACKING = "&tracking=";
	public static final String SHARE_A_SALE_TRACKING_S = "-S";
	public static final String SHARE_A_SALE_STORE_ID = "&storeID=";
	public static final String SHARE_A_SALE_PER_SALE = "&persale=";
	public static final String SHARE_A_SALE_NEW_CUSTOMER = "&newcustomer=";
	public static final String ONE = "1";
	public static final String SHARE_A_SALE_URL_LINK = "&urllink=";
	public static final String DEFAULT_ENCODING = "UTF-8";
	public static final String SHARE_A_SALE_COUPON_CODE = "&couponcode=";
	public static final String SHARE_A_SALE_DATE_FORMAT = "EEE, dd MMM yyyy HH:mm:ss z";
	public static final String SHARE_A_SALE_GMT = "GMT";
	public static final String SHARE_A_SALE_NEW_AC_URL = "shareASale new account url: {}";
	public static final String SHARE_A_SALE_AUTH_DATE = "x-ShareASale-Date";
	public static final String SHARE_A_SALE_AUTH = "x-ShareASale-Authentication";
	public static final String SHARE_A_SALE_JOB_FINISH = "Finished performing BlShareASaleJob...";
	public static final String SHARE_A_SALE_JOB_ERROR_MSG = "Error occurred while performing BlShareASaleJob";
	public static final String SHARE_A_SALE_JOB_HTTP_STATUS_MSG = "Unexpected http status: ";
	public static final String SHARE_A_SALE_JOB_HTTP_CONNECTION_FAILURE = "ShareASale http connection failure: {}";
	public static final String SHARE_A_SALE_ORDER_CREATION_DATE_FORMAT = "MM/dd/yyyy";
	public static final String SHARE_A_SALE_NEW_URL_SENDING_MSG = "Sending shareASale new account message: {}";
	public static final String SHARE_A_SALE_ORDERS_NOT_EXIST = "No completed status rental orders exist";
	public static final String SHARE_A_SALE_COMMA = ",";
	public static final String BIN_LOCATION_ID = "binLocationId";

	public static final String ORDER_CONFIRMATION_EVENT_DEFINITION_KEY = "order.confirmation.event.definition.key";
	public static final String ORDER_CONFIRMATION_EVENT_TEMPLATE = "order.confirmation.event.template";
	public static final String ORDER_PAYMENT_DECLINED_EVENT_TEMPLATE = "order.paymentdeclined.event.template";
	public static final String ORDER_VERIFICATION_MORE_INFO_EVENT_DEFINITION_KEY = "order.verification.moreinfo.event.definition.key";
	public static final String ORDER_VERIFICATION_MORE_INFO_EVENT_TEMPLATE = "order.verification.moreinfo.event.template";

	public static final String ORDER_VERIFICATION_COI_NEEDED_EVENT_DEFINITION_KEY = "order.verification.coineeded.event.definition.key";
	public static final String ORDER_VERIFICATION_COI_NEEDED_EVENT_TEMPLATE = "order.verification.coineeded.event.template";

	public static final String ORDER_VERIFICATION_REQUIRED_EVENT_DEFINITION_KEY = "order.verification.required.event.definition.key";
	public static final String ORDER_VERIFICATION_REQUIRED_EVENT_TEMPLATE = "order.verification.required.event.template";
	public static final String ORDER_VERIFICATION_COMPLETED_EVENT_DEFINITION_KEY = "order.verification.completed.event.definition.key";
	public static final String ORDER_VERIFICATION_COMPLETED_EVENT_TEMPLATE = "order.verification.completed.event.template";

	public static final String ORDER_CANCELED_EVENT_TEMPLATE = "order.Canceled.event.template";
	public static final String ORDER_READYFORPICKUP_EVENT_TEMPLATE = "order.readyforpickup.event.template";
	public static final String ORDER_CANCELED_EVENT_DEFINITION_KEY="order.canceled.event.definition.key";
	public static final String ORDER_PAYMENT_DECLINED_EVENT_DEFINITION_KEY="order.paymentdeclined.event.definition.key";
	public static final String ORDER_READYFORPICKUP_EVENT_DEFINITION_KEY="order.readyforpickup.event.definition.key";
	public static final String ORDER_NEWSHIPPING_EVENT_TEMPLATE = "order.newshipping.event.template";
	public static final String ORDER_RNEWSHIPPING_EVENT_DEFINITION_KEY="order.newshipping.event.definition.key";

	public static final String ORDER_EXCEPTION_EVENT_DEFINITION_KEY = "order.exception.event.definition.key";
	public static final String ORDER_EXCEPTION_EVENT_TEMPLATE = "order.exception.event.template";
	public static final String ORDER_UNBOXED_EVENT_DEFINITION_KEY = "order.unboxed.event.definition.key";
	public static final String ORDER_UNBOXED_EVENT_TEMPLATE = "order.unboxed.event.template";

	public static final String ORDER_DEPOSIT_EVENT_DEFINITION_KEY = "order.deposit.event.definition.key";
	public static final String ORDER_DEPOSIT_EVENT_TEMPLATE = "order.deposit.event.template";

	public static final String ORDER_EXTENSION_EVENT_DEFINITION_KEY = "order.extension.event.definition.key";
	public static final String ORDER_EXTENSION_EVENT_TEMPLATE = "order.extension.event.template";

	public static final String ORDER_SHIPPED_EVENT_DEFINITION_KEY = "order.shipped.event.definition.key";
	public static final String ORDER_SHIPPED_EVENT_TEMPLATE = "order.shipped.event.template";
	public static final String ORDER_PICKEDUP_EVENT_DEFINITION_KEY = "order.pickedup.event.definition.key";
	public static final String ORDER_PICKEDUP_EVENT_TEMPLATE = "order.pickedup.event.template";


	public static final String ORDER_EXTRA_ITEM_EVENT_DEFINITION_KEY = "order.extra.item.event.definition.key";
	public static final String ORDER_EXTRA_ITEM_EVENT_TEMPLATE = "order.extra.item.event.template";


	public static final String ORDER_PULL_BACK_ADDED_ITEMS_EVENT_DEFINITION_KEY = "order.pull.back.item.added.event.definition.key";

	public static final String ORDER_PULL_BACK_REMOVED_ITEMS_EVENT_DEFINITION_KEY = "order.pull.back.item.removed.event.definition.key";
	public static final String ORDER_PULL_BACK_REMOVED_ITEMS_EVENT_TEMPLATE = "pullBackOrderRemoveItems";

	public static final String BORROW_LENSES_SUBSCRIBER_ID = "borrow.lenses.subscriber.id";

	public static final String ORDER_DEPOSIT_REQUIRED_EVENT_DEFINITION_KEY = "order.deposit.required.event.definition.key";
	public static final String ORDER_DEPOSIT_REQUIRED_EVENT_TEMPLATE = "order.deposit.required.event.template";

	public static final String ORDER_GIFT_CARD_EVENT_DEFINITION_KEY = "order.giftcard.purchase.event.definition.key";
	public static final String ORDER_GIFT_CARD_EVENT_TEMPLATE = "order.giftcard.purchase.event.template";

	public static final String SHIPPING_ROOT_ELEMENT = "shippinginfo";
	public static final String SHIPPING_FIRST_NAME = "shippingfirstname";
	public static final String SHIPPING_LAST_NAME = "shippinglastname";
	public static final String SHIPPING_ORGANIZATION = "shippingorganization";
	public static final String SHIPPING_ADDRESS_1 = "shippingaddress1";
	public static final String SHIPPING_ADDRESS_2 = "shippingaddress2";
	public static final String SHIPPING_CITY = "shippingcity";
	public static final String SHIPPING_STATE = "shippingstate";
	public static final String SHIPPING_ZIP_CODE = "shippingzipcode";
	public static final String SHIPPING_PHONE = "shippingphone";
	public static final String SHIPPING_EMAIL = "shippingemail";
	public static final String SHIPPING_HOURS = "shippinghours";
	public static final String SHIPPING_NOTES = "shippingnotes";

	public static final String BILLING_ROOT_ELEMENT = "billinginfo";
	public static final String BILLING_FIRST_NAME = "billingfirstname";
	public static final String BILLING_LAST_NAME = "billinglastname";
	public static final String BILLING_ORGANIZATION = "billingorganization";
	public static final String BILLING_ADDRESS_1 = "billingaddress1";
	public static final String BILLING_ADDRESS_2 = "billingaddress2";
	public static final String BILLING_CITY = "billingcity";
	public static final String BILLING_STATE = "billingstate";
	public static final String BILLING_ZIP_CODE = "billingzipcode";
	public static final String BILLING_PHONE = "billingphone";
	public static final String BILLING_EMAIL = "billingemail";
	public static final String BILLING_NOTES = "ordernotes";
	public static final String BILLING_GIFT_CARD_USED = "giftcardused";
	public static final String BILLING_GIFT_CARD_BALANCE = "giftcardbalance";

	public static final String ORDER_ITEMS_ROOT_ELEMENT = "orderitemsinfo";
	public static final String ORDER_ITEM_ROOT_ELEMENT = "orderitem";
	public static final String ORDER_ITEM_PRODUCT_CODE = "productcode";
	public static final String ORDER_ITEM_PRODUCT_TITLE = "producttitle";
	public static final String ORDER_ITEM_PRODUCT_PHOTO = "productphoto";
	public static final String ORDER_ITEM_RENTAL_PRICE = "rentalprice";
	public static final String ORDER_ITEM_DAMAGE_WAIVER_PRICE = "damagewaiverprice";
	public static final String ORDER_ITEM_DAMAGE_WAIVER_TEXT = "damagewaivertext";
	public static final String ORDER_ITEM_TOTAL_PRICE = "totalprice";
	public static final String ORDER_ITEM_QUANTITY = "quantity";

	public static final String ITEMS_ROOT_ELEMENT = "itemsinfo";
	public static final String ITEM_ROOT_ELEMENT = "item";
	public static final String ITEM_PRODUCT_URL = "producturl";
	public static final String ITEM_AMOUNT_DUE_ROOT_ELEMENT = "amountdue";
	public static final String ITEM_NOTES_ROOT_ELEMENT = "notes";

	public static final String DATE_PATTERN = "yyyy-MM-dd";
	public static final String CUSTOMER_CHECKOUT_ORDER_NOTES = "CUSTOMER_CHECKOUT_ORDER_NOTES";
	public static final String RENTAL = "Rental";
	public static final String UN_BOXED = "UNBOXED_COMPLETELY";

	public static final String NAME = "Name";

	public static final String GIFT_CARD_ORDER = "Gift Card";
	public static final String NEW_GEAR_ORDER = "New Gear";

	public static final String TECH_ENG_USER_GROUP  = "techEngusergroup";
	public static final String REPAIR_USER_GROUP  = "repairmemberusergroup";

	public static final String PAY_PAL_PROVIDER  = "BrainTreePayPalExpress";
	public static final String PAY_PAL = "Paypal";
  public static final String UID = "uid";
	public static final String BL_GROUP ="BLGroup";
  public static final String USER_RESTRICTION = "hasUserRestriction";

	public static final String LATE_CHARGE = "LATE_CHARGE";
	public static final String REPAIR_CHARGE = "REPAIR_CHARGE";

	public static final String ORDER_REFUND_EVENT_DEFINITION_KEY = "order.refund.event.definition.key";
	public static final String ORDER_REFUND_EVENT_TEMPLATE = "order.refund.event.template";
	public static final String ORDER_ITEM_PRODUCT_CANCEL_REASON = "reason";


	public static final String BILLING_CHARGES = "billingCharges";
	public static final String UNBOXED = "UNBOXED";

	public static final String ORDER_BILL_PAID_EVENT_DEFINITION_KEY = "order.bill.paid.event.definition.key";
	public static final String ORDER_BILL_PAID_EVENT_TEMPLATE = "order.bill.paid.event.template";

	public static final String RESTRICTED_PRINCIPALS_STRING_MV = "-restrictedPrincipals_string_mv";
	public static final String COLON = ":";

	public static final String VERIFICATION_LEVEL_ZERO = "0";
	public static final String VERIFICATION_LEVEL_ONE = "1";
	public static final String VERIFICATION_LEVEL_TWO = "2";

	public static final String BILLING_TYPE = "billtype";
	
	public static final String GIFT_CARD_TYPE = "GiftCard";
	public static final String GC_TYPE = "GC";
	public static final String PLUS = "+";
	public static final String BL_PRODUCT_VALIDATE_INTERCEPTOR = "BlProductValidateInterceptor";
	public static final String ORDER_MANUAL_ALLOCATION_EVENT_DEFINITION_KEY = "order.manual.allocation.event.definition.key";
	public static final String UNALLOCATED_QUANTITY = "unallocatedquantity";
	public static final String SERIAL_ALLOCATED = "serialallocated";
	public static final String SERIAL_ALLOCATED_YES = "Yes";
	public static final String SERIAL_ALLOCATED_NO = "No";

	public static final String FEDEX = "fedex";
	public static final String UPS = "ups";
	public static final String USPS = "usps";
	
	public static final String ANONYMOUS = "anonymous";
	public static final String FRONT_DESK_DELIVERY_MODE_KEY_PREFIX = "BL_";

	public static final String OPTIMIZEDSHIPPINGSTARTDATE = "optimizedShippingStartDate";
	public static final String PULLED_ORDERS_QUEUE = "PulledOrdersQueue";
	public static final String AWAITING_ORDERS_QUEUE = "OrdersAwaitingQueue";
	public static final String COMPLTED_ORDER_QUEUE = "ShippingCompleteOrderQueue";
	
	public static final String BL_ORDER_VALIDATE_INTERCEPTOR = "BlOrderValidateInterceptor";
	public static final String BL_REPAIR_LOG_PREPARE_INTERCEPTOR = "BlRepairLogPrepareInterceptor";

	public static final String ACTIVE_STATUS = "ACTIVE";
	public static final String PARTIALLY_UNBOXED = "PARTIALLY_UNBOXED";
	public static final String RECEIVED_OR_RETURNED = "RECEIVED_OR_RETURNED";
	public static final String BOXED = "BOXED";
	public static final String SHIPPED = "SHIPPED";
	public static final String IN_HOUSE = "IN_HOUSE";
	public static final String ADDED_TO_CART = "ADDED_TO_CART";
	
	public static final String SYNC_ACTIVE_PROPERTY = "catalog.sync.active";

	public static final int NAME_LENGTH_SIZE = 48;


	public static final String ORDER_MODIFIED_DATE = "orderModifiedDate";
	public static final String ORDER_MODIFIED_END_DATE = "orderModifiedEndDate";
	public static final String ORDER_BILL_MODIFIED_DATE = "orderBillModifiedDate";
	public static final String ORDER_BILL_MODIFIED_END_DATE = "orderBillModifiedEndDate";

	// Order Status
	public static final String RECEIVED_IN_VERIFICATION = "RECEIVED_IN_VERIFICATION";
	public static final String RECEIVED_MANUAL_REVIEW = "RECEIVED_MANUAL_REVIEW";
	public static final String RECEIVED_SHIPPING_MANUAL_REVIEW = "RECEIVED_SHIPPING_MANUAL_REVIEW";
	public static final String RECEIVED_PAYMENT_DECLINED = "RECEIVED_PAYMENT_DECLINED";
	public static final String UNBOXED_PARTIALLY = "UNBOXED_PARTIALLY";
	public static final String UNBOXED_COMPLETELY = "UNBOXED_COMPLETELY";
	public static final String RECEIVED_ROLLING = "RECEIVED_ROLLING";
	public static final String SOLD_SHIPPED = "SOLD_SHIPPED";
	public static final String SOLD_RMA_CREATED = "SOLD_RMA_CREATED";
	public static final String RETURNED = "RETURNED";
	public static final String ORDER_COMPLETED = "COMPLETED";
	public static final String INCOMPLETE = "INCOMPLETE";
	public static final String INCOMPLETE_BALANCE_DUE = "INCOMPLETE_BALANCE_DUE";
	public static final String INCOMPLETE_STOLEN = "INCOMPLETE_STOLEN";
	public static final String INCOMPLETE_LOST_IN_TRANSIT = "INCOMPLETE_LOST_IN_TRANSIT";
	public static final String INCOMPLETE_ITEMS_IN_REPAIR = "INCOMPLETE_ITEMS_IN_REPAIR";
	public static final String INCOMPLETE_MISSING_ITEMS = "INCOMPLETE_MISSING_ITEMS";
	public static final String INCOMPLETE_MISSING_AND_BROKEN_ITEMS = "INCOMPLETE_MISSING_AND_BROKEN_ITEMS";
	public static final String CANCELLED = "CANCELLED";
	public static final String LATE = "LATE";
	public static final String RECEIVED_STATUS = "RECEIVED";
	public static final String PENDING_STATUS = "PENDING";
	public static final String SOLD = "SOLD";
	public static final String TAX = "Tax";
	public static final String QUANTITY = "Quantity";
	public static final String FORMAT_STRING= "##0.00";

	// Consignment Status
	public static final String RECEIVED_READY_TO_SHIP = "RECEIVED_READY_TO_SHIP";
	public static final String RECEIVED_READY_FOR_PICKUP = "RECEIVED_READY_FOR_PICKUP";
	
	public static final int INT_ONE = 1;
   public static final int INT_TWO = 2;
   public static final String CARD_MASK = "******";
   public static final String MASKED_CARD_SEPARATOR = "\\*\\*\\*\\*\\*\\*";
   public static final String MASKED_CARD_FORMAT = "****%s";

	public static final String OUT_CONFIRM = "orderdepositrequired";
	public static final String COMPLETE = "completed";
	public static final String MESSAGE_BOX_TITLE = "success.message.depositRequired.title";
	public static final String MESSAGE_BOX_TEXT = "success.message.depositRequired.emailsent";
	public static final String MESSAGE_BOX_ERROR_TEXT = "error.message.depositRequired.emailsent";
	public static final String INPUT_OBJECT = "inputObject";
	public static final String DEPOSIT_REQUIRED_TITLE = "blbackoffice.depositrequired.confirm.title";
	public static final String DEPOSIT_REQUIRED_MISSING_AMOUNT = "blbackoffice.depositrequired.missing.amount";
	public static final String DEPOSIT_REQUIRED_NOT_AN_AMOUNT = "blbackoffice.depositrequired.amount.notanumber";
	public static final String UNDO_CHANGES = "undochanges";
	public static final String CONFIRM_TRIGGER = "confirmTriggerEmail";
	public static final String UPDATE_PASSWORD_URL = "/login/pw/change";
	public static final String CUSTOMER_MODEL = "customerModel";
	public static final String TOKEN = "token=";
	public static final String THUMBNAIL = "thumbnail";
	public static final String RENTAL_PDP_URL_PREFIX = "/rent/product/";




	private BlCoreConstants()
	{
		//empty
	}
}
