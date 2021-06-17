package com.bl.storefront.controllers.pages;

public class BlControllerConstants
{

	public static final String FORM_GLOBAL_ERROR = "form.global.error";
	public static final String VALIDATE_CHECKPWD_EQUALS = "validation.checkPwd.equals";
	public static final String REGISTER_CHECKPWD_INVALID = "register.checkPwd.invalid";
	public static final String REGISTER_PWD_INVALID = "register.pwd.invalid";
	public static final String REGISTER_EMAIL_INVALID = "register.email.invalid";
	public static final int PASSWORD_MIN_LENGTH = 6;
	public static final int PASSWORD_MAX_LENGTH = 255;
	public static final int EMAIL_MAX_LENGTH = 255;
	public static final String DUBLICATE_UID_ERROR = "registration.error.account.exists.title";
	public static final String REGISTRATION_CONFIRMATION_MESSAGE = "registration.confirmation.message.title";
	public static final String EMAIL = "email";
	public static final String RENTAL_PAGE_IDENTIFIER = "rent";
	public static final String USED_PAGE_IDENTIFIER = "buy";
	public static final String FORWARD_SLASH = "/";
	public static final String PRODUCT_CODE_PATH_VARIABLE_PATTERN = FORWARD_SLASH +"{productCode:.*}";
	public static final String IS_RENTAL_PAGE = "IsRentalPage";
	public static final String HOME_CSS = "home ";
	public static final String HOMEPAGE = "homepage";
	public static final String COMMA = ",";
	public static final String DATE_FORMAT = "MMM d, yyyy";
	public static final String SEPARATOR = "|";
	public static final int PAIR_OF_DATES = 2;
	public static final String NUMBER_OF_DAYS = "numberOfDays";
	public static final String SELECTED_TO_DATE = "selectedToDate";
	public static final String SELECTED_FROM_DATE = "selectedFromDate";
	public static final int MIN_RENTAL_DAYS = 3;
	public static final int MAX_RENTAL_DAYS = 90;
	public static final String CARTPAGE_CSS = "cart cart-rental ";
	public static final String CARTPAGE = "cartpage";
	public static final String CART_CMS_PAGE_LABEL = "cart";
	public static final String EMPTY_CART_CMS_PAGE_LABEL = "emptyCart";
	public static final String CART_DATA = "cartData";
	public static final String PICKUP_CART_ENTRIES = "hasPickUpCartEntries";
	public static final String RENTAL_DATE = "rentalDate";
	public static final String RECOMMENDATION_DATE_FORMAT = "MMM d";
	public static final String DEFAULT_DAYS = "7";


	public static final String DAY_MON_DATE_YEAR_FORMAT = "EE MMM dd yyyy";
	public static final String DATE_FORMAT_PATTERN = "dd-MM-yyyy";
	public static final String SUCCESS = "success";
	public static final String SELECTED_DATE_MAP = "selectedDateMap";
	public static final String SELECTED_DATE = "selectedDate";
	
	public static final int PAGE_SIZE = 100;
	public static final String REGISTER_EMAIL_EMPTY = "register.email.empty";
	public static final String PASSWORD_STRING = "pwd";
  public static final String CONFIRM_PASSWORD_STRING = "checkPwd";
  public static final String REFERER= "Referer";
  public static final String HOME_PAGE_URL = "/";
	public static final String RENTAL_DATE_FAILURE_RESULT = "rentalDateNotSelected";
	public static final String STOCK_FAILURE_RESULT = "stockNotAvailable";
	public static final String ERROR = "error";

	public static final String REQUEST = "request";
	public static final String RESPONSE = "response";


	public static final String CATEGORY_CODE_PATH_PATTERN = FORWARD_SLASH +"{categoryCode:.*}";
	public static final String CATEGORY_CODE_PATH_VARIABLE_PATTERN = FORWARD_SLASH +"{parentcategory:.*}" + CATEGORY_CODE_PATH_PATTERN;

	public static final String LOGIN_EMAIL_OR_PASSWORD_INCORRECT = "login.error.account.not.found.title";

	public static final String ERROR_MESSAGE = "Error:";
	public static final String RATIO = ":";

	public static final String LOG_IN = "login";
	public static final String LOG_IN_ERROR = "loginError";
	public static final String LOG_IN_URL = "/login";
	
	public static final String RENTAL_DATE_FORMAT = "MMM dd";
	public static final String NEXT_AVAILABLE_DATE = "nextAvailabilityDate";

	public static final int SKIP_TWO_DAYS = 2;
	public static final String TRUE_STRING = "true";
	public static final String FALSE_STRING = "false";
	public static final String IS_WATCHING = "isWatching";
	public static final String STOCK_NOTIFICATION_FORM = "stockNotificationForm";
	public static final String USER_EMAIL_STRING = "userEmail";
	public static final String GIFT_CARD_REMOVE = "giftCardCodeRemove";
	public static final String GIFT_CARD_FORM = "giftCardForm";
	public static final String IS_GIFT_CARD_REMOVE = "isGiftCardRemoved";

	public static final String VOUCHER_FORM = "voucherForm";
	public static final String DELIVERY_METHOD_CHECKOUT_URL = "/checkout/multi/delivery-method/chooseShipping";
	public static final String PAYMENT_METHOD_CHECKOUT_URL = "/checkout/multi/payment-method/add";

	public static final String PRODUCT_REFERENCE = "productReferences";
	public static final String MAXIMUM_LIMIT = "productsLimit";
	public static final String ADDRESS_PAGE_IDENTIFIER = "Addresses";

	private BlControllerConstants()
	{
		//empty
	}

}
