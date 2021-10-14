package com.bl.storefront.controllers.pages;

public class BlControllerConstants
{

	public static final String FORM_GLOBAL_ERROR = "form.global.error";
	public static final String VALIDATE_CHECKPASSWORD_EQUALS = "validation.checkPwd.equals";
	public static final String REGISTER_CHECKPASSWORD_INVALID = "register.checkPwd.invalid";
	public static final String REGISTER_PASSWORD_INVALID = "register.pwd.invalid";
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
	public static final String SELECTED_DURATION = "selectedDuration";

	public static final int PAGE_SIZE = 100;
	public static final String REGISTER_EMAIL_EMPTY = "register.email.empty";
	public static final String PASSWORD_STRING = "pwd";
  public static final String CONFIRM_PASSWORD_STRING = "checkPwd";
  public static final String REFERER= "Referer";
  public static final String HOME_PAGE_URL = "/";
	public static final String RENTAL_DATE_FAILURE_RESULT = "rentalDateNotSelected";
	public static final String STOCK_FAILURE_RESULT = "stockNotAvailable";
	public static final String BLACKOUT_DATE_FOUND = "blackoutDateFound";
	public static final String ERROR = "error";

	public static final String REQUEST = "request";
	public static final String RESPONSE = "response";


	public static final String CATEGORY_CODE_PATH_PATTERN = FORWARD_SLASH +"{categoryCode:.*}";
	public static final String CATEGORY_CODE_PATH_VARIABLE_PATTERN = FORWARD_SLASH +"{parentcategory:.*}" + CATEGORY_CODE_PATH_PATTERN;

	public static final String LOGIN_EMAIL_OR_PASSWORD_INCORRECT = "login.error.account.not.found.title";
	public static final String ACCOUNT_DEACTIVATED = "login.error.account.deactivate.title";
	public static final String ACCOUNT_DEACTIVATED_MSG ="Account deactivated for user {}";
	public static final String LOGIN_EMAIL_OR_PASSWORD_INCORRECT_MSG =  "Login email or password is invalid for user {}";

	public static final String ERROR_MESSAGE = "Error:";
	public static final String RATIO = ":";

	public static final String LOG_IN = "login";
	public static final String LOG_IN_ERROR = "loginError";
	public static final String LOG_IN_URL = "/login";
	
	public static final String RENTAL_DATE_FORMAT = "MMM dd";
	public static final String NEXT_AVAILABLE_DATE = "nextAvailabilityDate";
	public static final String DISABLE_BUTTON = "disableButton";

	public static final int SKIP_TWO_DAYS = 2;
	public static final String TRUE_STRING = "true";
	public static final String FALSE_STRING = "false";
	public static final String IS_WATCHING = "isWatching";
	public static final String STOCK_NOTIFICATION_FORM = "stockNotificationForm";
	public static final String USER_EMAIL_STRING = "userEmail";
	public static final String GIFT_CARD_REMOVE = "giftCardCodeRemove";
	public static final String GIFT_CARD_FORM = "giftCardForm";
	public static final String IS_GIFT_CARD_REMOVE = "isGiftCardRemoved";

	public static final String AUDIO_CATEGORY = "audio";
	public static final String LIGHTING_CATEGORY = "lighting";
	public static final String PRODUCTION_CATEGORY = "production";

	public static final String VOUCHER_FORM = "voucherForm";
	public static final String DELIVERY_METHOD_CHECKOUT_URL = "/checkout/multi/delivery-method/chooseShipping";
	public static final String PAYMENT_METHOD_CHECKOUT_URL = "/checkout/multi/payment-method/add";

	public static final String PRODUCT_REFERENCE = "productReferences";
	public static final String MAXIMUM_LIMIT = "productsLimit";
	public static final String ADDRESS_PAGE_IDENTIFIER = "Addresses";
	public static final String USER_SELECTED_PAYMENT_INFO = "userSelectedPaymentInfo";
	public static final String SELECTED_PAYMENT_METHOD_NONCE = "selectedPaymentMethodNonce";
	public static final String PAYMENT_INFO_BILLING_ADDRESS = "paymentInfoBillingAddress";
	public static final String IS_SAVED_CARD_ORDER = "isSavedCardOrder";
	public static final String CHECKOUT_SUMMARY_PAGE = "multiStepCheckoutSummaryPage";
	public static final String UPDATE_PASSWORD_PAGE_IDENTIFIER = "Change Password";
	public static final String ORDER_CONFIRMATION_PAGE = "orderConfirmation";
	public static final String CREDIT_CARD_CHECKOUT = "CreditCard";
	public static final String BOOKMARKS_PAGE_IDENTIFIER = "Bookmarks";
	public static final String UPDATE_EMAIL_IDENTIFIER = "Change Email";
	public static final String REVIEW_PAGE_DATE_FORMAT = "EEEE, MMM d";
	public static final String FORMATTED_RENTAL_START_DATE = "formattedRentalStartDate";
	public static final String FORMATTED_RENTAL_END_DATE = "formattedRentalEndDate";
	
	public static final String USER_SELECTED_PAYPAL_PAYMENT_INFO = "userSelectedPayPalPaymentInfo";
	public static final String PAYPAL_CHECKOUT = "BrainTreePayPalExpress";
	public static final String REVIEW_PAGE = "Review";
	public static final String DEFAULT_BILLING_ADDRESS = "defaultBillingAddress";

	public static final String REDIRECT_PREFIX = "redirect:";

	public static final String REDIRECT_CART_URL = REDIRECT_PREFIX + "/cart";
	public static final String SAVE_CART_FORM = "saveCartForm";
	
	public static final String PAYPAL_ERROR_MESSAGE_KEY = "text.paypal.payment.error.message";
	public static final String DISABLE_PAYMENT = "disablePayment";
	public static final String BILL_TO_COUNTRY = "billTo_country";
	public static final String DISCONTINUE_MESSAGE_KEY = "cart.discontinue.product.removed.entry.text";

	public static final String BRAINTREE_GENERAL_ERROR_KEY = "braintree.billing.general.error";
	public static final String BRAINTREE_CVV_ERROR_KEY = "braintree.cvv.general.error";
	public static final String CART_PAGE = "cartPage";
	public static final String FROM_PAGE = "fromPage";
	public static final String USER_SELECTED_PO_NUMBER = "selectedPoNumber";
	public static final String USER_SELECTED_PO_NOTES = "selectedPoNotes";


	public static final String ORDER_DATA = "orderData";
	public static final String PAGE_TYPE = "pageType";
	public static final String ORDER_DETAILS = "orderDetails";
	public static final String IS_USED_GEAR_CART_ACTIVE = "isUsedGearCartActive";
	public static final String MY_ACCOUNT_ORDER = "/my-account/order/";
	public static final String PAYMENT_ID = "paymentId";
	public static final String PAYMENT_NONCE= "paymentNonce";
	public static final String EXTEND_ORDER_DATA = "extendOrderData";
	public static final String MY_ACCOUNT_EXTEND_RENTAL = "/my-account/extendRent/";
  public static final String CLIENT_TOKEN = "client_token";
	public static final String SHIPPING_PAGE ="shippingPage";
	public static final String BILLING_PAGE ="paymentPage";
  public static final String EXTEND_ORDER = "extendOrderError";
  public static final String COUPON_INVALID = "coupon.invalid.code.provided";
   public static final String MULTIPLEGIFTCARD = "Only one gift card can be ordered at a time";
   public static final String GIFTCARDNOTALLOWE = "Gift card can not be ordered with active rental or used gear products";
   public static final String ADDTOCARTWARNING = "Rental and Used gear products are not allowed together";

	public static final String PO_NUMBER = "extendPoNumber";
	public static final String PO_NOTES = "extendPoNotes";

	public static final String PAYMENT_METHOD = "extendOrderPaymentMethod";
	public static final String PO = "poNumber";
	public static final String CREDIT_CARD = "creditCard";
	public static final String PAY_PAL = "payPal";
	public static final String SUCCESS_MSG_TYPE = "successMsgEmail";
	public static final String ERROR_MSG_TYPE = "errorMsg";

	public static final String MY_ACCOUNT_SAVED_CARTS_URL = "/my-account/saved-carts";
	public static final String REDIRECT_TO_SAVED_CARTS_PAGE = REDIRECT_PREFIX + MY_ACCOUNT_SAVED_CARTS_URL;
	public static final String SAVED_CART_SUCCESS = "saved_cart_success";
	public static final String RENAMED_CART_CODE = "renamed_cart_code";
	public static final String SAVED_CART_MESSAGE = "text.saved.cart.success";

	public static final String RETURN_REQUEST = "returnRequest";

	public static final String PASSWORDMISMATCH_MSG_TYPE = "passwordError";
	public static final String CURRENTPASSWORD_MSG_TYPE = "currentPasswordError";

	public static final String EXTEND = "extendOrder";
	public static final String EMPTY = "";
	public static final String PAY_BILL = "payBill";
	public static final String REDIRECT_TO_HOME_URL ="redirect:/";
	public static final String IS_NEW_GEAR_CATEGORY ="isNewGearCategory";
  public static final String USED_CATEGORY_CODE = "usedgear";
  public static final String USED_CATEGORY_PREFIX_URL = "/buy/category/";
  public static final String ENABLE_SATURDAYS = "enableSaturdays";
  public static final String MODIFYPAYMENT = "modifyPayment";

  public static final String IS_AVALARA_EXCEPTION = "isAvalaraException";

	public static final String VERIFICATION_PAGE_IDENTIFIER = "verificationImages";

	public static final String CLEAR_BRANDS = "clearBrands";
	public static final String BRANDS = "brands";
	public static final String ENABLE_DATE_PICKER = "enableDatePicker";
	public static final String HAS_USER_RESTRICTION = "hasUserRestriction";
	public static final String BL_GROUP = "BLGroup";


	private BlControllerConstants()
	{
		//empty
	}

}
