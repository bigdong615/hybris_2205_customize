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


	public static final String SFTPHOST = "ftp.s10.exacttarget.com";
	public static final int SFTPPORT = 22;
	public static final String SFTPUSER = "ftp.user.name";
	public static final String SFTPPASS = "ftp.user.password";
	public static final String SFTPWORKINGDIR = "/newbl/bl/bin/custom/testxml/";
	public static final String XML_INDENT = "{http://xml.apache.org/xslt}indent-amount";
	public static final String XML_INDENT_SIZE = "xml.indent.size";
	public static final String YES = "yes";
	public static final String FILE_FORMAT = "yyyyMMddHHmm";
	public static final String FILE_SUFFIX = ".xml";
	public static final String LOCAL_FTP_PATH = "local.ftp.file.path";
	public static final String STICT_HOST_KEY = "StrictHostKeyChecking";
	public static final String NO = "no";
	public static final String SFTP = "sftp";
	public static final String CLIENT_FTP_PATH = "client.ftp.file.path";

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

	public static final String ORDER_ITEMS_ROOT_ELEMENT = "orderitemsinfo";
	public static final String ORDER_ITEM_ROOT_ELEMENT = "orderitems";
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
	public static final String CUSTOMER_CHECKOUT_ORDER_NOTES = "CUSTOMER_CHECKOUT_ORDER_NOTES";
	public static final String OLD_ORDER_ID = "old order id";
	public static final String ORDER_ITEM_ID = "OrderItemId";

	public static final String PRODUCT_CATALOG = "catalog";
	public static final String VERSION = "version";
	public static final String STAGED = "Staged";
	public static final String CATALOG_VALUE = "blProductCatalog";
	public static final String COLON = ":";
	public static final String ONLINE = "Online";

	public static final String ORDER_ID = "OrderId";
	public static final String TEMPLATE="Template";
	public static final String SUBSCRIBER_ID="SubscriberId";
	public static final String EMAIL_ADDRESS="EmailAddress";
	public static final String TYPE="Type";
	public static final String REPLACEMENT="Replacement";
	public static final String STATUS = "Status";
	public static final String DATE_PLACED="DatePlaced";
	public static final String SHIPPING_METHOD_TYPE="ShippingMethodType";
	public static final String SHIPPING_METHOD="ShippingMethod";
	public static final String SHIPPING_METHOD_TEXT="ShippingMethodText";
	public static final String TRACKING_INFO="TrackingInfo";
	public static final String ITEM_COST="ItemCost";
	public static final String DAMAGE_WAIVER_COST="DamageWaiverCost";
	public static final String SUB_TOTAL="Subtotal";
	public static final String SHIPPING_AMOUNT="ShippingAmount";
	public static final String TAX_AMOUNT="TaxAmount";
	public static final String DISCOUNT_AMOUNT="DiscountAmount";
	public static final String TOTAL_COST="TotalCost";
	public static final String DISCOUNT_TEXT="DiscountText";
	public static final String EXPECTED_SHIPPING_DATE="ExpectedShippingDate";
	public static final String ARRIVAL_DATE="ArrivalDate";
	public static final String RETURN_DATE="ReturnDate";
	public static final String ACTUAL_RETURN_DATE="ActualReturnDate";
	public static final String RENTAL_DURATION="RentalDuration";
	public static final String CUSTOMER_NAME="CustomerName";
	public static final String VERIFICATION_LEVEL="VerificationLevel";
	public static final String COI_AMOUNT="COIAmount";
	public static final String COI_EXPIRATION_DATE="COIExpirationDate";
	public static final String TOTAL_VALUE="TotalValue";
	public static final String PAYMENT_TYPE="PaymentType";
	public static final String PAYMENT_TEXT="PaymentText";
	public static final String EXTENSION_AMOUNT="ExtensionAmount";
	public static final String OLD_ORDER = "oldorderid";

	public static final String GIFT_CARD_ORDER = "GC";
	public static final String NEW_GEAR_ORDER = "New Gear";
	public static final String RENTAL = "Rental";
	public static final String USED_GEAR = "Used Gear";
	public static final String DATE_PATTERN = "yyyy-MM-dd";
	public static final String PO = "PO";

	public static final String PAY_PAL_PROVIDER  = "BrainTreePayPalExpress";
	public static final String PAY_PAL = "Paypal";

	public static final String ORDER_ITEM_ROOT = "orderitem";
	public static final String ORDERS = "Orders";
	public static final String ORDER = "Order";













	private BlespintegrationConstants()
	{
		//empty to avoid instantiating this constant class
	}

	// implement here constants used by this extension

	public static final String PLATFORM_LOGO_CODE = "blespintegrationPlatformLogo";
}
