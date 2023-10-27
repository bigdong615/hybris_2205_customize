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


	public static final String SFTPHOST = "ftp.host.name";
	public static final String SFTPPORT = "ftp.port.number";
	public static final String SFTPUSER = "ftp.user.name";
	public static final String SFTPPASS = "ftp.user.password";
	public static final String XML_INDENT = "{http://xml.apache.org/xslt}indent-amount";
	public static final String XML_INDENT_SIZE = "xml.indent.size";
	public static final String YES = "yes";
	public static final String FILE_FORMAT = "yyyyMMddHHmm";
	public static final String FILE_SUFFIX = ".xml";
	public static final String LOCAL_FTP_PATH = "local.sftp.file.path";
	public static final String STICT_HOST_KEY = "StrictHostKeyChecking";
	public static final String NO = "no";
	public static final String SFTP = "sftp";
	public static final String CLIENT_FTP_PATH = "client.ftp.file.path";
	public static final String FILE_NAME_PREFIX = "bl_order_master_feed_";
	public static final String BILL_FILE_NAME_PREFIX = "bl_order_bill_feed_";
	public static final String SFTP_RETURN_ORDER_HOST = "ftp.host.return.order.name";
	public static final String SFTP_RETURN_ORDER_PASS = "ftp.user.return.order.password";
	public static final String RETUNR_ORDER_FILE_NAME_PREFIX = "bl_return_order_master_feed";
	public static final String RETURN_ORDER_FILE_SUFFIX = ".csv";


	public static final String SHIPPING_ROOT_ELEMENT = "shippinginfo";
	public static final String SHIPPING_FIRST_NAME = "Shipping_First_Name";
	public static final String SHIPPING_LAST_NAME = "Shipping_Last_Name";
	public static final String SHIPPING_ORGANIZATION = "Shipping_Organization";
	public static final String SHIPPING_ADDRESS_1 = "Shipping_Address_1";
	public static final String SHIPPING_ADDRESS_2 = "Shipping_Address_2";
	public static final String SHIPPING_CITY = "Shipping_City";
	public static final String SHIPPING_STATE = "Shipping_State";
	public static final String SHIPPING_ZIP_CODE = "Shipping_Zipcode";
	public static final String SHIPPING_PHONE = "Shipping_Phone";
	public static final String SHIPPING_EMAIL = "Shipping_Email";
	public static final String SHIPPING_HOURS = "Shipping_Hours";
	public static final String SHIPPING_NOTES = "Shipping_Notes";

	public static final String ORDER_ITEMS_ROOT_ELEMENT = "orderitemsinfo";
	public static final String ORDER_ITEM_ROOT_ELEMENT = "orderitems";
	public static final String ORDER_ITEM_PRODUCT_CODE = "Product_Code";
	public static final String ORDER_ITEM_PRODUCT_TITLE = "Product_Title";
	public static final String ORDER_ITEM_PRODUCT_PHOTO = "Product_Photo";
	public static final String ORDER_ITEM_RENTAL_PRICE = "Rental_Price";
	public static final String ORDER_ITEM_DAMAGE_WAIVER_PRICE = "Damage_Waiver_Price";
	public static final String ORDER_ITEM_DAMAGE_WAIVER_TEXT = "Damage_Waiver_Text";
	public static final String ORDER_ITEM_TOTAL_PRICE = "Total_Price";
	public static final String ORDER_ITEM_QUANTITY = "quantity";

	public static final String ITEMS_ROOT_ELEMENT = "itemsinfo";
	public static final String ITEM_ROOT_ELEMENT = "item";
	public static final String ITEM_PRODUCT_URL = "producturl";
	public static final String ITEM_AMOUNT_DUE_ROOT_ELEMENT = "amountdue";
	public static final String ITEM_NOTES_ROOT_ELEMENT = "notes";


	public static final String BILLING_ROOT_ELEMENT = "billinginfo";
	public static final String BILLING_FIRST_NAME = "Billing_First_Name";
	public static final String BILLING_LAST_NAME = "Billing_Last_Name";
	public static final String BILLING_ORGANIZATION = "Billing_Organization";
	public static final String BILLING_ADDRESS_1 = "Billing_Address_1";
	public static final String BILLING_ADDRESS_2 = "Billing_Address_2";
	public static final String BILLING_CITY = "Billing_City";
	public static final String BILLING_STATE = "Billing_State";
	public static final String BILLING_ZIP_CODE = "Billing_Zipcode";
	public static final String BILLING_PHONE = "Billing_Phone";
	public static final String BILLING_EMAIL = "Billing_Phone";
	public static final String BILLING_NOTES = "Order_Notes";
	public static final String BILLING_GIFT_CARD_USED = "Gift_Card_Used";
	public static final String BILLING_GIFT_CARD_BALANCE = "Gift_Card_Balance";
	public static final String CUSTOMER_CHECKOUT_ORDER_NOTES = "CUSTOMER_CHECKOUT_ORDER_NOTES";
	public static final String OLD_ORDER_ID = "old order id";
	public static final String ORDER_ITEM_ID = "Order_Item_ID";

	public static final String PRODUCT_CATALOG = "catalog";
	public static final String VERSION = "version";
	public static final String STAGED = "Staged";
	public static final String CATALOG_VALUE = "blProductCatalog";
	public static final String COLON = ":";
	public static final String ONLINE = "Online";

	public static final String ORDER_ID = "Order_ID";
	public static final String TEMPLATE="Template";
	public static final String SUBSCRIBER_ID="Subscriber_ID";
	public static final String EMAIL_ADDRESS="Email_Address";
	public static final String TYPE="Type";
	public static final String REPLACEMENT="Replacement";
	public static final String STATUS = "Status";
	public static final String DATE_PLACED="Date_Placed";
	public static final String SHIPPING_METHOD_TYPE="Shipping_Method_Type";
	public static final String SHIPPING_METHOD="Shipping_Method";
	public static final String SHIPPING_METHOD_TEXT="Shipping_Method_Text";
	public static final String TRACKING_INFO="Tracking_Info";
	public static final String ITEM_COST="Item_Cost";
	public static final String DAMAGE_WAIVER_COST="Damage_Waiver_Cost";
	public static final String SUB_TOTAL="Subtotal";
	public static final String SHIPPING_AMOUNT="Shipping_Amount";
	public static final String TAX_AMOUNT="TaxAmount";
	public static final String DISCOUNT_AMOUNT="Discount_Amount";
	public static final String TOTAL_COST="Total_Cost";
	public static final String DISCOUNT_TEXT="Discount_Text";
	public static final String EXPECTED_SHIPPING_DATE="Expected_Shipping_Date";
	public static final String ARRIVAL_DATE="Arrival_Date";
	public static final String RETURN_DATE="Return_Date";
	public static final String ACTUAL_RETURN_DATE="Actual_Return_Date";
	public static final String RENTAL_DURATION="Rental_Duration";
	public static final String CUSTOMER_NAME="Customer_Name";
	public static final String VERIFICATION_LEVEL="Verification_Level";
	public static final String COI_AMOUNT="COIAmount";
	public static final String COI_EXPIRATION_DATE="COIExpirationDate";
	public static final String TOTAL_VALUE="Total_Value";
	public static final String PAYMENT_TYPE="Payment_Type";
	public static final String PAYMENT_TEXT="Payment_Text";
	public static final String EXTENSION_AMOUNT="Extension_Total";
	public static final String OLD_ORDER = "Old_Order_ID";
	public static final String RETURNING_CUSTOMER = "Returning_Customer";
	public static final String LAST_UPDATED = "Last_Updated";
	public static final String TOTAL_BILL_AMOUNT ="Total_Bill_Amount";

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
	public static final String COMMA = ",";
	public static final String SLASH = "/";
	public static final String Order_Bill = "OrderBill";
	public static final String ORDER_BILL_ITEMS = "orderBillItems";
	public static final String PRODUCT_IMAGE_THUMBNAIL = "Product_Image_Thumbnail";
	public static final String GEAR_GUARD = "GearGuard";
	public static final String ORDER_BILL_ITEM = "orderBillItem";
	public static final String TOTAL = "Total";
	public static final String PAID = "Paid";
	public static final String NOT_PAID = "Not Paid";
	public static final String BILL_TYPE = "Bill_Type";
	public static final String NOTES = "Notes";
	public static final String BILL_CREATED_DATE = "Bill_Created_Date";
	public static final String BILL_QUANTITY = "1";


	public static final String LATE_ORDER_FILE_NAME_PREFIX = "bl_late_orders_feed_";
	public static final String LOCAL_FTP_PATH_LATE_ORDER = "local.sftp.file.path.late.order";










	private BlespintegrationConstants()
	{
		//empty to avoid instantiating this constant class
	}

	// implement here constants used by this extension

	public static final String PLATFORM_LOGO_CODE = "blespintegrationPlatformLogo";
}
