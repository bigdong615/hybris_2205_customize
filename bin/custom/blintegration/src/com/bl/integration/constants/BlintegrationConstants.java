/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.integration.constants;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;

import java.util.Arrays;
import java.util.List;


/**
 * Global class for all Blintegration constants. You can add global constants for your extension into this class.
 */
public final class BlintegrationConstants extends GeneratedBlintegrationConstants
{
	public static final String EXTENSIONNAME = "blintegration";
	public static final String AUTHORIZATION_SCOPE_PROPERTY = EXTENSIONNAME + ".oauth.scope";
	public static final String LICENSE_URL_PROPERTY = EXTENSIONNAME + ".license.url";
	public static final String TERMS_OF_SERVICE_URL_PROPERTY = EXTENSIONNAME + ".terms.of.service.url";
	public static final String LICENSE_PROPERTY = EXTENSIONNAME + ".licence";
	public static final String DOCUMENTATION_DESC_PROPERTY = EXTENSIONNAME + ".documentation.desc";
	public static final String DOCUMENTATION_TITLE_PROPERTY = EXTENSIONNAME + ".documentation.title";
	public static final String API_VERSION = "1.0.0";

	public static final String AUTHORIZATION_URL = "/authorizationserver/oauth/token";
	public static final String PASSWORD_AUTHORIZATION_NAME = "oauth2_password";
	public static final String CLIENT_CREDENTIAL_AUTHORIZATION_NAME = "oauth2_client_credentials";

	public static final String SAMPLE_MAP_STRING_KEY = "StringKey";
	public static final String SAMPLE_MAP_STRING_VALUE = "StringValue";
	public static final String SAMPLE_MAP_INTEGER_KEY = "integerKey";
	public static final int SAMPLE_MAP_INTEGER_VALUE = 10001;

	public static final String SAMPLE_LIST_STRING_VALUE = "new String";
	public static final double SAMPLE_LIST_DOUBLE_VALUE = 0.123d;

	public static final String HOST = "blintegration.host";
	public static final String HOST_DEFAULT = "hostname";

	public static final String PICKUP_ZIP_CODE = "pickUpZipCode";
	public static final String DROPOFF_ZIP_CODE = "dropOffZipCode";
	public static final String X_API_KEY = "X-Api-Key";
	public static final String CONTENT_TYPE = "Content-Type";
	public static final String AUTHORIZATION = "Authorization";
	public static final String Q_NAME_CODE = "ShipService";
	public static final String Q_NAME_TRACK_CODE = "TrackService";

	public static final String INTERNAL_SERVER_ERROR_CODE = "500";
	public static final String FAILURE_STRING = "Failure";
	public static final String ADDRESS_TYPE_UNKNOWN = "UNKNOWN";
	public static final String RESIDENTIAL_ADDRESS_TYPE_CODE = "2";
	public static final String BUSINESS_ADDRESS_TYPE_CODE = "1";

	public static final String REQUEST_OPTION = "nonvalidate";
	public static final String CUSTOMER_CONTEXT = "JAX-WS Test Client";
	public static final String DELIVERY_CONFIRMATION_SIGNATURE = "1";

	public static final String CLIENT_SIDE_ERROR = "400";
	public static final String CLIENT_SIDE_ERROR_DESCRIPTION = "Client Trasportation Error";
	public static final String LABEL_IMAGE_FORMAT_CODE = "ZPL";
	public static final String LABEL_IMAGE_FORMAT_DESCRIPTION = "ZPL";
	public static final String LABEL_SPECIFICATION_HTTPUSERAGENT = "Mozilla/4.5";
	public static final String LABEL_INDICATOR = "1";
	public static final String PACKAGE_DESCRIPTION = "Package 1";
	public static final String SHIPMENT_DESCRIPTION = "Some Goods";
	public static final String UTF_8_CODE = "utf-8";
	public static final String PACAKAGING_TYPE_CODE = "02";
	public static final String PACAKAGING_TYPE_DESCIPTION = "Customer Supplied Package";
	public static final String SIGNATURE_SERVICE_CODE = "SERVICE_DEFAULT";
	public static final String CATEGORY_CODE = "CONSUMER_GOODS";
	public static final String RECIPIENT_TYPE = "RECIPIENT";
	public static final String SUCCESS = "success";
	public static final String POPUP_TEXT = "Message:";
	public static final String ERROR_TEXT = "Error Message:";
	public static final String UPS_SHIPMENT_MSG = "Shipment Call Started.....";
	public static final String RETURN_SHIPMENT_MSG = "Return Shipment Call Started.....";
	public static final String DEFAULT_WAREHOUSE_CODE = "OPTIMISED";
	public static final String DEFAULT_SHIPPING_CODE = "DEFAULT";
	public static final String WAREHOUSE_MA = "warehouse_ma";
	public static final String WAREHOUSE_CA = "warehouse_ca";
	public static final String WEIGHT_UNIT = "LB";
	public static final String DIMENSION_UNIT = "IN";
	public static final String SIMPLE_DATE_FORMAT = "hh";
	public static final String INBOUND_PACKAGE = "InboundPackage";
	public static final String OUTBOUND_PACKAGE = "OutboundPackage";
	public static final String MIME_TYPE = "x-application/zpl";
	public static final String FILE_FORMAT = ".zpl";
	public static final String UNDERSCORE = "_";
	public static final String EMPTY_STRING = "";


	public static final String FEDEX_ACCOUNT_NUMBER = "ups.scrape.fedex.acount.number";
	public static final String FEDEX_METER_NUMBER = "ups.scrape.fedex.meter.number";
	public static final String FEDEX_PARENT_API_KEY = "fedex.parent.api.key";
	public static final String FEDEX_PARENT_API_PASSWORD = "fedex.parent.api.password";
	public static final String FEDEX_USER_API_KEY = "fedex.user.api.key";
	public static final String FEDEX_USER_API_PASSWORD = "fedex.user.api.password";
	public static final String IN_BOUND_OR_OUT_BOUND = "inbound";
	public static final String HYPHEN = "-";
	public static final String TRCK = "trck";
	public static final String FEDEX_API_URL = "fedex.api.url";
	public static final String UPS_API_URL = "ups.api.url";
	public static final String UPS_API_USER_NAME = "ups.token.user.name";
	public static final String UPS_API_PASSWORD = "ups.token.password";
	public static final String UPS_API_LICENSE = "ups.token.license";
	public static final String RETURN_LABEL_CODE = "03";
	public static final String RETURN_LABEL_DESC = "GROUND";

	public static final String ESTIMATED_DELIVERY_TIME_STAMP = "EstimatedDeliveryTimestamp";
	public static final String TRACKING_NUMBER = "TrackingNumber";
	public static final String DELIVERED = "Delivered";
	public static final String STATUS_DESCRIPTION = "StatusDescription";
	public static final String TRACK_EVENTS = "TrackEvents";
	public static final String DESCRIPTION = "Description";
	public static final int END_HOURS = 23;
	public static final int END_MINUTES = 59;
	public static final int END_SECONDS = 59;
	public static final int START_HOURS = 0;
	public static final int START_MINUTES = 0;
	public static final int START_SECONDS = 0;
	public static final String CODE = "code";
	public static final String FROM = "} FROM {";
	public static final String OPTIMIZED_SHIPPING_START_DATE = "optimizedShippingStartDate";
	public static final String OPTIMIZED_SHIPPING_END_DATE = "optimizedShippingEndDate";
	public static final String PACKAGE_RETURNED_TO_WAREHOUSE = "packageReturnedToWarehouse";
	public static final String IS_SCRAPE_SCAN_COMPLETED = "isScrapeScanCompleted";
	public static final String START_DATE = "startDate";
	public static final String END_DATE = "endDate";
	public static final String DATE_FORMATTER = "yyyy-MM-dd HH:mm:ss";
	public static final String DATE_FORMAT = "yyyy-MM-dd";
	public static final String UTC = "UTC";
	public static final String SCRAPE_TYPE = "upsScrapeServiceType";
	public static final String FEDEX_TYPE = "fedex";
	public static final String UPS_TYPE = "UPS";
	public static final String STATUS_TYPE = "StatusType";
	public static final String M = "M";
	public static final String MV = "MV";
	public static final String REQUEST_OPTION_NUMBER = "1";
	public static final String TRACKING_OPTION = "02";
	public static final String SERVICE_TYPE = "ServiceType";
	public static final String SERVICE_DESCRIPTION = "ServiceDescription";
	public static final String SHIPMENT_WEIGHT = "ShipmentWeight";
	public static final String SHIP_TIME_STAMP = "ShipTimestamp";
	public static final String A_PACKAGE_WAS_DELIVERED = "APackageWasDelivered";
	public static final String A_PACKAGE_WAS_DELIVERED_ON = "APackageWasDeliveredOn";
	public static final String DELIVERY_TIME_STAMP = "DeliveryTimestamp";
	public static final String SCHEDULED_DELIVERY_TIME_STAMP = "ScheduledDeliveryTimestamp";
	public static final String RESCHEDULED_DELIVERY_TIME_STAMP = "RescheduledDeliveryTimestamp";
	public static final String STATUS_CODE = "StatusCode";
	public static final String ACTIVITY_TIME_STAMP = "ActivityTimeStamp";
	public static final String PACKAGE_COUNT = "PackageCount";
	public static final String ACTIVITY_COUNT = "ActivityCount";
	public static final String ZERO = "0";
	public static final String DELIVERY_DETAIL_ONE = "01";
	public static final String DELIVERY_DETAIL_TWO = "02";
	public static final String DELIVERY_DETAIL_THREE = "03";
	public static final String DELIVERY_DETAIL_FOUR = "04";
	public static final String PACAKAGE_ACTIVITY_D = "D";
	public static final String PACAKAGE_ACTIVITY_P = "P";
	public static final String PACAKAGE_ACTIVITY_I = "I";
	public static final String DEFAULT = "000000";
	public static final String TRACKING_NUMBER_IDENTIFIER = "TrackingNumberUniqueIdentifier";
	public static final String PACAKAGE_SEQUENCE_NUMBER = "PackageSequenceNumber";
	public static final String PACKAGING = "Packaging";
	public static final String PACKAGE_WEIGHT = "PackageWeight";
	public static final String SHIP = "SHIP";
	public static final String ESTIMATED_DELIVERY = "ESTIMATED_DELIVERY";
	public static final String TIME_STAMP = "Timestamp";
	public static final String CITY = "City";
	public static final String STATE = "State";
	public static final String ADDRESS = "Address";
	public static final String POSTAL_CODE = "PostalCode";
	public static final String COUNTRY_CODE = "CountryCode";
	public static final String DESTINATION_ADDRESS = "DestinationAddress";
	public static final String NAME_MAX_CHARACTER = "name.max.character";


	public static final String FEDEX_SERVICE_ID = "ship";
	public static final int FEDEX_MAJOR = 28;
	public static final int FEDEX_INTERMEDIATE = 0;
	public static final int FEDEX_MINOR = 0;
	public static final String ONE = "1";
	public static final String TWO = "1";
	public static final String FEDEX_MASTER = "Master";
	public static final String FEDEX_CHILD = "Child";
	public static final String FEDEX_PACKAGING_TYPE = "YOUR_PACKAGING";
	public static final String FEDEX_COUNTRY_CODE = "US";
	public static final String FEDEX_SERVICE_TYPE = "PRIORITY_OVERNIGHT";
	public static final String OUT_BOUND_LABEL = "outbound";
	public static final String IN_BOUND_LABEL = "inbound";
	public static final int SIXTEEN = 16;
	public static final int EIGHTEEN = 18;
	public static final int THIRTY_ONE = 31;
	public static final int TWENTY = 20;
	public static final int MINUS_SEVEN = -7;
	public static final String END_POINT = "endPoint";
	public static final String DELIVERY_ZIP_CODE_SF = "95054";
	public static final String DELIVERY_ZIP_CODE_NYC = "10109";
	public static final int STATUS_CODE_200 = 200;
	public static final int STATUS_CODE_201 = 201;
	public static final int STATUS_CODE_400 = 400;
	public static final String UPS_DATE_FORMAT = "yyyyMMdd";
	public static final String WHITE_SPACE = " ";
	public static final String DS = "DS";
	public static final String OT = "OT";

	//Below is the list of order status and consignment status for which the tools (create package, shipping scan and mark shipment as BL_SHIPPED) in shipment module will not be visible.
	public static final List ORDERS_AND_CONSIGNMENT_STATUS = Arrays.asList(OrderStatus.CHECKED_INVALID,
			OrderStatus.PAYMENT_NOT_AUTHORIZED, OrderStatus.RECEIVED_PAYMENT_DECLINED, OrderStatus.RECEIVED_IN_VERIFICATION,
			OrderStatus.WAIT_FRAUD_MANUAL_CHECK, OrderStatus.PAYMENT_NOT_CAPTURED, ConsignmentStatus.WAITING,
			ConsignmentStatus.PAYMENT_NOT_CAPTURED);

	private BlintegrationConstants()
	{
		//empty to avoid instantiating this constant class
	}

	// implement here constants used by this extension
}
