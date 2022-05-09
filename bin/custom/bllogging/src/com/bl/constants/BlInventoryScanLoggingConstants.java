package com.bl.constants;

import java.util.ArrayList;
import java.util.List;

/**
 * {javadoc}
 *
 * @auther Namrata Lohar
 * Scanning specific response messages
 */
public class BlInventoryScanLoggingConstants {

	public static final int ZERO = 0;
    public static final double ZERO_FIVE = 0.5;
    public static final int ONE = 1;
    public static final int TWO = 2;
    public static final int THREE = 3;
    public static final int FOUR = 4;
    public static final int FIVE = 5;
    public static final int SIX = 6;
    public static final int SEVEN = 7;
    public static final int EIGHT = 8;
    public static final int TEN = 10;
    public static final int ELEVEN = 11;
    public static final int FIFTEEN = 15;
    public static final int FORTY = 40;
    public static final int HUNDERED = 100;

    public static final String HUN_PER = "100%";
    public static final String SAVING = "Saving ";
    public static final String SPACE = ": ";
    public static final String EMPTY_SPACE = " ";
    public static final String OPEN_BRACKET = "[";
    public static final String CLOSE_BRACKET = "]";
    public static final String COUNTRY_CODE = "US";
    public static final String SHIPPING_METHOD_ERROR_MESSAGE = "Selected shipping method service is not applicable for added zip code";
    public static final String SHIPPING_DETAILS_UPDATE_MESSGAE = "Shipping Details Updated Successfully";
    public static final String SAMPLE1 = "sample1%";
    public static final String SAMPLE2 = "sample2%";
    public static final String SAMPLE3 = "sample3%";
    public static final String LOCATION = "MAR001 S01";

    public static final String BIN = "BIN";
    public static final String MA = "MA";
    public static final String CA = "CA";
    public static final String FEDEX = "FedEx";
    public static final String UPS = "UPS";
    public static final String USPS = "USPS";
    public static final String VIP = "VIP";
    public static final String EMPLOYEE = "Employee";
    public static final String BOS = "BOS";
    public static final String IN_HOUSE = "In House Scrapped Items";
    public static final String UNKNOWN_CARRIER = "Unknown Carrier";
    public static final String UNKNOWN = "UNKNOWN";
    public static final String MAW = "MAW";
    public static final String CAW = "CAW";
    public static final String ORD = "ORD";
    public static final String BIN_LOCATION = "FDSHIP01";
    public static final String CARRIER = "CARRIER";
    public static final String BOXING_FEDEX = "FEDEX";
    public static final String FD_MOBILE_SHELF = "FDSHIPPED";
 	public static final String ITEM_TEXT = " Item :";
 	public static final String PRODUCT_TEXT = " for product : ";

    public static final String FAILED_BARCODE_LIST = "Failed barcode list: ";

    public static final String NOTIFICATION_HANDLER = "webSanToolNotification";
    public static final String ICC_NOTIFICATION_HANDLER = "inventoryCycleCountNotification";
    public static final String ONE_ITEM_SCAN_ERROR_FAILURE = "oneBarcodeLimitErrorFailure";
    public static final String MAX_BARCODE_LIMIT_ERROR_FAILURE = "maxBarcodeLimitErrorFailure";
    public static final String MAX_BARCODE_LIMIT_EQ_ERROR_FAILURE = "maxBarcodeLimitEQErrorFailure";
    public static final String MAX_BIN_LIMIT_ERROR_FAILURE = "maxBinLimitErrorFailure";
    public static final String MUST_TWO_BARCODE_ERROR_FAILURE = "mustTwoBarcodeErrorFailure";
    public static final String SCAN_BATCH_ERROR_FAILURE = "scanBatchErrorFailure";
    public static final String SCAN_BARCODE_SUCCESS = "scanBarcodeSuccess";
    public static final String SCAN_BIN_SUCCESS = "scanBINSuccess";
    public static final String LAST_SCAN_INVALID_ERROR_FAILURE = "lastScanInvalidErrorFailure";
    public static final String LAST_SCAN_ERROR_FAILURE = "lastScanErrorFailure";
    public static final String MANY_LOCATION_ERROR_FAILURE = "manyLocationErrorFailure";
    public static final String WEB_SAN_TOOL_NOTIFICATION_FAILURE = "webSanToolNotificationFailure";
    public static final String ICC_NO_ACTIVE_NOTIFICATION_FAILURE = "iccNoActiveNotificationFailure";
    public static final String WEB_SCAN_TOOL_DATA_MODEL_KEY = "webScanToolDataModelKey";
    public static final String INVENTORY_CYCLE_COUNT_SCAN_TOOL_DATA_MODEL_KEY = "inventoryCycleCountScanToolDataModelKey";
    public static final String VALID_BIN_LOCATION_ERROR_FAILURE = "validBINLocationErrorFailure";
    public static final String VALID_PARENT_LOCATION_ERROR_FAILURE = "validParentLocationErrorFailure";
    public static final String ICC_INPUT_EMPTY_ERROR_NOTIF = "iccEmptyInputNotificationFailure";
    public static final String ICC_SUCCESS_NOTIF = "scanICCSuccess";
    public static final String ICC_SERIAL_ERROR_NOTIF = "scanICCSerialError";
    public static final String ICC_SERIAL_DB_ERROR_NOTIF = "scanICCSerialDBError";

    public static final String ONE_ITEM_SCAN_ERROR_FAILURE_MSG = "First scan is blank. First scan must be an item or a BIN";
    public static final String MAX_BARCODE_LIMIT_ERROR_FAILURE_MSG = "Invalid number of barcodes in sequence, max: {}";
    public static final String MAX_BARCODE_LIMIT_EQ_ERROR_FAILURE_MSG = "Maximum number of item scans reached without a location scan: {}";
    public static final String MAX_BIN_LIMIT_ERROR_FAILURE_MSG = "If the first scan is a BIN, only 2 scans are allowed and the second must be a location";
    public static final String MUST_TWO_BARCODE_ERROR_FAILURE_MSG = "Must have two barcode scans or more.";
    public static final String TWO_BARCODE_SCAN_ERROR_MSG = "Must have two barcode to scans";
    public static final String SCAN_BATCH_ERROR_FAILURE_MSG = "Failed to update inventory location for: {}";
    //public static final String SCAN_BATCH_ERROR_FAILURE_MSG = "scanBatchErrorFailure";

    public static final String SCAN_BARCODE_SUCCESS_MSG = "Successfully records scanned!!";
    public static final String LAST_SCAN_INVALID_ERROR_FAILURE_MSG = "Last scan must be valid Inventory Location";
    public static final String BIN_TYPE_ERROR_MSG = "{} : Not a BIN type location";
    public static final String LAST_SCAN_DPC_OR_DC_TYPE_ERROR_MSG = "If the first scan is a BIN, the second must be a Dirty Priority Cart or Dirty Cart location";
    public static final String LAST_SCAN_ERROR_FAILURE_MSG = "If the first scan is a tracking number, all following scans must be tracking numbers and the last scan must be a location";
 	public static final String MANY_LOCATION_ERROR_FAILURE_MSG = "Only a single location scan is allowed and it must be last when scanning in tracking numbers to locations";
 	public static final String FIRST_SCAN_EMPTY_ERROR_FAILURE_MSG = "First scan is blank. First scan must be an item or a BIN or a tracking number";
 	public static final String MIX_SCAN_EMPTY_ERROR_FAILURE_MSG = "An item or bin scan is not allowed when the first scan is a tracking number";

 	public static final String WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG = "Something went wrong!!";
 	public static final String SHIPPING_MAX_SCAN_ERROR_MSG = "Invalid number of barcodes in sequence";
 	public static final String INVALID_BARCODE_SCANNED_MSG = "Scanned barcodes are not valid {}";
 	public static final String SERIAL_MISMATCH_SCANNED_MSG = "{} is not assigned to orders present in tracking number: {} ";
 	public static final String PACKAGE_SERIAL_MISMATCH_SCANNED_MSG = "Could not find a matching order for tracking number: {} {}";

 	public static final String INVALID_SCAN_MSG = "Invalid Scan";

 	public static final String SHIPPING_MANUAL_REVIEW_FAILURE_MSG = "{} for product: {} should not processed for Shipping or Boxing as Consignment status = 'Shipping Manual Review'";
	public static final String SHIPPING_CANCEL_ORDER_FAILURE_MSG = "{} for product: {} should not processed for Shipping or Boxing as Consignment status = 'Cancelled'";
	public static final String SHIPPING_SCAN_CANCEL_ORDER_FAILURE_MSG = "This scan should not be processed for Shipping or Boxing as Consignment status = 'Cancelled'";
	public static final String SHIPPING_SCAN_MANUAL_REVIEW_FAILURE_MSG = "This scan should not be processed for Shipping or Boxing as Consignment status = 'Shipping Manual Review";
	public static final String SERIAL_MISSING_ON_CONSIGNMENT_MSG = "Serial {} is missing on consignment ";
	public static final String SERIAL_MISSING_ON_SCAN_MSG = "Serial {} is missing on Scan ";

    public static final String VALID_BIN_LOCATION_ERROR_FAILURE_MSG = "Failed to update BIN inventory location for: {}";
    public static final String VALID_PARENT_LOCATION_ERROR_FAILURE_MSG = "Last scan must be valid Parent Inventory Location for BIN";


    public static final String MAX_SEQUENCE_LIMIT_KEY = "maxSequenceScan";
    public static final String SCAN_STRING = "Scan data: ";

    public static final String FETCH_INVENTORY_LOC = "Fetching InventoryLocation for input locationId: {}";
    public static final String FETCH_SERIAL_PROD = "Fetching SerialProducts for input barcode list: {}";
    public static final String FETCH_CONFIG_VALUE = "Fetching ConfigurationValue for key: {}";
    
    //public static final String FETCH_INVENTORY_LOC = "Fetching InventoryLocation for input locationId: ";
    //public static final String FETCH_SERIAL_PROD = "Fetching SerialProducts for input barcode list: ";
    //public static final String FETCH_CONFIG_VALUE = "Fetching ConfigurationValue for key: ";
    public static final String BAR_CODE_SCAN_EMPTY_BAR_CODES = "No Barcodes found to scan.";

    public static final String TECH_ENG_NOTIFICATION_HANDLER = "techEngSanToolNotification";
    public static final String TECH_ENGINEERING_WORKSTATION = "TECH_ENGINEERING_WORKSTATION";
    public static final String VIP_WORKSTATION_TECH_ENG = "VIP_WORKSTATION_TECH_ENG";
    public static final String CLEAN_PRIORITY_GEAR_CART = "CLEAN_PRIORITY_GEAR_CART";
    public static final String CLEAN_GEAR_MOBILE_CART = "CLEAN_GEAR_MOBILE_CART";
    public static final String REPAIR_SHELF = "REPAIR_SHELF";
    public static final String MISSING_BARCODE_ITEMS = "MISSING_BARCODE_ITEMS";
    public static final String SOMETHING_WENT_WRONG = "SOMETHING_WENT_WRONG";
    public static final String WRONG_ITEM_CLEAN_CART = "WRONG_ITEM_CLEAN_CART";
    public static final String WRONG_ITEM_CLEAN_PRIORITY_CART = "WRONG_ITEM_CLEAN_PRIORITY_CART";
    public static final String SUCCESS = "SUCCESS";
    public static final String LOG_SOMETHING_WENT_WRONG = "SOMETHING WENT WRONG";
    public static final String SCAN_ERROR_FAILURE = "somethingWentWrong";
    public static final String WRONG_CLEAN_CART_LOCATION = "Should Move To Clean Priority Cart";
    public static final String CLEAN_CART_SCAN_ERROR_FAILURE = "cleanCartScanError";
    public static final String WRONG_CLEAN_PRIORITY_CART_LOCATION = "Should Move To Clean Cart";
    public static final String CLEAN_PRIORITY_CART_SCAN_ERROR_FAILURE = "cleanPriorityCartScanError";
    public static final String CAR = "CAR";
    public static final String MAR = "MAR";
    public static final String CAM = "CAM";
    public static final String MAM = "MAM";
    public static final String CLEAN_FRONT_DESK_CART = "CLEAN_FRONT_DESK_CART";
    public static final String CLEAN_GEAR_AISLE_IN_CAGE = "CLEAN_GEAR_AISLE_IN_CAGE";
    public static final String CLEAN_GEAR_CAGE = "CLEAN_GEAR_CAGE";
    public static final String CLEAN_GEAR_REQUEST_PICKUP_MOBILE_CART = "CLEAN_GEAR_REQUEST_PICKUP_MOBILE_CART";
    public static final String CLEAN_GEAR_SHIPPING_MOBILE_CART = "CLEAN_GEAR_SHIPPING_MOBILE_CART";
    public static final String CLEAN_MOBILE_LAUNDRY_BIN = "CLEAN_MOBILE_LAUNDRY_BIN";
    public static final String CLEAN_PRIORITY_MOBILE_CART = "CLEAN_PRIORITY_MOBILE_CART";
    public static final String VIP_CLEAN_PRIORITY_GEAR = "VIP_CLEAN_PRIORITY_GEAR";
    public static final String SHIPPER_WORKSTATION = "SHIPPER_WORKSTATION";
    public static final String BOXING_WORKSTATION = "BOXING_WORKSTATION";
    public static final String FRONT_DESK_SHIPPER_WORKSTATION = "FRONT_DESK_MOBILE_CART";
 	public static final String FRONT_DESK_SHIPPING_BIN = "FRONT_DESK_SHIPPING_BIN";
 	public static final String FRONT_DESK_SHIPPED_MOBILE_CART = "FRONT_DESK_SHIPPED_MOBILE_CART";
    public static final String SCANNING_SUCCESS_MSG = "Scanning completed successfully";
 	public static final String LAST_SCAN_INVALID_ERROR_FAILURE_KEY = "blbackoffice.order.scan.last.location.error";
 	public static final String FIRST_SCAN_EMPTY_ERROR_FAILURE_KEY = "blbackoffice.order.scan.first.location.error";
	public static final String PACKAGE_SERIAL_MISMATCH_SCANNED_ERROR = "blbackoffice.order.package.serial.mismatch.error";
	public static final String MIX_SCAN_EMPTY_ERROR_FAILURE_KEY = "blbackoffice.order.scan.mix.location.error";
 	public static final String NO_ITEM_SCAN_KEY = "blbackoffice.order.scan.tool.noitem.scan.error";
 	public static final String INVALID_SCAN_ERROR = "blbackoffice.order.scan.invalid.scan.error";
 	public static final String MANY_LOCATION_ERROR_KEY = "blbackoffice.order.scan.many.location.error";
 	public static final String INVALID_LOCATION_ERROR = "blbackoffice.order.scan.invalid.location.error";
 	public static final String LAST_SCAN_LOCATION_ERROR_FAILURE_KEY = "blbackoffice.order.scan.last.invalid.location.error";
 	public static final String INVALID_BARCODE_SCANNED_ERROR = "blbackoffice.order.invalid.scan.error";
 	public static final String SERIAL_MISMATCH_SCANNED_ERROR = "blbackoffice.order.serial.mismatch.error";
 	public static final String INVALID_SCAN_ERROR_KEY = "blbackoffice.order.invali.scan.error.key";
 	public static final String TWO_BARCODE_SCAN_ERROR_KEY = "blbackoffice.order.scan.item.error";
	public static final String PACKAGE_MANUAL_REVIEW_FAILURE = "blbackoffice.order.manual.review.error.messge";
	public static final String PACKAGE_SCAN_MANUAL_REVIEW_FAILURE = "blbackoffice.order.manual.review.package.error.messge";
	public static final String PACKAGE_CANCEL_ORDER_FAILURE = "blbackoffice.order.cancel.order.error.messge";
	public static final String PACKAGE_SCAN_CANCEL_ORDER_FAILURE = "blbackoffice.order.cancel.package.scan.error.messge";
	public static final String LAST_LOCATION_VALID_TRACKING_FAILURE = "blbackoffice.order.last.location.tracking.id.error.messge";
	
 	public static final String SHIPPING_NO_ITEM_SCAN_KEY = "blbackoffice.shipping.scan.tool.noitem.scan.error";
 	public static final String SHIPPING_INVALID_SCAN_ERROR = "blbackoffice.shipping.scan.invalid.scan.error";
 	public static final String SHIPPING_TWO_BARCODE_SCAN_ERROR_KEY = "blbackoffice.shipping.scan.item.error";
 	public static final String SHIPPING_LAST_LOCATION_ERROR_KEY = "blbackoffice.shipping.scan.last.location.error";
 	public static final String SHIPPING_LAST_INVALID_LOCATION_ERROR = "blbackoffice.shipping.scan.last.invalid.location.error";
 	public static final String SHIPPING_MANY_LOCATION_ERROR = "blbackoffice.shipping.scan.many.location.error";
 	public static final String SHIPPING_INVALID_LOCATION_ERROR = "blbackoffice.shipping.scan.invalid.location.error";
 	public static final String SHIPPING_MANUAL_REVIEW_FAILURE = "blbackoffice.shipping.manual.review.error.messge";
	public static final String SHIPPING_SCAN_MANUAL_REVIEW_FAILURE = "blbackoffice.shipping.manual.review.scan.error.messge";
	public static final String SHIPPING_CANCEL_ORDER_FAILURE = "blbackoffice.shipping.cancel.order.error.messge";
	public static final String SHIPPING_SCAN_CANCEL_ORDER_FAILURE = "blbackoffice.shipping.cancel.order.scan.error.messge";
	public static final String SHIPPING_SERIAL_MISSING_ON_CONSIGNMENT_KEY = "blbackoffice.shipping.serial.missing.error.messge";
	public static final String SHIPPING_SERIAL_MISSING_ON_SCAN_KEY = "blbackoffice.shipping.scan.missing.error.messge";
	public static final String SHIPPING_FIRST_SCAN_EMPTY_ERROR_FAILURE_KEY = "blbackoffice.shipping.scan.first.location.error";
 	
	public static final String FRONT_DESK_MANUAL_REVIEW_FAILURE_MSG = "Can not scan order which is in SHIPPIN MANUAL REVIEW state";
	public static final String FRONT_DESK_CANCEL_ORDER_FAILURE_MSG = "Can not scan cancelled order";

	public static final String FRONT_DESK_NO_ITEM_SCAN_KEY = "blbackoffice.frontdesk.order.noitem.scan.error";
	public static final String FRONT_DESK_INVALID_LOCATION_ERROR = "blbackoffice.shipping.scan.invalid.location.error";
	public static final String FRONT_DESK_TWO_BARCODE_SCAN_ERROR_KEY = "blbackoffice.frontdesk.order.invalid.scan.error";
	public static final String FRONT_DESK_MANUAL_REVIEW_FAILURE = "blbackoffice.frontdesk.manual.order.review.error.messge";
	public static final String FRONT_DESK_CANCEL_ORDER_FAILURE = "blbackoffice.frontdesk.cancel.order.error.messge";
	public static final String FRONT_DESK_INVALID_SCAN_ERROR = "blbackoffice.frontdesk.scan.invalid.scan.error ";
	public static final String FRONT_DESK_SERIAL_MISSING_ON_CONSIGNMENT_KEY = "blbackoffice.frontdesk.serial.missing.error.messge";
	public static final String FRONT_DESK_SERIAL_MISSING_ON_SCAN_KEY = "blbackoffice.frontdesk.scan.missing.error.messge";
	public static final String FRONT_DESK_LOCATION_ERROR_KEY = "blbackoffice.frontdesk.scan.last.location.error";
	public static final String FRONT_DESK_LAST_INVALID_LOCATION_ERROR = "blbackoffice.frontdesk.scan.last.invalid.location.error";
	public static final String FRONT_DESK_MANY_LOCATION_ERROR = "blbackoffice.frontdesk.scan.many.location.error";


 	public static final String DOUBLE_HYPHEN = "--";
 	public static final String NEW_LINE = "\n";
 	public static final String ON_CHANGE_EVENT = "onChange";
 	public static final String ON_CLICK_EVENT = "onClick";
 	public static final String CANCEL_EVENT = "cancel";
 	public static final String VERIFY_SCAN_EVENT = "verifyScan";
 	public static final String SOCKET_ID = "inputObject";
 	public static final String SCANNING_AREA = "scanningArea";
 	public static final String SCAN_TO_BIN = "scanToBin";
 	public static final String GENERATE_INBOUND_LABEL = "generateInboundLabel";
 	public static final String SCAN_TO_WORKSTATION = "scanToWorkStation";
 	public static final String SCAN_TO_TRACKING_ID = "scanToTrackingLabel";
 	public static final String SCAN_TO_UPS_OUTBOUND = "scanToOutBoundCart";
 	public static final String WAREHOUSE_COMBOBOX = "warehouseCombobox";
 	public static final String EMPTY_STRING = "";
 	public static final String LAST_SCANNED_ITEM = "lastScannedItem";
    public static final String UNBOX_NOTIFICATION_HANDLER = "UnboxingScanToolNotification";
    public static final String UNBOX_SAN_TOOL_PACKAGE_FAILURE = "unboxPkgMissingErrorFailure";
    public static final String UNBOX_SAN_TOOL_DPC_FAILURE = "unboxInvalidDPLocationErrorFailure";
    public static final String UNBOX_SAN_TOOL_DPC_WARNING = "unboxInvalidDPLocationErrorWarning";
    public static final String UNBOX_SAN_TOOL_DC_FAILURE = "unboxInvalidDCLocationErrorFailure";
    public static final String UNBOX_SAN_TOOL_PACKAGE_FAILURE_MSG = "Scanned serials not exist on any of the package: ";
    public static final String UNBOX_SAN_TOOL_DPC_FAILURE_MSG = "Please, place this item to the dirty priority shelf: ";
    public static final String UNBOX_SAN_TOOL_DC_FAILURE_MSG = "Please, place this item to the dirty cart shelf: ";
    public static final String FETCH_PACKAGE_DETAILS = "Fetching PackagingDetails for serial barcodes {} and found {} packages.";
    public static final String FETCH_OUT_ORDER_DETAILS = "Fetching OUT orders for today. Found {} orders";
    public static final String FETCH_OUT_ORDER_SERIAL = "Fetching OUT orders for serial: {} and found {} orders";
    public static final String FETCH_OUT_TODAYS_ORDER_SERIAL = "Fetching OUT todays orders for serial: {} and found {} orders";
    
    public static final String UNBOXING_WORKSTATION = "UNBOXING_WORKSTATION";
    public static final String UNBOXING_WORKSTATION_LOCATION = "VIP_WORKSTATION_UNBOX";
    public static final String DIRTY_PRIORITY_GEAR = "DIRTY_PRIORITY_GEAR";
    public static final String DIRTY_PRIORITY_MOBILE_CART = "DIRTY_PRIORITY_MOBILE_CART";
    public static final String DIRTY_GEAR_MOBILE_CART = "DIRTY_GEAR_MOBILE_CART";
    public static final String DIRTY_MOBILE_LAUNDRY_BIN = "DIRTY_MOBILE_LAUNDRY_BIN";
    public static final String TRUE_STRING = "true";
    public static final String SPLIT_STRING = "##";

 	public static final String MISSING_IN_CONSIGNMENT = "MISSING_IN_CONSIGNMENT";
 	public static final String MISSING_IN_SCAN = "MISSING_IN_SCAN";
 	public static final String MISSING_SCAN_BARCODE = "MISSING_SCAN_BARCODE";
 	public static final String SUCCESS_SCAN = "SUCCESS_SCAN";
    public static final String IS_PAYMENT_PAGE_VISITED = "isPaymentPageVisited";
    public static final String CUSTOMER_SUPPORT_AGENT_GROUP = "customersupportagentgroup";
    public static final String ERROR_EXIST = "ERROR_EXIST";
    public static final String ALLOW_SCAN = "ALLOW_SCAN";
    public static final String REPAIR_CLEAN_MOBILE_CART = "REPAIR_CLEAN_MOBILE_CART";
    public static final String REPAIR = "REPAIR";                   
    public static final String REPAIR_CABINET = "REPAIR_CABINET";           
    public static final String REPAIR_MOBILE_SHELF = "REPAIR_MOBILE_SHELF";      
    public static final String REPAIR_MOBILE_LAUNDRY_BIN = "REPAIR_MOBILE_LAUNDRY_BIN";

    public static final String SERIAL_HARD_ASSIGN = "Serial {} is hard assigned to true ";
    public static final String FOR = " for ";
    public static final String MUST_HAVE_BIN_LOCATION_ID = "A Bin OcLocation Id Must be provided";
    public static final String LOCATION_NOT_FOUND = "Location Not Found";
    public static final String BIN_OCLOCATION_NOTIFICATION_HANDLER = "binOcLocScanToolNotification";
  public static final String BIN_OCLOCATION_TOOL_NOTIFICATION_FAILURE = "binOcLocScanToolNotificationFailure";
  public static final String MUST_ONE_LOCATION_ERROR_FAILURE = "mustOneLocationErrorFailure";
  public static final String NO_LOCATION_ERROR_FAILURE = "noLocationErrorFailure";
  public static final String FETCH_SERIAL_PROD_LOC = "Fetching SerialProducts for input Location: {}";
  public static final String CONTENT_CLEARED_FROM_BIN = "Content Cleared for the Bin with Id: {}";
  public static final String NO_RELATED_ITEMS_FOUND = "No related serial or parent Location found to clear";
  public static final String NO_RELATED_ITEMS_FOUND_FAILURE = "noRelatedItemsFoundFailure";
  public static final String TYPE_NOT_SUPPORTED = "Type Not Supported";
  public static final String TYPE_NOT_SUPPORTED_ERROR_FAILURE = "typeNotSupportedErrorFailure";

    //Inventory Cycle Count
    public static final String START_PERFORMING_BL_AUTHORIZE_PAYMENT_JOB = "Start performing BlAuthorizePaymentJob...!!";
    public static final String FINISHED_PERFORMING_BL_AUTHORIZE_PAYMENT_JOB = "Finished performing BlAuthorizePaymentJob...!!";
    public static final String FAILED_TO_PERFORM_BL_AUTHORIZE_PAYMENT_JOB_AS_IT_S_WEEKEND = "Failed to perform BlAuthorizePaymentJob as it's weekend!!";

    public static final String FETCH_ACTIVE_INVENTORY_CYCLE_COUNT = "Fetch active inventory cycle count record!!";
    public static final String FETCH_ALL_SKUs = "Fetch all active SKUs considering serials status!!";
    public static final String ICC_DAY = "ICC_Day_";
    public static final int TWENTY_NINE = 29;
    public static final int THIRTY = 30;
    public static final String PREVIOUS_CYCLE_COUNT_NOT_ENDED_YET_FOR_CODE = "Previous Cycle count not ended yet for code: {}";
    public static final String PREVIOUS_CYCLE_COUNT_ENDED_FOR_CODE = "Previous Cycle count ended yet for code: {}";
    public static final String PREVIOUS_CYCLE_COUNT_ENDED = "No active inventory cycle found!!";
    public static final String DEACTIVATED_PREVIOUS_CYCLE_COUNT_WITH_CODE = "Deactivated previous cycle count with code: {}";
    public static final String FAILED_TO_CREATE_INVENTORY_CYCLE_AS_SKU_LIST_IS_EMPTY = "Failed to create inventory Cycle as SKU list is empty!!";
    public static final String PREVIOUS_INVENTORY_CYCLE_NOT_COMPLETED_WITH_CODE = "Previous Inventory Cycle not completed with code: {}";
    public static final String CREATING_NEW_INVENTORY_CYCLE_COUNT_FROM_TO = "Creating new Inventory Cycle count from: {} to: {}";
    public static final String CREATING_NEW_PRODUCT_INVENTORY_CYCLE_COUNT_FOR_SKU = "Creating new Inventory Cycle count Product details for day: {} with SKU: {}";
    public static final String CREATING_NEW_PRODUCT_INVENTORY_CYCLE_COUNT_HISTORY = "Creating new Inventory Cycle count History for day: {} by user: {}";
    public static final String SET_INVENTORY_CYCLE_COUNT_DAY_COMPLETED = "Setting ICC as COMPLETED for day: {}";
    public static final String SUCCESSFULLY_CREATED_NEW_INVENTORY_CYCLE_COUNT_FOR_CODE_FROM_TO = "Successfully created new Inventory Cycle count for code:{} from: {} to: {}";
    public static final String CREATED_INVENTORY_CYCLE_FOR_DATE = "Created {} Inventory Cycle for date: {}";
    public static final String CHUNKS_FOR_THIRTY_DAYS = "Created Inventory Cycle SKU list for 30 days as: {}";
    public static final String CALENDER_TIME = "Calender time generated: {}";

    public static final String SKU_LIST_ERROR = "Entered list of SKU not matching with DB!!";
    public static final String MODEL_SAVING_EXCEPTION = "Error while saving model: {}";
    public static final String EXCEPTION = "Error occurred: {}";
    public static final String ICC_SUCCESS = "Successfully in logging report for InventoryCycleCountCode: {} for day: {} with date: {}. Please Check Scan History!!";
    public static final String ICC_NO_SERIALS = "Please reconcile both of these lists!!";

    public static final String ON_CHANGE = "onChange";
    public static final String RESIZE_NONE_DISPLAY_BLOCK_SKU = "resize:none;display:inline-block";
    public static final String RESIZE_NONE_DISPLAY_BLOCK_SERIAL = "resize:none;display:block;float:right";
    public static final String CLEAR = "Clear";
    public static final String ON_CLICK = "onClick";
    public static final String DEFAULT_INVENTORY_CYCLE_COUNT_SCAN_TOOL_RENDERER = "defaultInventoryCycleCountScanToolRenderer";
    public static final String WIDTH_FOURTY_EIGHT = "48%";
    public static final String SKU_INPUT_PLACEHOLDER_ENTER_SKU_LIST = "Enter SKU list...";
    public static final String BARCODE_INPUT_PLACEHOLDER_SCAN_SERIAL_BARCODES = "Scan serial barcodes...";
    public static final String MARGIN_TOP_21_PX_FLOAT_LEFT_MARGIN_LEFT_122_PX_POSITION_FIXED_Z_INDEX_1 = "margin-top:21px;float:left;margin-left: 122px;position: fixed;z-index: 1;";
    public static final String ICC_INPUT_EMPTY_ERROR_NOTIF_MSG = "Input fields can not be empty!!";
    public static final String FETCHING_CONSIGNMENTS_THAT_WILL_RETURN_TODAY_FOR_SERIAL = "Fetching Consignments that will return today for serial: {}";
    public static final String FIRST_SCAN_LOCATION_ERROR_FAILURE = "firstScanLocationErrorFailure";
    public static final String FIRST_SCAN_LOCATION_BIN_ERROR_FAILURE = "firstScanLocationBinErrorFailure";
    public static final String UNBOX_SAN_TOOL_DC_FROM_DPC_FAILURE = "unboxInvalidDCLocationFromDPErrorFailure";
    
    public static final String MUST_TWO_BARCODE_ERROR_FAILURE_MSG_FOR_BIN = "If the first scan is a BIN, only 2 scans are allowed and the second must be a location";
    public static final String BIN_TYPE_ERROR = "binTypeError";
    public static final String TECH_ENG_BIN_TYPE_ERROR = "techEngbinTypeError";
    public static final String LAST_SCAN_DPC_OR_DC_TYPE_ERROR = "lastScanToBeDPCOrDC";
    public static final String LAST_SCAN_CPC_OR_CC_TYPE_ERROR = "lastScanToBeCPCOrCC";
    public static final String LAST_SCAN_CPC_OR_CC_TYPE_ERROR_MSG = "If the first scan is a BIN, the second must be a Tech. Eng. WorkStation OR Clean Priority Cart OR Clean Cart OR Repair Cart location";
    public static final String MISSING_BARCODE_PACKAGE_ERROR = "missingBarcodesPackage";
    public static final String MISSING_BARCODE_PACKAGE_ERROR_MSG = "No package created for the Serial {}. Unboxing is not successful";
    public static final int INT_SIX = 6;
    public static final int INT_SEVEN = 7;
    public static final int INT_EIGHT = 8;
    public static final int INT_ZERO = 0;
    public static final int INT_NINE = 9;
    public static final int INT_TEN = 10;
    public static final int INT_ELEVEN = 11;
    public static final int INT_TWELVE = 12;
    public static final int INT_THIRTEEN = 13;

    public static final String REGEX_N = "\n";
    public static final String ONLINE = "Online";
    public static final String FOR_PRODUCT_MESSAGE = " for product: ";
    
    public static final String CART_CLEAN_AND_READY_TO_SHIP = "CART_CLEAN_AND_READY_TO_SHIP";
    public static final String CART_DIRTY_PRIORITY = "CART_DIRTY_PRIORITY";
    public static final String CART_HARD_ASSIGNED_WAITING_PAYMENT_RESOLUTION = "CART_HARD_ASSIGNED_WAITING_PAYMENT_RESOLUTION";
    public static final String CLEAN_AND_READY_TO_SHIP = "CLEAN_AND_READY_TO_SHIP";
    public static final String CUSTOMER_ITEMS = "CUSTOMER_ITEMS";
    public static final String DIRTY_GEAR_CAGE = "DIRTY_GEAR_CAGE";
    public static final String DIRTY_PRIORITY = "DIRTY_PRIORITY";
    public static final String DIRTY_MOBILE_CART = "DIRTY_MOBILE_CART";
    public static final String HARD_ASSIGNED_AND_BOXED_GEAR = "HARD_ASSIGNED_AND_BOXED_GEAR";
    public static final String HARD_ASSIGNED_WAITING_PAYMENT_RESOLUTION = "HARD_ASSIGNED_WAITING_PAYMENT_RESOLUTION";
    public static final String INVENTORY_WORKSTATION = "INVENTORY_WORKSTATION";
    public static final String SCRAPPED = "SCRAPPED";

    private BlInventoryScanLoggingConstants() { }

    /**
     * javadoc
     * @return List<String> for default locations
     */
    public static List<String> getDefaultInventoryLocation() {
        List<String> defaultLocations = new ArrayList<>();
        defaultLocations.add(MA);
        defaultLocations.add(CA);
        defaultLocations.add(FEDEX);
        defaultLocations.add(UPS);
        defaultLocations.add(UNKNOWN);
        defaultLocations.add(VIP);
        defaultLocations.add(EMPLOYEE);
        defaultLocations.add(BOS);
        defaultLocations.add(MAW);
        defaultLocations.add(CAW);
        defaultLocations.add(IN_HOUSE);
        defaultLocations.add(MAW);
        defaultLocations.add(CAM);
        defaultLocations.add(BIN);
        defaultLocations.add(UNKNOWN_CARRIER);
        defaultLocations.add(ORD);
        defaultLocations.add(USPS);
        return defaultLocations;
    }

    /**
     * Method to return the Bin locations
     *
     * @return List<String> for default Bin Locations
     */
    public static List<String> getDefaultBinInventoryLocations() {
        final List<String> defaultLocations = new ArrayList<>();
        defaultLocations.add(BIN);
        return defaultLocations;
    }
}
