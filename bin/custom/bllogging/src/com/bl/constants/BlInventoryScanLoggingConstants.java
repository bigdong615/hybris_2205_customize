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

    public static final String FAILED_BARCODE_LIST = "Failed barcode list: ";

    public static final String NOTIFICATION_HANDLER = "webSanToolNotification";
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
    public static final String WEB_SCAN_TOOL_DATA_MODEL_KEY = "webScanToolDataModelKey";
    public static final String VALID_BIN_LOCATION_ERROR_FAILURE = "validBINLocationErrorFailure";
    public static final String VALID_PARENT_LOCATION_ERROR_FAILURE = "validParentLocationErrorFailure";

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
    public static final String LAST_SCAN_ERROR_FAILURE_MSG = "Last scan must be a location";
    public static final String MANY_LOCATION_ERROR_FAILURE_MSG = "Too many location scans in sequence for an item";
    public static final String WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG = "Something went wrong!!";
 	public static final String SHIPPING_MAX_SCAN_ERROR_MSG = "Invalid number of barcodes in sequence";
 	public static final String INVALID_BARCODE_SCANNED_MSG = "Scanned barcodes are not valid {}";
 	public static final String SERIAL_MISMATCH_SCANNED_MSG = "Scanned serial does not match to serials on package";
 	public static final String INVALID_SCAN_MSG = "Invalid Scan";

 	public static final String SHIPPING_MANUAL_REVIEW_FAILURE_MSG = "Can not scan order which is in SHIPPIN MANUAL REVIEW state";
	public static final String SHIPPING_CANCEL_ORDER_FAILURE_MSG = "Can not scan cancelled order"; 
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
    public static final String SCANNING_SUCCESS_MSG = "Scanning completed successfully";
 	public static final String LAST_SCAN_INVALID_ERROR_FAILURE_KEY = "blbackoffice.order.scan.last.location.error";
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
	public static final String PACKAGE_CANCEL_ORDER_FAILURE = "blbackoffice.order.cancel.order.error.messge";
	public static final String LAST_LOCATION_VALID_TRACKING_FAILURE = "blbackoffice.order.last.location.tracking.id.error.messge";
	
 	public static final String SHIPPING_NO_ITEM_SCAN_KEY = "blbackoffice.shipping.scan.tool.noitem.scan.error";
 	public static final String SHIPPING_INVALID_SCAN_ERROR = "blbackoffice.shipping.scan.invalid.scan.error";
 	public static final String SHIPPING_TWO_BARCODE_SCAN_ERROR_KEY = "blbackoffice.shipping.scan.item.error";
 	public static final String SHIPPING_LAST_LOCATION_ERROR_KEY = "blbackoffice.shipping.scan.last.location.error";
 	public static final String SHIPPING_LAST_INVALID_LOCATION_ERROR = "blbackoffice.shipping.scan.last.invalid.location.error";
 	public static final String SHIPPING_MANY_LOCATION_ERROR = "blbackoffice.shipping.scan.many.location.error";
 	public static final String SHIPPING_INVALID_LOCATION_ERROR = "blbackoffice.shipping.scan.invalid.location.error";
 	public static final String SHIPPING_MANUAL_REVIEW_FAILURE = "blbackoffice.shipping.manual.review.error.messge";
	public static final String SHIPPING_CANCEL_ORDER_FAILURE = "blbackoffice.shipping.cancel.order.error.messge";
 	public static final String SHIPPING_SERIAL_MISSING_ON_CONSIGNMENT_KEY = "blbackoffice.shipping.serial.missing.error.messge";
 	public static final String SHIPPING_SERIAL_MISSING_ON_SCAN_KEY = "blbackoffice.shipping.scan.missing.error.messge";


 	public static final String DOUBLE_HYPHEN = "--";
 	public static final String NEW_LINE = "\n";
 	public static final String ON_CHANGE_EVENT = "onChange";
 	public static final String ON_CLICK_EVENT = "onClick";
 	public static final String CANCEL_EVENT = "cancel";
 	public static final String VERIFY_SCAN_EVENT = "verifyScan";
 	public static final String SOCKET_ID = "inputObject";
 	public static final String SCANNING_AREA = "scanningArea";
 	public static final String SCAN_TO_BIN = "scanToBin";
 	public static final String SCAN_TO_TRACKING_ID = "scanToTrackingLabel";
 	public static final String SCAN_TO_UPS_OUTBOUND = "scanToOutBoundCart";
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
    
    public static final String UNBOXING_WORKSTATION = "TECH_ENGINEERING_WORKSTATION";
    public static final String UNBOXING_WORKSTATION_LOCATION = "VIP_WORKSTATION_TECH_ENG";
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
        defaultLocations.add(CAW);
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
