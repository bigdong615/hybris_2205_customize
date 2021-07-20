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
    public static final int SEVEN = 7;
    public static final int EIGHT = 8;
    public static final int TEN = 10;
    public static final int FORTY = 40;

    public static final String HUN_PER = "100%";

    public static final String SAMPLE1 = "sample1%";
    public static final String SAMPLE2 = "sample2%";
    public static final String SAMPLE3 = "sample3%";
    public static final String LOCATION = "MAR001 S01";

    public static final String BIN = "BIN";
    public static final String MA = "MA";
    public static final String CA = "CA";
    public static final String FEDEX = "FedEx";
    public static final String UPS = "UPS";
    public static final String VIP = "VIP";
    public static final String EMPLOYEE = "Employee";
    public static final String BOS = "BOS";
    public static final String IN_HOUSE = "In House Scrapped Items";
    public static final String UNKNOWN = "Unknown Carrier";
    public static final String MAW = "MAW";
    public static final String CAW = "CAW";
    public static final String BIN_LOCATION = "FDSHIP01";

    public static final String FAILED_BARCODE_LIST = "Failed barcode list: ";

    public static final String NOTIFICATION_HANDLER = "webSanToolNotification";
    public static final String MAX_BARCODE_LIMIT_ERROR_FAILURE = "maxBarcodeLimitErrorFailure";
    public static final String MUST_TWO_BARCODE_ERROR_FAILURE = "mustTwoBarcodeErrorFailure";
    public static final String SCAN_BATCH_ERROR_FAILURE = "scanBatchErrorFailure";
    public static final String SCAN_BARCODE_SUCCESS = "scanBarcodeSuccess";
    public static final String LAST_SCAN_INVALID_ERROR_FAILURE = "lastScanInvalidErrorFailure";
    public static final String LAST_SCAN_ERROR_FAILURE = "lastScanErrorFailure";
    public static final String MANY_LOCATION_ERROR_FAILURE = "manyLocationErrorFailure";
    public static final String WEB_SAN_TOOL_NOTIFICATION_FAILURE = "webSanToolNotificationFailure";
    public static final String WEB_SCAN_TOOL_DATA_MODEL_KEY = "webScanToolDataModelKey";

    public static final String MAX_BARCODE_LIMIT_ERROR_FAILURE_MSG = "Invalid number of barcodes in sequence, max: ";
    public static final String MUST_TWO_BARCODE_ERROR_FAILURE_MSG = "Must have two barcode scans or more.";
    public static final String SCAN_BATCH_ERROR_FAILURE_MSG = "scanBatchErrorFailure";
    public static final String SCAN_BARCODE_SUCCESS_MSG = "Successfully records scanned!!";
    public static final String LAST_SCAN_INVALID_ERROR_FAILURE_MSG = "Last scan must be valid Inventory Location";
    public static final String LAST_SCAN_ERROR_FAILURE_MSG = "Last scan must be a location";
    public static final String MANY_LOCATION_ERROR_FAILURE_MSG = "Too many location scans in sequence for an item";
    public static final String WEB_SAN_TOOL_NOTIFICATION_FAILURE_MSG = "Something went wrong!!";

    public static final String MAX_SEQUENCE_LIMIT_KEY = "maxSequenceScan";
    public static final String SCAN_STRING = "Scan data: ";

    public static final String FETCH_INVENTORY_LOC = "Fetching InventoryLocation for input locationId: ";
    public static final String FETCH_SERIAL_PROD = "Fetching SerialProducts for input barcode list: ";
    public static final String FETCH_CONFIG_VALUE = "Fetching ConfigurationValue for key: ";
  public static final String BAR_CODE_SCAN_EMPTY_BAR_CODES = "No Barcodes found to scan.";

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
