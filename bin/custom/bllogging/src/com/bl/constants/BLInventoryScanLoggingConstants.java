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
    public static final int ONE = 1;
    public static final int TWO = 2;
    public static final int THREE = 3;
    public static final int FOUR = 4;
    public static final int SEVEN = 7;
    public static final int EIGHT = 8;
    public static final int TEN = 10;
    public static final int FORTY = 40;

    public static final String HUN_PER = "100%";

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

    public static final String MAX_SEQUENCE_LIMIT_KEY = "maxSequenceScan";
    public static final String EMPTY_STRING = "";
    public static final String SCAN_STRING = "Scan data: ";


    private BlInventoryScanLoggingConstants() { }

    public static List<String> getDefaultInventoryLocation() {
        List<String> defaultLocations = new ArrayList<>();
        defaultLocations.add(BIN);
        defaultLocations.add(MA);
        defaultLocations.add(CA);
        defaultLocations.add(FEDEX);
        defaultLocations.add(UPS);
        defaultLocations.add(UNKNOWN);
        defaultLocations.add(VIP);
        defaultLocations.add(EMPLOYEE);
        defaultLocations.add(BOS);
        defaultLocations.add(IN_HOUSE);
        return defaultLocations;
    }
}
