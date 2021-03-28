package com.bl.constants;

import com.bl.logging.LogError;
import com.bl.logging.impl.LogErrorCodeEnum;

import java.util.ArrayList;
import java.util.List;

/**
 * @{JavaDoc}
 * @auther Namrata Lohar
 * Scanning specific response messages
 */
public class BLInventoryScanLoggingConstants  {

    public static List<String> defaultLocations;

    public static List<String> getDefaultLocations() {
        return defaultLocations;
    }

    public static void setDefaultLocations(List<String> defaultLocations) {
        defaultLocations = defaultLocations;
    }

    public static final int ZERO = 0;
    public static final int ONE = 1;
    public static final int TWO = 2;
    public static final int THREE = 3;
    public static final int FOUR = 4;
    public static final int FIVE = 5;
    public static final int SIX = 6;
    public static final int SEVEN = 7;
    public static final int EIGHT = 8;

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

    public BLInventoryScanLoggingConstants() {
        defaultLocations = new ArrayList<>();
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
        setDefaultLocations(defaultLocations);
    }

    /* BL Inventory Scan Error Messages */
    public static final String MUST_2_ERROR = "Must have two barcode scans or more";
    public static final String LAST_SCAN_ERROR = "Last scan must be a location";
    public static final String LAST_SCAN_INVALID_ERROR = "Last scan must be valid Inventory Location";
    public static final String MANY_LOC_ERROR = "Too many location scans in sequence for an item";
    public static final String MAX_BARCODE_LIMIT_ERROR = "Invalid number of barcodes in sequence, max: ";
    public static final String SCAN_ERROR_FOR_BATCH = "Failed to update inventory location for: ";
    public static final String SCAN_SUCCESS = "Successfully record scans: ";
    public static final String SCAN_DATA_ERROR = "Scan data: ";

}
