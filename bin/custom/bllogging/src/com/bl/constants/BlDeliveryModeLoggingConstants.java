package com.bl.constants;

import com.bl.core.enums.AddressTypeEnum;

import java.util.ArrayList;
import java.util.Collection;

/**
 * {javadoc}
 *
 * @auther Namrata Lohar
 * Shipping specific response messages
 */

public class BlDeliveryModeLoggingConstants {

    public static final String DATE_TIME= "HH:mm";
    public static final String ZONE_PST= "PST";
    public static final String ZONE_EST= "EST";

    public static final String AM= "AM";
    public static final String AM_ERROR = "AM-ERROR";
    public static final String PIN_ERROR = "PIN-ERROR";

    public static final String RENTAL_DATE_PATTERN= "MM-dd-yyyy";
    public static final String RENTAL_FE_DATE_PATTERN= "dd-MM-yyyy";
    public static final String LOCAL_DATE_PATTERN= "yyyy-MM-dd";

    public static final String TOTAL_WEIGHT= "TotalWeight";
    public static final String DIMENSIONAL_WEIGHT= "DimensionalWeight";
    public static final String STORE= "bl";
    public static final int DIMENSIONAL_FACTOR= 194;

    public static final String PST_CUT_OFF_TIME_CONST = " and {pickZone.cutOffTime} > ?pstCutOffTime";
    public static final String PST_CUT_OFF_TIME = "pstCutOffTime";

    public static final String SF= "SF";
    public static final String NYC= "NYC";

    public static final String DELIVERY_TYPE_STANDARD= "STANDARD";
    public static final String DELIVERY_TYPE_OVERNIGHT= "OVERNIGHT";

    public static final String SHIP_HOME_HOTEL_BUSINESS = "SHIP_HOME_HOTEL_BUSINESS";
    public static final String BL_PARTNER_PICKUP = "BL_PARTNER_PICKUP";
    public static final String SHIP_HOLD_UPS_OFFICE = "SHIP_HOLD_UPS_OFFICE";
    public static final String SAME_DAY_DELIVERY = "SAME_DAY_DELIVERY";
    public static final String NEXT_DAY_RUSH_DELIVERY = "NEXT_DAY_RUSH_DELIVERY";

    public static final String SHIP_HOME_HOTEL_BUSINESS_MSG = "Ship to home, hotel business";
    public static final String BL_PARTNER_PICKUP_MSG = "BL-Partner pickup";
    public static final String SHIP_HOLD_UPS_OFFICE_MSG = "Ship to UPS store";
    public static final String SAME_DAY_DELIVERY_MSG = "Rush Same Day Delivery!!";
    public static final String NEXT_DAY_RUSH_DELIVERY_MSG = "Rush Next Day Delivery!!";
    public static final String DEFAULT_DELIVERY_MSG = "No Predefined shipping group found!!";

    public static final String FETCH_SHIPPING_GROUP = "Fetching all shipping groups!!";
    public static final String FETCH_PARTNER_PICKUP_ZONE_GROUP = "Fetching all partner pickup zones!!";
    public static final String FETCH_SHIP_TO_HOME_ZONE_DELIVERY_MODE = "Fetching delivery modes for ship to home shipping groups for: ";
    public static final String FETCH_SHIP_TO_HOME_ZONE_DELIVERY_MODE_AM = "Fetching delivery modes for ship to home shipping groups without AM for: ";
    public static final String FETCH_PARTNER_PICKUP_ZONE_DELIVERY_MODE = "Fetching delivery modes for Partner-pickup shipping groups for: ";
    public static final String FETCH_PARTNER_PICKUP_UPS_STORE_ZONE_DELIVERY_MODE = "Fetching delivery modes for Partner-pickup shipping groups for UPS Store ";
    public static final String FETCH_PARTNER_PICKUP_UPS_STORE_ZONE_DELIVERY_MODE_AM = "Fetching delivery modes for Partner-pickup shipping groups for UPS Store without AM ";
    public static final String FETCH_RUSH_ZONE_DELIVERY_MODE = "Fetching delivery modes for rush delivery shipping groups for :";
    public static final String FETCH_SHIPPING_COST = "Fetching shipping cost for : ";
    public static final String BASE_STORE_DIMENSIONAL_FACTOR = "Fetching dimensional factor from base store : ";


    public static final String PAY_BY_CUSTOMER = "payByCustomer";

    private BlDeliveryModeLoggingConstants() {}
}
