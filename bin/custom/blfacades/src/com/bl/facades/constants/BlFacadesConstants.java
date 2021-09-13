/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.facades.constants;

/**
 * Global class for all BlFacades constants.
 */
public class BlFacadesConstants extends GeneratedBlFacadesConstants {

  public static final String EXTENSIONNAME = "blfacades";
  public static final String CUSTOMER = "customer";
  public static final String RENTAL_PAGE_IDENTIFIER = "rent";
  public static final String PRODUCT_URL = "/product/";
  public static final String USED_PAGE_IDENTIFIER = "buy";
  public static final String TIME_FORMAT_STRING = "HH:mm:ss";
  public static final String DEFAULT_REDIRECT_URL = "/";
  public static final double DEFAULT_CONDITIONAL_RATING = 5;

  public static final String DATE_FORMAT = "dd-MM-yyyy";
  public static final String START_DATE = "startDate";
  public static final String END_DATE = "endDate";
  public static final String SERIAL_CODE_MISSING = "serialCodeNotPresent";
  public static final String SUCCESS = "success";
  public static final String RESULT_SUCCESS = "SUCCESS";

  public static final String USED_GEAR_CART = "isUsedGearCart";
  public static final String RENTAL_CART = "isRentalCart";
  public static final String GIFT_CART = "isGiftCart";
  public static final String NEW_GEAR_CART="isNewGearCart";
  public static final String RENTAL_OR_USED_GEAR_PRODUCT_ALLOWED = "allowAddToCart";

  public static final String ON_SALE = "onSale";
  public static final String ON_SALE_TAG_VALUE = "ON SALE";

  public static final String RENTAL_DATE_FORMAT = "EEEE, MMM d";
  public static final String FORMATTED_RENTAL_DATE= "MMM d";

  public static final String COMMA_SEPERATER =",";
  public static final String REMOVE_ENTRIES = "removedEntries";

  public static final String ROUND_TRIP = "Round Trip";
  public static final String START_DATE_PATTERN = "MM/dd/yyyy";
  public static final String END_DATE_PATTERN = "EE MMM dd yyyy";
  public static final String ORDER_FORMAT_PATTERN = "MMM d , YYYY hh:mm a";
  public static final String EXTEND_ORDER_FORMAT_PATTERN = "MMM d , YYYY";
  public static final String EXTEND_ORDER_FORMAT_PATTERN_FOR_JS = "yyyy ,MM, dd";
  public static final String DAYS = "days";
  public static final String EMPTY = "";

  public static final String DAMAGE_WAIVER_FIELD = "damageWaiver";
  public static final String TOTAL_TAX_FIELD = "totalTax";
  public static final String TOTAL_PRICE_FIELD = "totalPrice";
  public static final String DISCOUNT_FIELD = "discount";
  public static final String OPTION_FIELD = "Options";
  public static final String SUB_TOTAL_FIELD = "subTotal";
  public static final String PRODUCT_SEPERATOR = "x";
  public static final String BLANK = " ";
  public static final String URL_SEPERATOR = "/";

  public static final String DAY = "day";

  public static final String INCOMPLETE = "Incomplete";
  public static final String RECEIVED = "Received";
  public static final String SHIPPED = "Shipped";
  public static final String RETURNED = "Returned";
  public static final String COMPLETED = "Completed";
  public static final String CANCELED = "Canceled";
  public static final String LATE = "Late";
  public static final String SOLD = "Sold";
  
  public static final int ONE = 1;
  public static final int TWO = 2;
  

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

  public static final String ORDER_ITEMS_ROOT_ELEMENT = "orderitemsinfo";
  public static final String ORDER_ITEM_ROOT_ELEMENT = "orderitem";
  public static final String ORDER_ITEM_PRODUCT_CODE = "productcode";
  public static final String ORDER_ITEM_PRODUCT_TITLE = "producttitle";
  public static final String ORDER_ITEM_PRODUCT_PHOTO = "productphoto";
  public static final String ORDER_ITEM_RENTAL_PRICE = "rentalprice";
  public static final String ORDER_ITEM_DAMAGE_WAIVER_PRICE = "damagewaiverprice";
  public static final String ORDER_ITEM_DAMAGE_WAIVER_TEXT = "damagewaivertext";
  public static final String ORDER_ITEM_TOTAL_PRICE = "totalprice";

  private BlFacadesConstants() {
    //empty
  }
}
