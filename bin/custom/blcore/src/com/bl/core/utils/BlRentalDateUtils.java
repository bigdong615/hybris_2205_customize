package com.bl.core.utils;

import com.bl.facades.product.data.RentalDateDto;

/**
 * This Utils class created to get rental duration for renal products
 * @author Manikandan
 */

public final class BlRentalDateUtils {

  public static final String DEFAULT_DAYS = "7";

  private BlRentalDateUtils()
  {
    //empty to avoid instantiating utils class
  }

  /**
   * This Method created to get rental duration for rental products
   */
  public static RentalDateDto getRentalsDuration(RentalDateDto rentalDates) {
    if (null == rentalDates)
    {
      rentalDates = new RentalDateDto();
      rentalDates.setNumberOfDays(DEFAULT_DAYS);
    }
    return rentalDates;
  }
}
