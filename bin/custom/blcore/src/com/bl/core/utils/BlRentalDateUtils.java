package com.bl.core.utils;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.core.Registry;

/**
 * This Utils class created to get rental duration for renal products
 * @author Manikandan
 */

public final class BlRentalDateUtils {

  private static BlDatePickerService blDatePickerService;


  private BlRentalDateUtils()
  {
    //empty to avoid instantiating utils class
  }

  /**
   * This Method created to get rental duration for rental products
   */
  public static RentalDateDto getRentalsDuration() {
    RentalDateDto rentalDates = getBlDatePickerService().getRentalDatesFromSession();
    if (null == rentalDates)
    {
      rentalDates = new RentalDateDto();
      rentalDates.setNumberOfDays(BlCoreConstants.DEFAULT_DAYS);
    }
    return rentalDates;
  }
  
  /**
	 * Gets the formatted rental dates.
	 *
	 * @return the formatted rental dates
	 */
	public static RentalDateDto getFormattedRentalDates()
	{
		return getBlDatePickerService().getFormattedRentalDatesFromSession();
	}

  /**
   * Get Method created to get the bean for static entry by using Registry
   */
  public static BlDatePickerService getBlDatePickerService() {
    return null == blDatePickerService ? (BlDatePickerService) Registry.getApplicationContext()
        .getBean("blDatePickerService") : blDatePickerService;
  }

}
