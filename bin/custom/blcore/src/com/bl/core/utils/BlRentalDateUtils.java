package com.bl.core.utils;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.core.Registry;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;

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
    if (Objects.nonNull(rentalDates) && StringUtils.isNotBlank(rentalDates.getSelectedFromDate())
				&& StringUtils.isNotBlank(rentalDates.getSelectedToDate()))
		{
   	 rentalDates.setSelectedFromDate(getFormattedDate(rentalDates.getSelectedFromDate()));
   	 rentalDates.setSelectedToDate(getFormattedDate(rentalDates.getSelectedToDate()));
   	 rentalDates.setNumberOfDays(rentalDates.getNumberOfDays());
			return rentalDates;
		}
    
    if (null == rentalDates)
    {
      rentalDates = new RentalDateDto();
      rentalDates.setNumberOfDays(BlCoreConstants.DEFAULT_DAYS);
    }
    return rentalDates;
  }
  
  private static String getFormattedDate(final String rentalDatesFromSession)
	{
		return BlDateTimeUtils.convertDateToStringDate(BlDateTimeUtils.getDate(rentalDatesFromSession, BlCoreConstants.DATE_FORMAT),
				BlCoreConstants.RENTAL_DATE_FORMAT);
	}

  /**
   * Get Method created to get the bean for static entry by using Registry
   */
  public static BlDatePickerService getBlDatePickerService() {
    return null == blDatePickerService ? (BlDatePickerService) Registry.getApplicationContext()
        .getBean("blDatePickerService") : blDatePickerService;
  }

}
