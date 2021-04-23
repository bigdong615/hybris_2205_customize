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
	public static RentalDateDto getRentalsDuration()
	{
		final RentalDateDto rentalDates = getBlDatePickerService().getRentalDatesFromSession();
		return formatRentalDates(Objects.nonNull(rentalDates) ? rentalDates : new RentalDateDto());
	}

	/**
	 * Format rental dates in MMM dd format.
	 *
	 * @param rentalDates
	 *           the rental dates
	 * @return the rental date dto
	 */
	private static RentalDateDto formatRentalDates(final RentalDateDto rentalDates)
	{
		rentalDates.setSelectedFromDate(
				StringUtils.isNotBlank(rentalDates.getSelectedFromDate()) ? getFormattedDate(rentalDates.getSelectedFromDate())
						: null);
		rentalDates.setSelectedToDate(
				StringUtils.isNotBlank(rentalDates.getSelectedToDate()) ? getFormattedDate(rentalDates.getSelectedToDate()) : null);
		rentalDates.setNumberOfDays(
				StringUtils.isNotBlank(rentalDates.getNumberOfDays()) ? rentalDates.getNumberOfDays() : BlCoreConstants.DEFAULT_DAYS);
		return rentalDates;
	}

	/**
	 * Gets the formatted date in string type.
	 *
	 * @param rentalDatesFromSession
	 *           the rental dates from session
	 * @return the formatted date
	 */
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
