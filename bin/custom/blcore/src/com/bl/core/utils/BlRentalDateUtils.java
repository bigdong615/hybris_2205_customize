package com.bl.core.utils;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.core.Registry;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This Utils class created to get rental duration for renal products
 * @author Manikandan
 */

public final class BlRentalDateUtils {

	private static final Logger LOG = Logger.getLogger(BlRentalDateUtils.class);

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

	public static String getHolidayDates() {
		final List<Date> holidayBlackoutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
		List<String> holidayDates = new ArrayList<>();
		holidayBlackoutDates.forEach(date -> {
			holidayDates.add(BlDateTimeUtils.convertDateToStringDate(date, BlCoreConstants.DATE_PATTERN));
		});
		ObjectMapper jsonMapper = new ObjectMapper();
		String holidayJsonData = "";
		try {
			holidayJsonData = jsonMapper.writeValueAsString(holidayDates);
		} catch (Exception e) {
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Unable to write Data for Holiday Dates: {}",
					e);
		}
		return holidayJsonData;
	}
}
