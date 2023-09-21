package com.bl.core.datepicker;

import java.util.Date;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.facades.product.data.RentalDateDto;
import com.fasterxml.jackson.core.JsonProcessingException;


/**
 * This class is used to set the rental date to cookie and to set into session and access it from session
 *
 * @author Moumita
 *
 */
public interface BlDatePickerService
{
	/**
	 * It gets the rental date from cookie
	 *
	 * @param request the request object
	 * @return RentalDateDto
	 * @throws JsonProcessingException the exception while processing json
	 */
	public RentalDateDto getRentalDatesFromCookie(final HttpServletRequest request)
			throws JsonProcessingException;

	/**
	 * It checks if the selected date is same as the date set into cookie
	 *
	 * @param request the request object
	 * @param startDate the selected start date
	 * @param endDate the selected end date
	 * @return boolean
	 * @throws JsonProcessingException the exception while processing json
	 */
	public boolean checkIfSelectedDateIsSame(final HttpServletRequest request, final String startDate, final String endDate)
			throws JsonProcessingException;

	/**
	 * It removes the date picker date from session
	 *
	 */
	public void removeRentalDatesFromSession();

	/**
	 * It gets the date from session
	 *
	 * @return RentalDateDto
	 */
	public RentalDateDto getRentalDatesFromSession();

	/**
	 * It sets the date picker date into session
	 *
	 * @param startDate the rental start date
	 * @param endDate the rental end date
	 */
	public void addRentalDatesIntoSession(final String startDate, final String endDate, final String selectedFromDateMMDDYYYY,
			final String daysUntilRental);

	/**
	 * Gets the list of black out dates with time as 00:00:00 am from current base store.
	 *
	 * @param blackoutDateType the blackout date type
	 * @return the all blackout dates for given type
	 */
	public List<Date> getAllBlackoutDatesForGivenType(final BlackoutDateTypeEnum blackoutDateType);


	/**
	 * It sets the date picker date into session
	 *
	 * @param selectedDuration the selected Duration
	 */
  void addSelectedRentalDurationIntoSession(final String selectedDuration);
}
