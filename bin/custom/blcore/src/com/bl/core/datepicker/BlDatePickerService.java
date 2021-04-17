package com.bl.core.datepicker;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.bl.core.data.BlDatePicker;
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
	 * It gets the cookie of rental date
	 *
	 * @param request the request object
	 * @return BlDatePicker
	 * @throws JsonProcessingException the exception while processing json
	 */
	public BlDatePicker getCookieForDatePicker(final HttpServletRequest request)
			throws JsonProcessingException;

	/**
	 * It sets the rental date into session or remove it from the session based on condition
	 *
	 * @param blDatePicker
	 */
	public void setOrRemoveDatePickerInSession(final BlDatePicker blDatePicker);

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
	 * It removes the date from session
	 *
	 * @param selectedDateMap the map for the selected date
	 */
	public void removeDatePickerFromSession(final String selectedDateMap);

	/**
	 * It removes the cookie for selected date
	 *
	 * @param response the response object
	 * @param cookieName the name of the cookie
	 */
	public void removeCookie(final HttpServletResponse response, final String cookieName);

	/**
	 * It gets the date from session
	 *
	 * @return RentalDateDto
	 */
	public RentalDateDto getDateFromSession();
}
