package com.bl.core.datepicker;

import com.bl.facades.product.data.RentalDateDto;
import com.fasterxml.jackson.core.JsonProcessingException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


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
	 * It removes the cookie for rental date
	 *
	 * @param response the response object
	 */
	public void removeRentalDateCookie(final HttpServletResponse response);

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
	public void addRentalDatesIntoSession(final String startDate, final String endDate);
}
