package com.bl.core.datepicker.impl;

import de.hybris.platform.servicelayer.session.SessionService;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.web.util.WebUtils;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.data.BlDatePicker;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.stock.impl.DefaultBlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.fasterxml.jackson.core.JsonProcessingException;


/**
 * This class is used to set the rental date to cookie and to set into session and access it from session
 *
 * @author Moumita
 *
 */
public class DefaultBlDatePickerService implements BlDatePickerService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlDatePickerService.class);
	private SessionService sessionService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public BlDatePicker getCookieForDatePicker(final HttpServletRequest request)
			throws JsonProcessingException
	{
		final Cookie selectedDateCookie = WebUtils.getCookie(request, BlCoreConstants.COOKIE_NAME_FOR_DATE);
		if (null != selectedDateCookie && StringUtils.isNotEmpty(selectedDateCookie.getValue()))
		{
			final String date = selectedDateCookie.getValue();
			final String[] lSelectedDates = date.split(BlCoreConstants.SEPARATOR);
			if (lSelectedDates.length == BlCoreConstants.PAIR_OF_DATES)
			{
				final BlDatePicker blDatePicker = new BlDatePicker();
				blDatePicker.setRentalStartDate(lSelectedDates[0]);
				blDatePicker.setRentalEndDate(lSelectedDates[1]);
				return blDatePicker;
			}
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removeDatePickerFromSession(final String selectedDateMap)
	{
		getSessionService().removeAttribute(selectedDateMap);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "date {} removed from session", selectedDateMap);
	}

	/**
	 * It sets the date into session
	 *
	 * @param selectedFromDate
	 * @param selectedToDate
	 * @param startDate
	 * @param endDate
	 */
	private void addDatePickerIntoSession(final String selectedFromDate, final String selectedToDate, final String startDate,
			final String endDate)
	{
		final Map<String, String> datepickerDates = new HashMap<>();
		datepickerDates.put(selectedFromDate, startDate);
		datepickerDates.put(selectedToDate, endDate);
		getSessionService().setAttribute(BlCoreConstants.SELECTED_DATE_MAP, datepickerDates);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "date {} and {} set into session", startDate, endDate);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setOrRemoveDatePickerInSession(final BlDatePicker blDatePicker)
	{
		final LocalDate currentDate = LocalDate.now();
		final String selectedFromDate = blDatePicker.getRentalStartDate();
		final String selectedToDate = blDatePicker.getRentalEndDate();
		final LocalDate startDate = BlDateTimeUtils.convertStringDateToLocalDate(selectedFromDate,
				BlCoreConstants.DATE_FORMAT);
		if (startDate.isBefore(currentDate))
		{
			removeDatePickerFromSession(BlCoreConstants.SELECTED_DATE_MAP);
		}
		else
		{
			addDatePickerIntoSession(BlCoreConstants.START_DATE, BlCoreConstants.END_DATE, selectedFromDate,
					selectedToDate);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean checkIfSelectedDateIsSame(final HttpServletRequest request, final String startDate, final String endDate)
			throws JsonProcessingException
	{
		final BlDatePicker blDatePicker = getCookieForDatePicker(request);
		if (null != blDatePicker)
		{
			final String startDayFromCookie = blDatePicker.getRentalStartDate();
			final String endDayFromCookie = blDatePicker.getRentalEndDate();
			if (startDate.equals(startDayFromCookie) && endDate.equals(endDayFromCookie))
			{
				return true;
			}
		}
		return false;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removeCookie(final HttpServletResponse response, final String cookieName)
	{
		final Cookie cookie1 = new Cookie(cookieName, "");

		cookie1.setMaxAge(0);

		response.addCookie(cookie1);

		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Cookie removed for {} ", cookieName);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public RentalDateDto getDateFromSession()
	{
		final Map<String, String> rentalDate = getSessionService().getAttribute(BlCoreConstants.SELECTED_DATE_MAP);
		if (null != rentalDate)
		{
			final RentalDateDto date = new RentalDateDto();
			final String startDate = rentalDate.get(BlCoreConstants.START_DATE);
			final String endDate = rentalDate.get(BlCoreConstants.END_DATE);
			if (null != startDate && null != endDate)
			{
				date.setSelectedFromDate(startDate);
				date.setSelectedToDate(endDate);
				date.setNumberOfDays(String.valueOf(
						ChronoUnit.DAYS.between(BlDateTimeUtils.convertStringDateToLocalDate(startDate, BlCoreConstants.DATE_FORMAT),
								BlDateTimeUtils.convertStringDateToLocalDate(endDate, BlCoreConstants.DATE_FORMAT))));
				return date;
			}
		}
		return null;
	}

	/**
	 * @return the sessionService
	 */
	public SessionService getSessionService()
	{
		return sessionService;
	}

	/**
	 * @param sessionService
	 *           the sessionService to set
	 */
	public void setSessionService(final SessionService sessionService)
	{
		this.sessionService = sessionService;
	}

}
