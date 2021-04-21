package com.bl.core.datepicker.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.fasterxml.jackson.core.JsonProcessingException;
import de.hybris.platform.servicelayer.session.SessionService;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.web.util.WebUtils;


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
	public RentalDateDto getRentalDatesFromCookie(final HttpServletRequest request)
			throws JsonProcessingException
	{
		final Cookie selectedDateCookie = WebUtils.getCookie(request, BlCoreConstants.COOKIE_NAME_FOR_DATE);
		if (null != selectedDateCookie && StringUtils.isNotEmpty(selectedDateCookie.getValue()))
		{
			final String date = selectedDateCookie.getValue();
			final String[] lSelectedDates = date.split(BlCoreConstants.SEPARATOR);
			if (lSelectedDates.length == BlCoreConstants.PAIR_OF_DATES)
			{
				final RentalDateDto rentalDateDto = new RentalDateDto();
				rentalDateDto.setSelectedFromDate(lSelectedDates[0]);
				rentalDateDto.setSelectedToDate(lSelectedDates[1]);
				return rentalDateDto;
			}
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removeRentalDatesFromSession()
	{
		getSessionService().removeAttribute(BlCoreConstants.SELECTED_DATE_MAP);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "date {} removed from session", BlCoreConstants.SELECTED_DATE_MAP);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void addRentalDatesIntoSession(final String startDate, final String endDate)
	{
		final Map<String, String> datepickerDates = new HashMap<>();
		datepickerDates.put(BlCoreConstants.START_DATE, startDate);
		datepickerDates.put(BlCoreConstants.END_DATE, endDate);
		getSessionService().setAttribute(BlCoreConstants.SELECTED_DATE_MAP, datepickerDates);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "date from {} to {} set into session", startDate, endDate);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean checkIfSelectedDateIsSame(final HttpServletRequest request, final String startDate, final String endDate)
			throws JsonProcessingException
	{
		final Map<String, String> rentalDate = getSessionService().getAttribute(BlCoreConstants.SELECTED_DATE_MAP);
		if (null != rentalDate)
		{
			final String startDayFromCookie = rentalDate.get(BlCoreConstants.START_DATE);
			final String endDayFromCookie = rentalDate.get(BlCoreConstants.END_DATE);
			if (startDate.equals(startDayFromCookie) && endDate.equals(endDayFromCookie))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Selected dates from {} to {} are same as present in session",
						startDayFromCookie, endDayFromCookie);
				return true;
			}
		}
		return false;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public RentalDateDto getRentalDatesFromSession()
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
								BlDateTimeUtils.convertStringDateToLocalDate(endDate, BlCoreConstants.DATE_FORMAT).plusDays(1))));
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
