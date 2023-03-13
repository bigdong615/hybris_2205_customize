package com.bl.core.datepicker.impl;

import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.store.services.BaseStoreService;

import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.web.util.WebUtils;

import com.bl.core.blackout.date.dao.BlBlackoutDatesDao;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlBlackoutDateModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.google.common.collect.Lists;


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
	private BaseStoreService baseStoreService;
	private BlBlackoutDatesDao blBlackoutDatesDao;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public RentalDateDto getRentalDatesFromCookie(final HttpServletRequest request)
			throws JsonProcessingException
	{
		final Cookie selectedDateCookie = WebUtils.getCookie(request, BlCoreConstants.COOKIE_NAME_FOR_DATE);
		final Cookie durationDayCookie = WebUtils.getCookie(request, BlCoreConstants.COOKIE_NAME_FOR_DURATION);
		if (null != selectedDateCookie && StringUtils.isNotEmpty(selectedDateCookie.getValue()) && null != durationDayCookie && StringUtils.isNotEmpty(durationDayCookie.getValue()))
		{
			final String date = selectedDateCookie.getValue();
			final String[] lSelectedDates = date.split(BlCoreConstants.SEPARATOR);
			final String rentalDuration = durationDayCookie.getValue();
			if (lSelectedDates.length == BlCoreConstants.PAIR_OF_DATES)
			{
				final RentalDateDto rentalDateDto = new RentalDateDto();
				rentalDateDto.setSelectedFromDate(lSelectedDates[0]);
				rentalDateDto.setSelectedToDate(lSelectedDates[1]);
				// duration days fixed picked from date picker
				rentalDateDto.setSelectedDays(rentalDuration);
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
		getSessionService().removeAttribute(BlCoreConstants.SELECTED_DURATION_MAP);
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
		final Map<String, String> selectedDuration = getSessionService().getAttribute(BlCoreConstants.SELECTED_DURATION_MAP);
		if (null != rentalDate)
		{
			final RentalDateDto date = new RentalDateDto();
			final String startDate = rentalDate.get(BlCoreConstants.START_DATE);
			final String endDate = rentalDate.get(BlCoreConstants.END_DATE);
			final String selectedDurationDays = selectedDuration.get(BlCoreConstants.SELECTED_DURATION);
			if (null != startDate && null != endDate)
			{
				date.setSelectedFromDate(startDate);
				date.setSelectedToDate(endDate);
				date.setNumberOfDays(String.valueOf(
						ChronoUnit.DAYS.between(BlDateTimeUtils.convertStringDateToLocalDate(startDate, BlCoreConstants.DATE_FORMAT),
								BlDateTimeUtils.convertStringDateToLocalDate(endDate, BlCoreConstants.DATE_FORMAT))));
				if(org.apache.commons.lang.StringUtils.isNotBlank(selectedDurationDays)) {
					date.setSelectedDays(selectedDurationDays);
				}
				return date;
			}
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<Date> getAllBlackoutDatesForGivenType(final BlackoutDateTypeEnum blackoutDateType)
	{
		try
		{
			final List<BlBlackoutDateModel> allBlackoutDatesForGivenType = getBlBlackoutDatesDao()
					.getAllBlackoutDatesForGivenType(blackoutDateType);
			final List<Date> blackoutDates = allBlackoutDatesForGivenType.stream().map(
					BlBlackoutDateModel::getBlackoutDate)
					.collect(Collectors.toList()).stream().map(date -> BlDateTimeUtils.getFormattedStartDay(date).getTime())
					.collect(Collectors.toList());
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Blackout dates found for type {} is {}", blackoutDateType.toString(),
					blackoutDates);
			return blackoutDates;
		}
		catch (final Exception exception)
		{
			BlLogger.logMessage(LOG, Level.ERROR, StringUtils.EMPTY, "Error while getting list of black out dates", exception);
		}

		return Lists.newArrayList();
	}

	/**
	 * It sets the date picker date into session
	 *
	 * @param selectedDuration the selected Duration
	 */
	@Override
	public void addSelectedRentalDurationIntoSession(final String selectedDuration) {
		final Map<String, String> selectedDurationMap = new HashMap<>();
		selectedDurationMap.put(BlCoreConstants.SELECTED_DURATION, selectedDuration);
		getSessionService().setAttribute(BlCoreConstants.SELECTED_DURATION_MAP, selectedDurationMap);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Selected duration {} set into session", selectedDuration);
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

	/**
	 * @return the baseStoreService
	 */
	public BaseStoreService getBaseStoreService()
	{
		return baseStoreService;
	}

	/**
	 * @param baseStoreService the baseStoreService to set
	 */
	public void setBaseStoreService(final BaseStoreService baseStoreService)
	{
		this.baseStoreService = baseStoreService;
	}

	/**
	 * @return the blBlackoutDatesDao
	 */
	public BlBlackoutDatesDao getBlBlackoutDatesDao()
	{
		return blBlackoutDatesDao;
	}

	/**
	 * @param blBlackoutDatesDao the blBlackoutDatesDao to set
	 */
	public void setBlBlackoutDatesDao(final BlBlackoutDatesDao blBlackoutDatesDao)
	{
		this.blBlackoutDatesDao = blBlackoutDatesDao;
	}

}
