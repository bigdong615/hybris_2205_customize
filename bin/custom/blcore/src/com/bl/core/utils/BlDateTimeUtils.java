package com.bl.core.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.logging.BlLogger;


/**
 * This class is used to convert the date format
 *
 * @author Moumita
 */
public final class BlDateTimeUtils
{
	private static final Logger LOG = Logger.getLogger(BlDateTimeUtils.class);
	private static final String UNABLE_TO_PARSE_DATE = "Unable to parse String : {} to Date.";
	private static Map<String, DateTimeFormatter> formatterCache = new HashMap<>();

	private BlDateTimeUtils()
	{
		//empty to avoid instantiating utils class
	}

	/**
	 * Method takes a {@link String} dateTime and pattern and returns a {@link Date} object. The method will return null
	 * if parsing fails.
	 *
	 * @param date
	 *           the {@link String} dateTime to be converted
	 * @param pattern
	 *           the pattern
	 *
	 * @return {@link Date}
	 */
	public static Date convertStringDateToDate(final String date, final String pattern)
	{
		try
		{
			return Date.from(LocalDate.parse(date, getFormatter(pattern)).atStartOfDay(ZoneId.systemDefault()).toInstant());
		}
		catch (final DateTimeParseException e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE,
					date);
		}

		return null;
	}

	/**
	 * Method takes a {@link String} dateTime and pattern and returns a {@link Date} object. The method will return null
	 * if parsing fails. The default zone ID considered will be UTC
	 *
	 * @param date
	 *           the {@link String} dateTime to be converted
	 * @param pattern
	 *           the pattern
	 * @param zoneId
	 *           the zone id
	 *
	 * @return {@link Date}
	 */
	public static Date convertStringDateToDate(final String date, final String pattern, final String zoneId)
	{
		try
		{
			return Date.from(LocalDate.parse(date, getFormatter(pattern))
					.atStartOfDay(ZoneId.of(StringUtils.isNotEmpty(zoneId) ? zoneId : BlCoreConstants.DEFAULT_ZONE_ID)).toInstant());
		}
		catch (final DateTimeParseException e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE,
					date);
		}

		return null;
	}

	/**
	 * Method takes a {@link Date} and return a string representation based on the pattern.
	 *
	 * @param date
	 *           the {@link Date} to be converted
	 * @param pattern
	 *           the pattern
	 *
	 * @return {@link String}
	 */
	public static String convertDateToStringDate(final Date date, final String pattern)
	{
		return LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault()).format(getFormatter(pattern));
	}

	/**
	 * Method to get the ZoneDateTime with the UTC time-zone given a Date and its ZoneId.
	 *
	 * @param date
	 *           the date to be converted in the UTC ZonedDateTime
	 * @param zoneId
	 *           the zoneId of the date
	 *
	 * @return the ZonedDateTime with the UTC time-zone.
	 */
	public static ZonedDateTime getUtcZonedDateTime(final Date date, final ZoneId zoneId)
	{
		return getZonedDateTime(date, zoneId).withZoneSameInstant(ZoneOffset.UTC);
	}

	/**
	 * Method to get the ZonedDateTime object from a date and its zoneId.
	 *
	 * @param date
	 *           the date to be converted in a ZonedDateTime
	 * @param zoneId
	 *           the zoneId of the date
	 *
	 * @return the ZonedDateTime calculated from the date and its zoneId.
	 */
	public static ZonedDateTime getZonedDateTime(final Date date, final ZoneId zoneId)
	{
		final LocalDateTime localDateTime = LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
		return ZonedDateTime.of(localDateTime, zoneId);
	}

	/**
	 * This method returns a cached DateTimeFormatter from formatters cache based on the given pattern if exists, if not
	 * a new DateTimeFormatter will be created, added to the cache and then returned.
	 *
	 * @param pattern
	 *           the date pattern
	 *
	 * @return a DateTimeFormatter
	 */
	public static DateTimeFormatter getFormatter(final String pattern)
	{
		if (!formatterCache.containsKey(pattern))
		{
			formatterCache.put(pattern, DateTimeFormatter.ofPattern(pattern));
		}
		return formatterCache.get(pattern);
	}

	/**
	 * This method returns a cached SimpleDateFormat from formatters cache based on the given pattern if exists, if not a
	 * new SimpleDateFormat will be created, added to the cache and then returned.
	 *
	 * @param pattern
	 *           the date pattern
	 *
	 * @return a SimpleDateFormat
	 */
	public static SimpleDateFormat getSimpleDateFormatter(final String pattern)
	{
		return new SimpleDateFormat(pattern);
	}

	/**
	 * Gets days between dates.
	 *
	 * @param startDate
	 *           the start date
	 * @param endDate
	 *           the end date
	 *
	 * @return the days between dates
	 */
	public static long getDaysBetweenDates(final Date startDate, final Date endDate)
	{
		final LocalDate localStartDate = startDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		final LocalDate localEndDate = endDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		return ChronoUnit.DAYS.between(localStartDate, localEndDate);
	}


	/**
	 * Sets hours, minutes and period in a date and returns the updated date
	 *
	 * @param date
	 *           the date
	 * @param time
	 *           the time
	 * @param pattern
	 *           the pattern
	 *
	 * @return date with time
	 */
	public static Date getDateWithTime(final Date date, final String time, final String pattern)
	{
		final LocalDateTime targetDate = LocalDateTime.of(date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate(),
				LocalDateTime.parse(time, getFormatter(pattern)).toLocalTime());
		return Date.from(ZonedDateTime.of(targetDate, ZoneId.systemDefault()).toInstant());
	}

	/**
	 * Returns a date object based the String, formatted according to the pattern.
	 *
	 * @param date
	 *           the date
	 * @param pattern
	 *           the pattern
	 *
	 * @return date
	 */
	public static Date getDate(final String date, final String pattern)
	{
		final SimpleDateFormat df = getSimpleDateFormatter(pattern);
		try
		{
			return df.parse(date);
		}
		catch (final ParseException e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE,
					date);
		}
		return null;

	}

	/**
	 * Returns a time based on Date, formatted according to the pattern.
	 *
	 * @param date
	 *           the date with time
	 * @param pattern
	 *           the pattern
	 *
	 * @return time
	 */
	public static String getTimeForDate(final Date date, final String pattern)
	{
		return getSimpleDateFormatter(pattern).format(date);
	}

	/**
	 * Returns a date object based the String, formatted according to the pattern.
	 *
	 * @param dateParameter
	 *           the dateParameter
	 *
	 * @return the date from param
	 */
	public static Date createDateFromIsoDateFormat(final String dateParameter, final String pattern)
	{
		try
		{
			final SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
			return simpleDateFormat.parse(dateParameter);
		}
		catch (final ParseException pe)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE,
					dateParameter);
		}
		return null;
	}

	/**
	 * Returns a date String , formatted into given Format.
	 *
	 * @param inputDateString
	 * @param dateFormat
	 * @return the Formatted Date String
	 */
	public static String convertStringDateToGivenFormat(final String inputDateString, final String dateFormat)
	{
		try
		{
			return LocalDate.parse(inputDateString).atStartOfDay(ZoneId.systemDefault()).format(getFormatter(dateFormat));
		}
		catch (final DateTimeParseException e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE,
					inputDateString);
		}
		return null;
	}

	/**
	 * Returns localDate , formatted into given Format.
	 *
	 * @param inputDateString
	 * @param dateFormat
	 * @return the Formatted Local Date
	 */
	public static LocalDate convertStringDateToLocalDate(final String inputDateString, final String dateFormat)
	{
		try
		{
			return LocalDate.parse(inputDateString, getFormatter(dateFormat));
		}
		catch (final DateTimeParseException e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE, inputDateString);
		}
		return null;
	}

	/**
	 * Returns StringDate , formatted into given Format.
	 *
	 * @param localDate
	 * @param dateFormat
	 * @return the Formatted String Date
	 */
	public static String convertLocalDateToString(final LocalDate localDate, final String dateFormat)
	{
		final DateTimeFormatter formatter = DateTimeFormatter.ofPattern(dateFormat);
		return localDate.format(formatter);
	}
}
