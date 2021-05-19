package com.bl.core.utils;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
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
	public static String convertLocalDateToString(final LocalDate localDate, final String dateFormat) {
		final DateTimeFormatter formatter = DateTimeFormatter.ofPattern(dateFormat);
		return localDate.format(formatter);
	}

	/**
	 * To get the formatted date
	 * @param day the date
	 * @return Calendar
	 */
	public static Calendar getFormattedStartDay(final Date day) {
		final Calendar startDate = new GregorianCalendar();
		startDate.setTime(day);
		startDate.set(Calendar.HOUR_OF_DAY, BlCoreConstants.START_HOURS);
		startDate.set(Calendar.MINUTE, BlCoreConstants.START_MINUTES);
		startDate.set(Calendar.SECOND, BlCoreConstants.START_SECONDS);
		return startDate;
	}

	/**
	 * To get the formatted date
	 * @param day the date
	 * @return Calendar
	 */
	public static Calendar getFormattedEndDay(final Date day) {
		final Calendar startDate = new GregorianCalendar();
		startDate.setTime(day);
		startDate.set(Calendar.HOUR_OF_DAY, BlCoreConstants.END_HOURS);
		startDate.set(Calendar.MINUTE, BlCoreConstants.END_MINUTES);
		startDate.set(Calendar.SECOND, BlCoreConstants.END_SECONDS);
		return startDate;
	}

	/**
	 * @author Namrata Lohar
	 * This method will take string and return locale date
	 *
	 * @param date means current Day
	 * @return LocalDate date
	 *
	 */
	public static LocalDate convertToLocaleDate(final String date) {
		try
		{
			final SimpleDateFormat sdf = new SimpleDateFormat(BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN);
			final Date newDate = sdf.parse(date);
			return LocalDate.parse(new SimpleDateFormat(BlDeliveryModeLoggingConstants.LOCAL_DATE_PATTERN).format(newDate));
		}
		catch (final DateTimeParseException | ParseException e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE,
					date);
		}
		return null;
	}

	/**
	 * @author Namrata Lohar
	 * This method will calculate difference between two business days
	 *
	 * @param startDate means current Day
	 * @param endDate means start rental date
	 * @return long i.e., difference in days
	 *
	 * Note: these parameters are getting updated in method body that is why not marked as final otherwise need to explicitely
	 * take extra attributes!!
	 */
	public static int getDaysBetweenBusinessDays(final String startDate, final String endDate) {
		LocalDate start = BlDateTimeUtils.convertStringDateToLocalDate(startDate, BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN);
		LocalDate end = BlDateTimeUtils.convertStringDateToLocalDate(endDate, BlDeliveryModeLoggingConstants.RENTAL_FE_DATE_PATTERN);
		if(null != start && null != end) {
			if (start.getDayOfWeek().getValue() > BlInventoryScanLoggingConstants.FIVE) {
				start = start.with(TemporalAdjusters.next(DayOfWeek.MONDAY));
			}
			if (end.getDayOfWeek().getValue() > BlInventoryScanLoggingConstants.FIVE) {
				end = end.with(TemporalAdjusters.previous(DayOfWeek.FRIDAY));
			}
			if (start.isAfter(end)) {
				return BlInventoryScanLoggingConstants.ZERO;
			}
			Long weeks = ChronoUnit.WEEKS.between(start, end);
			if (start.getDayOfWeek().getValue() > end.getDayOfWeek().getValue()) {
				weeks += BlInventoryScanLoggingConstants.ONE;
			}
			return BlInventoryScanLoggingConstants.FIVE * weeks.intValue() + end.getDayOfWeek().getValue() - start.getDayOfWeek().getValue();
		}
		return BlInventoryScanLoggingConstants.ZERO;
	}

	/**
	 * @author Namrata Lohar
	 *
	 * This method will give us current time in specified time-zone time zone
	 * @return String time HH:mm
	 */
	public static String getCurrentTimeUsingCalendar(final String timeZone) {
		final DateFormat dateFormat = new SimpleDateFormat(BlDeliveryModeLoggingConstants.DATE_TIME);
		dateFormat.setTimeZone(TimeZone.getTimeZone(timeZone));
		return dateFormat.format(Calendar.getInstance().getTime());
	}

	/**
	 * @author Namrata Lohar
	 *
	 * This method will give us current time in specified time-zone time zone
	 * @return String date in specified zone
	 */
	public static String getCurrentDateUsingCalendar(final String timeZone, final Date date) {
		final DateFormat dateFormat = new SimpleDateFormat(BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN);
		dateFormat.setTimeZone(TimeZone.getTimeZone(timeZone));
		return dateFormat.format(date);
	}

	/**
	 * This method will convert string to date with specified time zone
	 *
	 * @param date date
	 * @param zone timeZone
	 * @return date
	 */
	public static Date getStringToDateWithTimeZone(final String date, final String zone) {
		try
		{
			SimpleDateFormat sdf = null;
			if(date.contains("-")) {
				sdf = new SimpleDateFormat(BlDeliveryModeLoggingConstants.RENTAL_FE_DATE_PATTERN);
			} else {
				sdf = new SimpleDateFormat("E MMM dd HH:mm:ss Z yyyy");
			}
			sdf.setTimeZone(TimeZone.getTimeZone(zone));
			return sdf.parse(date);
		}
		catch (final DateTimeParseException | ParseException e)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE, date);
		}
		return null;

	}

	/**
	 * This method will return day of week for input timezone
	 *
	 * @param timeZone PST/EST
	 * @return Day of Week like SUNDAY
	 */
	public static DayOfWeek getDayOfWeek(final String timeZone, final String date) {
		return LocalDate.parse(getCurrentDateUsingCalendar(timeZone, getStringToDateWithTimeZone(date, BlDeliveryModeLoggingConstants.ZONE_PST)),
				getFormatter(BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN)).getDayOfWeek();
	}

	/**
	 * It gets the date which is after a year
	 *
	 * @return the date
	 */
	public static Date getNextYearsSameDay()
	{
		final Date currentDate = new Date();
		final Calendar calendar = Calendar.getInstance();
		calendar.setTime(currentDate);
		calendar.add(Calendar.YEAR, 1);
		return calendar.getTime();
	}

	/**
	 * This method will calculate time to check cutOff time condition
	 *
	 * @param time zone mode time to compare
	 * @return true/false
	 */
	public static boolean compareTimeWithCutOff(final String time) {
		try {
			final SimpleDateFormat sdf = new SimpleDateFormat(BlDeliveryModeLoggingConstants.DATE_TIME);
			return StringUtils.isNotEmpty(time) ? sdf.parse(BlDateTimeUtils.getCurrentTimeUsingCalendar(
					BlDeliveryModeLoggingConstants.ZONE_PST)).before(sdf.parse(time)) : Boolean.FALSE;
		} catch (ParseException e) {
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, UNABLE_TO_PARSE_DATE, time);
			return false;
		}
	}
}
