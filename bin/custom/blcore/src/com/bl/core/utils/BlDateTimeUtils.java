package com.bl.core.utils;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;

import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.DayOfWeek;
import java.time.Instant;
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
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

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
		return getFormattedDateTime(date).format(getFormatter(pattern));
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
		final LocalDateTime localDateTime = getFormattedDateTime(date);
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
			return true;
		}
	}
	
	/**
	 * Subtracts days in rental dates excluding Weekends.
	 *
	 * @param numberOfDaysToRemove
	 *           the number of days to remove
	 * @param rentalDate
	 *           the rental date
	 * @return the date
	 */
	public static Date subtractDaysInRentalDates(final int numberOfDaysToRemove, final String rentalDate, final Collection<Date> listOfBlackOutDates)
	{
		try
		{
			LocalDate localDate = BlDateTimeUtils.convertStringDateToLocalDate(rentalDate, BlCoreConstants.DATE_FORMAT);
			if (Objects.nonNull(localDate))
			{
				int subtractedDays = 0;
				while (subtractedDays < numberOfDaysToRemove)
				{
					localDate = localDate.minusDays(1);
					subtractedDays = checkForSkipingDays(localDate, subtractedDays, listOfBlackOutDates);
				}
				return Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
			}
			return getDate(rentalDate, BlCoreConstants.DATE_FORMAT);
		}
		catch (final DateTimeParseException exception)
		{
			return getDate(rentalDate, BlCoreConstants.DATE_FORMAT);
		}
	}
	
	/**
	 * Adds the days in rental dates excluding Weekends.
	 *
	 * @param numberOfDaysToAdd
	 *           the number of days to add
	 * @param rentalDate
	 *           the rental date
	 * @return the date
	 */
	public static Date addDaysInRentalDates(final int numberOfDaysToAdd, final String rentalDate, final Collection<Date> listOfBlackOutDates)
	{
		try
		{
			LocalDate localDate = BlDateTimeUtils.convertStringDateToLocalDate(rentalDate, BlCoreConstants.DATE_FORMAT);
			return addDaysInRentalDates(numberOfDaysToAdd, localDate, listOfBlackOutDates);
		}
		catch (final DateTimeParseException e)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "BlDateTimeUtils : addDaysInRentalDates : Unable to add days in date");
			return null;
		}		
	}

	/**
	 * Adds the days in rental dates excluding Weekends.
	 *
	 * @param numberOfDaysToAdd
	 *           the number of days to add
	 * @param rentalDate
	 *           the rental date
	 * @return the date
	 */
	public static Date addDaysInRentalDates(final int numberOfDaysToAdd, LocalDate localDate, final Collection<Date> listOfBlackOutDates)
	{
		try
		{
			if (Objects.nonNull(localDate))
			{
				int addedDays = 0;
				while (addedDays < numberOfDaysToAdd)
				{
					localDate = localDate.plusDays(1);
					addedDays = checkForSkipingDays(localDate, addedDays, listOfBlackOutDates);
				}
				return Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
			}
			return null;
		}
		catch (final DateTimeParseException e)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "BlDateTimeUtils : addDaysInRentalDates : Unable to add days in date");
			return null;
		}
	}

	/**
	 * Check if the date falls on weekends .
	 *
	 * @param localDate
	 *           the local date
	 * @param addedDays
	 *           the added days
	 */
	private static int checkForSkipingDays(final LocalDate localDate, int addedDays, final Collection<Date> listOfBlackOutDates)
	{
		final Date dateToCheck = Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
		if (!(localDate.getDayOfWeek() == DayOfWeek.SATURDAY || localDate.getDayOfWeek() == DayOfWeek.SUNDAY 
				|| listOfBlackOutDates.stream().anyMatch(date -> DateUtils.isSameDay(date, dateToCheck))))
		{
			return addedDays + 1;
		}
		return addedDays;
	}
	
	/**
	 * Gets the rental end date by excluding blackout dates and weekends.
	 *
	 * @param blackOutDates
	 *           the black out dates
	 * @param rentalDates
	 *           the rental dates
	 * @param lastDateToCheck
	 *           the last date to check
	 * @return the rental end date
	 */
	public static Date getRentalEndDate(final List<Date> blackOutDates, final RentalDateDto rentalDates,
			final Date lastDateToCheck)
	{
		final Date rentalEndDate = getDate(rentalDates.getSelectedToDate(), BlCoreConstants.DATE_FORMAT);
		final Date endDateIncludeShipping = addDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,
				rentalDates.getSelectedToDate(), blackOutDates);
		Date nextEndDate = null;
		if (Objects.nonNull(endDateIncludeShipping) && endDateIncludeShipping.compareTo(lastDateToCheck) >= 0)
		{
			nextEndDate = lastDateToCheck;
			boolean dateFallsOnBlackOutDate = isDateFallsOnBlackOutDate(nextEndDate, blackOutDates)
					&& nextEndDate.compareTo(rentalEndDate) > 0;
			while (dateFallsOnBlackOutDate)
			{
				nextEndDate = Date.from(getFormattedDateTime(nextEndDate)
						.minusDays(1).atZone(ZoneId.systemDefault()).toInstant());
				dateFallsOnBlackOutDate = isDateFallsOnBlackOutDate(nextEndDate, blackOutDates)
						&& nextEndDate.compareTo(rentalEndDate) > 0;
			}
			return nextEndDate;
		}
		return endDateIncludeShipping;
	}

	/**
	 * Gets the formatted local date time object from date object.
	 *
	 * @param date
	 *           the date
	 * @return the formatted date time
	 */
	public static LocalDateTime getFormattedDateTime(final Date date)
	{
		final Instant instant = Instant.ofEpochMilli(date.getTime());
		return LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
	}

	/**
	 * Checks if is date falls on black out dates or weekends.
	 *
	 * @param dateToCheck
	 *           the date to check
	 * @param blackOutDates
	 *           the black out dates
	 * @return true, if is date falls on black out date
	 */
	public static boolean isDateFallsOnBlackOutDate(final Date dateToCheck, final List<Date> blackOutDates)
	{
		final LocalDate localDate = dateToCheck.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
		return localDate.getDayOfWeek() == DayOfWeek.SATURDAY || localDate.getDayOfWeek() == DayOfWeek.SUNDAY
				|| blackOutDates.stream().anyMatch(date -> DateUtils.isSameDay(date, dateToCheck));
	}

	/**
	 * This method will return business days difference considering cutOff time for current day
	 *
	 * @param rentalStart date
	 * @return int difference of days
	 */
	public static int getBusinessDaysDifferenceWithCutOffTime(final Date optimizedDate, final Date rentalStart, final String time) {
		int result = BlDateTimeUtils.getDaysBetweenBusinessDays(BlDateTimeUtils.convertDateToStringDate(
				optimizedDate, BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN), BlDateTimeUtils.convertDateToStringDate(
				rentalStart, BlDeliveryModeLoggingConstants.RENTAL_FE_DATE_PATTERN));
		if(!BlDateTimeUtils.compareTimeWithCutOff(time)) {
			result = result - BlInventoryScanLoggingConstants.ONE;
		}
		return result;
	}

	/**
	 * this method will return pass date
	 *
	 * @return
	 */
	public static String getYesterdayDate() {
		final Calendar calendar = Calendar.getInstance();
		calendar.setTimeZone(TimeZone.getTimeZone(BlDeliveryModeLoggingConstants.ZONE_PST));
		calendar.add(Calendar.DATE, -BlInventoryScanLoggingConstants.ONE);
		return new SimpleDateFormat(BlDeliveryModeLoggingConstants.RENTAL_FE_DATE_PATTERN).format(calendar.getTime());
	}

	/**
	 * javadoc
	 * this method will convert E MMM dd HH:mm:ss Z yyyy to dd-MM-yyyy
	 *
	 * @param date date
	 * @return string date
	 */
	public static String getDateInStringFormat(final Date date) {
		return new SimpleDateFormat(BlDeliveryModeLoggingConstants.RENTAL_FE_DATE_PATTERN).format(date);
	}

	/**
	 * This method will return new Date after subtracting given no of days from it considering blackout dates
	 *
	 * @param givenDate date
	 * @return noOfDaysToSubtract integer to be subtracted as days
	 */
	public static Date getDateWithSubtractedDays(final int noOfDaysToSubtract, final Date givenDate,
			final List<Date> holidayBlackoutDates) {

		final String stringGivenDate = BlDateTimeUtils
				.convertDateToStringDate(givenDate, BlCoreConstants.DATE_FORMAT);

		return BlDateTimeUtils
				.subtractDaysInRentalDates(noOfDaysToSubtract, stringGivenDate, holidayBlackoutDates);
	}

	/**
	 * This method will return new Date after adding given no of days to it considering blackout dates
	 *
	 * @param givenDate date
	 * @return noOfDaysToAdd integer to be added as days
	 */
	public static Date getDateWithAddedDays(final int noOfDaysToAdd, final Date givenDate,
			final List<Date> holidayBlackoutDates) {

		final String stringGivenDate = BlDateTimeUtils
				.convertDateToStringDate(givenDate, BlCoreConstants.DATE_FORMAT);

		return BlDateTimeUtils
				.addDaysInRentalDates(noOfDaysToAdd, stringGivenDate, holidayBlackoutDates);
	}

	/**
	 * This method will return new Date after adding given no of days to it considering blackout dates
	 *
	 * @param givenDate date
	 * @return noOfDaysToAdd integer to be added as days
	 */
	public static Date getFinalEndDateConsideringPostBlackoutDates(final int noOfDaysToAdd,
			final String givenDate,
			final List<Date> holidayBlackoutDates) {

		final Date endDate = BlDateTimeUtils
				.addDaysInRentalDates(noOfDaysToAdd, givenDate, holidayBlackoutDates);

		final LocalDate finalEndLocalDate = getDateBySkippingBlackoutDates(holidayBlackoutDates, endDate);

		return null == finalEndLocalDate ? endDate
				: Date.from(finalEndLocalDate.atStartOfDay(ZoneId.systemDefault()).toInstant());
	}

	/**
	 * This method will check if the given date falls on blackout date, if yes, then update itself and
	 * check for the next date and so on.
	 *
	 * @param holidayBlackoutDates
	 * @param date            date
	 */
	private static LocalDate getDateBySkippingBlackoutDates(
			final List<Date> holidayBlackoutDates,
			Date date) {

		//checking if next date of end date is a backout date and update
		LocalDate nextLocalDate = getNextLocalDate(date);

		if (null != nextLocalDate && BlDateTimeUtils
				.checkIfWeekendOrBlackoutDates(nextLocalDate, holidayBlackoutDates)) {

			getDateBySkippingBlackoutDates(holidayBlackoutDates,
					Date.from(nextLocalDate.atStartOfDay(ZoneId.systemDefault()).toInstant()));
		} else {

			nextLocalDate = convertDateToLocalDate(date);
		}

		return nextLocalDate;
	}

	/**
	 * This method will return new LocalDate after adding 1 day to it
	 *
	 * @param date
	 * @return localdate
	 */
	public static LocalDate getNextLocalDate(final Date date) {

		final LocalDate localDate = convertDateToLocalDate(date);

		return null == localDate ? null : localDate.plusDays(1);
	}

	/**
	 * This method will return new LocalDate after converting the given date
	 *
	 * @param date
	 * @return localdate
	 */
	public static LocalDate convertDateToLocalDate(final Date date) {

		final String stringDate = BlDateTimeUtils
				.convertDateToStringDate(date, BlCoreConstants.DATE_FORMAT);

		return BlDateTimeUtils
				.convertStringDateToLocalDate(stringDate, BlCoreConstants.DATE_FORMAT);
	}
	
	/** This method will update the Actual Rental End date for order
	 * @param abstractOrderModel
	 * @param zoneDeliveryMode
	 * @param dateFormat
	 */
	public static void updateActualRentalEndDate(final AbstractOrderModel abstractOrderModel,
			final ZoneDeliveryModeModel zoneDeliveryMode, final DateFormat dateFormat)
	{
		final int postDaysToAdd = Integer.parseInt(zoneDeliveryMode.getPostReservedDays());						
		final String rentalEndDate = dateFormat.format(abstractOrderModel.getRentalEndDate());
		LocalDate localEndDate = BlDateTimeUtils.convertStringDateToLocalDate(rentalEndDate, BlCoreConstants.DATE_FORMAT);
		if (Objects.nonNull(localEndDate))
		{
			localEndDate = localEndDate.plusDays(postDaysToAdd);

			abstractOrderModel
					.setActualRentalEndDate(Date.from(localEndDate.atStartOfDay(ZoneId.systemDefault()).toInstant()));
		}
	}

	/**
	 * This method will update the Actual Rental Start date for order
	 * @param abstractOrderModel
	 * @param zoneDeliveryMode
	 * @param dateFormat
	 */
	public static void updateActualRentalStartDate(final AbstractOrderModel abstractOrderModel,
			final ZoneDeliveryModeModel zoneDeliveryMode, final DateFormat dateFormat)
	{
		final int preDaysToDeduct = Integer.parseInt(zoneDeliveryMode.getPreReservedDays());
		final String rentalStartDate = dateFormat.format(abstractOrderModel.getRentalStartDate());
		LocalDate localStartDate = BlDateTimeUtils.convertStringDateToLocalDate(rentalStartDate, BlCoreConstants.DATE_FORMAT);
		if (Objects.nonNull(localStartDate))
		{
			localStartDate = localStartDate.minusDays(preDaysToDeduct);

			abstractOrderModel
					.setActualRentalStartDate(Date.from(localStartDate.atStartOfDay(ZoneId.systemDefault()).toInstant()));
		}
	}

	/**
	 * Check if the date falls on weekends or blackout dates.
	 *
	 * @param localDate
	 *           the local date
	 * @param listOfBlackOutDates
	 *           the listOfBlackOutDates days
	 */
	private static boolean checkIfWeekendOrBlackoutDates(final LocalDate localDate,
			final Collection<Date> listOfBlackOutDates) {

		final Date dateToCheck = Date.from(localDate.atStartOfDay(ZoneId.systemDefault()).toInstant());

		return localDate.getDayOfWeek() == DayOfWeek.SATURDAY
				|| localDate.getDayOfWeek() == DayOfWeek.SUNDAY
				|| listOfBlackOutDates.stream().anyMatch(date -> DateUtils.isSameDay(date, dateToCheck));
	}

	/**
	 * This method created to format the amount for double
	 * @param amount the amount
	 * @return the string
	 */
	public static String formatAmount(final Double amount) {
		final DecimalFormat decimalFormat = (DecimalFormat) NumberFormat.getNumberInstance(Locale.ENGLISH);
		decimalFormat.applyPattern(BlCoreConstants.FORMAT_STRING);
		return decimalFormat.format(amount);
	}

	/**
	 * This method used for adding no of days in given date.
	 * @param date
	 * @param noOfDays
	 * @return
	 */
	public static Date addingNoOfDaysInGivenDate(final Date date,final int noOfDays){
		final Calendar calendar = Calendar.getInstance();
		calendar.setTime(date);
		calendar.add(Calendar.DAY_OF_MONTH ,noOfDays);
		return calendar.getTime();
	}
}
