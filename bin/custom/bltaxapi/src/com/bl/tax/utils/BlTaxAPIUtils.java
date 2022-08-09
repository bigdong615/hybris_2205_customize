package com.bl.tax.utils;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.product.ProductModel;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This Util class created for common methods in avalara tax
 */
public class BlTaxAPIUtils {
	
	private static final Logger LOG = Logger.getLogger(BlTaxAPIUtils.class);
	private static final String UNABLE_TO_PARSE_DATE = "Unable to parse String : {} to Date.";
	private static Map<String, DateTimeFormatter> formatterCache = new HashMap<>();

  private BlTaxAPIUtils()
  {
    //empty to avoid instantiating utils class
  }

  /**
   * This method created to get product id from entry
   * @param productModel productModel
   * @return String
   */
  public static String getProductId(final ProductModel productModel) {
    final AtomicReference<String> stringAtomicReference = new AtomicReference<>(StringUtils.EMPTY);
    if(productModel instanceof BlSerialProductModel) {
      final BlSerialProductModel blSerialProductModel = (BlSerialProductModel) productModel;
      stringAtomicReference.set(StringUtils.isBlank(blSerialProductModel.getProductId()) ? StringUtils.EMPTY : blSerialProductModel.getProductId());
    }
    else {
      final BlProductModel blProductModel = (BlProductModel) productModel;
      stringAtomicReference.set(StringUtils.isBlank(blProductModel.getProductId()) ? StringUtils.EMPTY : blProductModel.getProductId());
    }
    return stringAtomicReference.get();
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
	 * Convert string date to local date.
	 *
	 * @param inputDateString the input date string
	 * @param dateFormat the date format
	 * @return the local date
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
	 * Gets the formatter.
	 *
	 * @param pattern the pattern
	 * @return the formatter
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
	 * Gets the simple date formatter.
	 *
	 * @param pattern the pattern
	 * @return the simple date formatter
	 */
	public static SimpleDateFormat getSimpleDateFormatter(final String pattern)
	{
		return new SimpleDateFormat(pattern);
	}

}
