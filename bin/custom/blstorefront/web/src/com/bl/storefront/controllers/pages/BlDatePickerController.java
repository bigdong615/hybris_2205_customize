package com.bl.storefront.controllers.pages;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.logging.BlLogger;
import com.bl.storefront.security.cookie.BlRentalDateCookieGenerator;
import com.bl.storefront.security.cookie.BlRentalDurationCookieGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import java.text.ParseException;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


/**
 * Controller to set the date in cookie
 *
 * @author Moumita
 */
@Controller
public class BlDatePickerController extends AbstractPageController
{
	private static final Logger LOG = Logger.getLogger(BlDatePickerController.class);

	@Resource(name = "blRentalDateCookieGenerator")
	private BlRentalDateCookieGenerator blRentalDateCookieGenerator;

	@Resource(name = "blRentalDurationCookieGenerator")
	private BlRentalDurationCookieGenerator blRentalDurationCookieGenerator;

	@Resource(name = "blDatePickerService")
	private BlDatePickerService blDatePickerService;
	
	@Resource(name ="cartFacade")
	private BlCartFacade blCartFacade;

	/**
	 * It sets the selected date in cookie
	 *
	 * @param selectedStartDate
	 * @param selectedEndDate
	 * @param request
	 * @param response
	 * @param model
	 * @param redirectModel
	 * @return
	 * @throws ParseException
	 */
	@GetMapping(value = "/datepicker")
	@ResponseBody
	public String setCookieForDate(@RequestParam(value = BlControllerConstants.SELECTED_FROM_DATE, defaultValue = "")
	final String selectedStartDate, @RequestParam(value = BlControllerConstants.SELECTED_TO_DATE, defaultValue = "")
	final String selectedEndDate, final HttpServletRequest request, final HttpServletResponse response, final Model model,
			final RedirectAttributes redirectModel) throws ParseException
	{
		if (StringUtils.isNotEmpty(selectedStartDate) && StringUtils.isNotEmpty(selectedEndDate))
		{
			final Date startDate = BlDateTimeUtils.convertStringDateToDate(selectedStartDate,
					BlControllerConstants.DAY_MON_DATE_YEAR_FORMAT);
			final String startDay = BlDateTimeUtils.convertDateToStringDate(startDate,
					BlControllerConstants.DATE_FORMAT_PATTERN);
			final Date endDate = BlDateTimeUtils.convertStringDateToDate(selectedEndDate,
					BlControllerConstants.DAY_MON_DATE_YEAR_FORMAT);
			final String endDay = BlDateTimeUtils.convertDateToStringDate(endDate,
					BlControllerConstants.DATE_FORMAT_PATTERN);
			final String rentalDuration = String.valueOf(ChronoUnit.DAYS.between(BlDateTimeUtils.convertStringDateToLocalDate(startDay, BlCoreConstants.DATE_FORMAT),
							BlDateTimeUtils.convertStringDateToLocalDate(endDay, BlCoreConstants.DATE_FORMAT)));
			try
			{
				if (!(blDatePickerService.checkIfSelectedDateIsSame(request, startDay, endDay)))
				{
					final StringBuilder datePickerCookieValue = new StringBuilder();
					datePickerCookieValue.append(startDay);
					datePickerCookieValue.append(BlControllerConstants.SEPARATOR);
					datePickerCookieValue.append(endDay);
					blRentalDateCookieGenerator.addCookie(response, datePickerCookieValue.toString());
					//added for getting explicit days on the Date Picker selected by user
					blRentalDurationCookieGenerator.addCookie(response, rentalDuration);
					// Setting rental start date and end date on cart
					getBlCartFacade().setRentalDatesOnCart(startDate, endDate);
					// Resetting cart and cart entries calculation flag to false, so that it recalculates once the rental dates is changed
					getBlCartFacade().resetCartCalculationFlag(); 
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Cookie added for {} for the duration of {} and {} ",
							BlControllerConstants.SELECTED_DATE, startDay, endDay);
				}
			}
			catch (final JsonProcessingException e)
			{
				BlLogger.logMessage(LOG, Level.ERROR, "JsonProcessingException while getting the cookie : {} ", e);
			}
		}
		return BlControllerConstants.SUCCESS;
	}

	/**
	 * It removed the selected date from cookie
	 *
	 * @param request
	 * @param response
	 * @param model
	 * @param redirectModel
	 * @return
	 * @throws ParseException
	 */
	@GetMapping(value = "/resetDatepicker")
	@ResponseBody
	public String resetDate(final HttpServletRequest request, final HttpServletResponse response, final Model model,
			final RedirectAttributes redirectModel) throws ParseException
	{
		blRentalDateCookieGenerator.removeCookie(response);
		blDatePickerService.removeRentalDatesFromSession();
		// Resetting rental start date and end date on cart
		getBlCartFacade().setRentalDatesOnCart(null, null);
		// Resetting cart and cart entries calculation flag to false, so that it recalculates once the rental dates is reset
		getBlCartFacade().resetCartCalculationFlag(); 
		return BlControllerConstants.SUCCESS;
	}

	/**
	 * @return the blCartFacade
	 */
	public BlCartFacade getBlCartFacade()
	{
		return blCartFacade;
	}

	/**
	 * @param blCartFacade the blCartFacade to set
	 */
	public void setBlCartFacade(BlCartFacade blCartFacade)
	{
		this.blCartFacade = blCartFacade;
	}
}
