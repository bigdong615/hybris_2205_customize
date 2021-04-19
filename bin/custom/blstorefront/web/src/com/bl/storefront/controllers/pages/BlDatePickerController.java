package com.bl.storefront.controllers.pages;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.bl.storefront.security.cookie.BlRentalDateCookieGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import java.text.ParseException;
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

	@Resource(name = "blDatePickerService")
	private BlDatePickerService blDatePickerService;

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
			try
			{
				if (!(blDatePickerService.checkIfSelectedDateIsSame(request, startDay, endDay)))
				{
					final StringBuilder datePickerCookieValue = new StringBuilder();
					datePickerCookieValue.append(startDay);
					datePickerCookieValue.append(BlControllerConstants.SEPARATOR);
					datePickerCookieValue.append(endDay);
					blRentalDateCookieGenerator.addCookie(response, datePickerCookieValue.toString());
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
		blDatePickerService.removeRentalDateCookie(response);
		blDatePickerService.removeRentalDatesFromSession();
		return BlControllerConstants.SUCCESS;
	}
}
