package com.bl.storefront.controllers.pages;

import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;

import java.text.ParseException;
import java.util.Date;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.bl.core.utils.BlDateTimeUtils;
import com.bl.storefront.security.cookie.BlDateRestoreCookieGenerator;


/**
 * Controller to set the date in cookie
 *
 * @author Moumita
 */
@Controller
public class BlDatePickerController extends AbstractPageController
{
	@Resource(name = "blDateRestoreCookieGenerator")
	private BlDateRestoreCookieGenerator blDateRestoreCookieGenerator;

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
	@RequestMapping(value = "/datepicker", method = RequestMethod.GET)
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
					BlControllerConstants.DATE_FORMAT_SEPARATED_BY_SLASH);
			final Date endDate = BlDateTimeUtils.convertStringDateToDate(selectedEndDate,
					BlControllerConstants.DAY_MON_DATE_YEAR_FORMAT);
			final String endDay = BlDateTimeUtils.convertDateToStringDate(endDate,
					BlControllerConstants.DATE_FORMAT_SEPARATED_BY_SLASH);
			final String date = startDay + "-" + endDay;
			blDateRestoreCookieGenerator.addCookie(response, date);
		}
		return BlControllerConstants.SUCCESS;
	}
}
