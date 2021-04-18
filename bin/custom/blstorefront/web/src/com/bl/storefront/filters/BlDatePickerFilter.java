package com.bl.storefront.filters;
import java.io.IOException;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.web.filter.OncePerRequestFilter;

import com.bl.core.data.BlDatePicker;
import com.bl.core.datepicker.BlDatePickerService;


/**
 * To get the value from cookie and set the same in sessionService
 *
 * @author Moumita
 *
 */
public class BlDatePickerFilter extends OncePerRequestFilter
{
	private BlDatePickerService blDatePickerService;

	/**
	 * To get the value from cookie and set the same in sessionService, so that the date will be applicable throughout
	 * the website
	 *
	 * @param request
	 * @param response
	 * @param filterChain
	 */
	@Override
	protected void doFilterInternal(final HttpServletRequest request, final HttpServletResponse response,
			final FilterChain filterChain) throws ServletException, IOException
	{
		final BlDatePicker blDatePicker = getBlDatePickerService().getCookieForDatePicker(request);
		if (null != blDatePicker)
		{
			getBlDatePickerService().setOrRemoveDatePickerInSession(blDatePicker);
		}
		filterChain.doFilter(request, response);
	}

	/**
	 * @return the blDatePickerService
	 */
	public BlDatePickerService getBlDatePickerService()
	{
		return blDatePickerService;
	}

	/**
	 * @param blDatePickerService
	 *           the blDatePickerService to set
	 */
	public void setBlDatePickerService(final BlDatePickerService blDatePickerService)
	{
		this.blDatePickerService = blDatePickerService;
	}

}
