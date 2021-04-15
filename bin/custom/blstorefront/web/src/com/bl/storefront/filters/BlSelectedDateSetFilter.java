package com.bl.storefront.filters;

import de.hybris.platform.servicelayer.session.SessionService;

import java.io.IOException;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.util.WebUtils;

import com.bl.core.utils.BlDateTimeUtils;
import com.bl.storefront.controllers.pages.BlControllerConstants;


/**
 * To get the value from cookie and set the same in sessionService
 *
 * @author Moumita
 *
 */
public class BlSelectedDateSetFilter extends OncePerRequestFilter
{
	private SessionService sessionService;

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
		final Cookie selectedDateCookie = WebUtils.getCookie(request, BlControllerConstants.COOKIE_NAME_FOR_DATE);
		if (null != selectedDateCookie && null != selectedDateCookie.getValue())
		{
			final String date = selectedDateCookie.getValue();
			final List<String> lSelectedDates = Arrays.asList(date.split(BlControllerConstants.SEPARATOR));
			if (CollectionUtils.isNotEmpty(lSelectedDates) && lSelectedDates.size() == BlControllerConstants.PAIR_OF_DATES)
			{
				final LocalDate selectedFromDate = BlDateTimeUtils.convertStringDateToLocalDate(lSelectedDates.get(0).trim(),
						BlControllerConstants.DATE_FORMAT_SEPARATED_BY_SLASH);
				final LocalDate selectedToDate = BlDateTimeUtils.convertStringDateToLocalDate(lSelectedDates.get(1).trim(),
						BlControllerConstants.DATE_FORMAT_SEPARATED_BY_SLASH);
				final LocalDate currentDate = LocalDate.now();
				if (selectedFromDate.isBefore(currentDate))
				{
					getSessionService().removeAttribute(BlControllerConstants.SELECTED_FROM_DATE);
					getSessionService().removeAttribute(BlControllerConstants.SELECTED_TO_DATE);
				}
				else
				{
					getSessionService().setAttribute(BlControllerConstants.SELECTED_FROM_DATE, selectedFromDate);
					getSessionService().setAttribute(BlControllerConstants.SELECTED_TO_DATE, selectedToDate);
				}
			}
		}
		filterChain.doFilter(request, response);
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
