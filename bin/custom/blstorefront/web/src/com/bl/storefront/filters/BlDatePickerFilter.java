package com.bl.storefront.filters;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import java.io.IOException;

import java.time.LocalDate;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.web.filter.OncePerRequestFilter;

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
	 * To get the date picker date from cookie and set the same in sessionService, so that the date will be applicable throughout
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
		final RentalDateDto rentalDateDto = getBlDatePickerService().getRentalDatesFromCookie(request);
		if (null != rentalDateDto)
		{
			setOrRemoveRentalDateInSession(rentalDateDto);
		}
		filterChain.doFilter(request, response);
	}

	/**
	 * It sets the rental date into session or remove it from the session based on condition
	 *
	 * @param rentalDateDto
	 */
	private void setOrRemoveRentalDateInSession(final RentalDateDto rentalDateDto)
	{
		final LocalDate currentDate = LocalDate.now();
		final String selectedFromDate = rentalDateDto.getSelectedFromDate();
		final String selectedToDate = rentalDateDto.getSelectedToDate();
		final LocalDate startDate = BlDateTimeUtils.convertStringDateToLocalDate(selectedFromDate,
				BlCoreConstants.DATE_FORMAT);
		if (startDate.isBefore(currentDate))
		{
			getBlDatePickerService().removeRentalDatesFromSession();
		}
		else
		{
			getBlDatePickerService().addRentalDatesIntoSession(selectedFromDate, selectedToDate);
		}
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
