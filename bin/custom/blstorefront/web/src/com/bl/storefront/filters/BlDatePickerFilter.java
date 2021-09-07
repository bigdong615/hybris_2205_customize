package com.bl.storefront.filters;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import java.io.IOException;
import java.time.LocalDate;
import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.springframework.web.filter.OncePerRequestFilter;


/**
 * To get the value from cookie and set the same in sessionService
 *
 * @author Moumita
 *
 */
public class BlDatePickerFilter extends OncePerRequestFilter
{
	private static final Logger LOG = Logger.getLogger(BlDatePickerFilter.class);
	private BlDatePickerService blDatePickerService;
	private BlCartService blCartService;

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
		String selectedToDate = rentalDateDto.getSelectedToDate();
		final String selectedDuration = rentalDateDto.getSelectedDays();
		final LocalDate startDate = BlDateTimeUtils.convertStringDateToLocalDate(selectedFromDate,
				BlCoreConstants.DATE_FORMAT);


		if (startDate.isBefore(currentDate))
		{
			getBlDatePickerService().removeRentalDatesFromSession();
		}

		else
		{
			selectedToDate = getBlCartService().getSessionCart().getRentalEndDate() != null ? BlDateTimeUtils.convertDateToStringDate(getBlCartService().getSessionCart().getRentalEndDate(),BlCoreConstants.DATE_FORMAT) : selectedToDate;
			getBlDatePickerService().addRentalDatesIntoSession(selectedFromDate, selectedToDate);
			getBlDatePickerService().addSelectedRentalDurationIntoSession(selectedDuration);
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

	public BlCartService getBlCartService() {
		return blCartService;
	}

	public void setBlCartService(BlCartService blCartService) {
		this.blCartService = blCartService;
	}
}
