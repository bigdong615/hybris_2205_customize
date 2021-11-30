package com.bl.core.model.interceptor;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.ValidateInterceptor;

import java.util.Date;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.services.customer.BlUserService;
import com.bl.logging.BlLogger;


/**
 * @author Ravikumar
 *
 */
public class BlOrderValidateInterceptor implements ValidateInterceptor<OrderModel>
{

	private static final Logger LOG = Logger.getLogger(BlOrderValidateInterceptor.class);

	private BlUserService userService;

	@Override
	public void onValidate(final OrderModel orderModel, final InterceptorContext interceptorContext) throws InterceptorException
	{
		validateRentalDateChange(orderModel);
	}

	private void validateRentalDateChange(final OrderModel orderModel) throws InterceptorException
	{
		if (getUserService().isCsUser() && ObjectUtils.allNotNull(orderModel.getRentalStartDate(), orderModel.getRentalEndDate()))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
					"Validating Order for Rental Start Date : {} and Rental End Date : {} for CS user for Order : {}",
					orderModel.getRentalStartDate(), orderModel.getRentalEndDate(), orderModel.getCode());
			final Date rentalStartDate = orderModel.getRentalStartDate();
			final Date rentalEndDate = orderModel.getRentalEndDate();
			if (DateUtils.isSameDay(rentalStartDate, rentalEndDate) || rentalStartDate.compareTo(rentalEndDate) > 0
					|| rentalEndDate.compareTo(rentalStartDate) < 0)
			{
				throw new InterceptorException("Rental Start Date should not be a date later than Rental End Date");
			}
		}
	}

	/**
	 * @return the userService
	 */
	public BlUserService getUserService()
	{
		return userService;
	}

	/**
	 * @param userService
	 *           the userService to set
	 */
	public void setUserService(final BlUserService userService)
	{
		this.userService = userService;
	}

}
