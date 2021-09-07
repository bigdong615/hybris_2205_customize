package com.bl.core.model.interceptor;

import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;

import java.util.Date;
import java.util.Objects;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.enums.BlackoutDateShippingMethodEnum;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlBlackoutDateModel;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;


/**
 * Prepare interceptor to Modify data before saving on BlBlackoutDateModel
 *
 * @author Ravikumar
 *
 */
public class BlBlackoutDatePrepareInterceptor implements PrepareInterceptor<BlBlackoutDateModel>
{
	private static final Logger LOG = Logger.getLogger(BlBlackoutDatePrepareInterceptor.class);

	@Override
	public void onPrepare(final BlBlackoutDateModel blBlackoutDateModel, final InterceptorContext interceptorContext)
			throws InterceptorException
	{
		if (Objects.nonNull(blBlackoutDateModel))
		{
			setBlackoutDateTimeToStartOfDay(blBlackoutDateModel, interceptorContext);
			setDefaultShippingGroupOrMethodToAll(blBlackoutDateModel);
		}
	}

	/**
	 * Sets the blackout date time to start of day.
	 *
	 * @param blBlackoutDateModel
	 *           the bl blackout date model
	 * @param interceptorContext
	 *           the interceptor context
	 */
	private void setBlackoutDateTimeToStartOfDay(final BlBlackoutDateModel blBlackoutDateModel,
			final InterceptorContext interceptorContext)
	{
		if (interceptorContext.isNew(blBlackoutDateModel)
				|| interceptorContext.isModified(blBlackoutDateModel, BlBlackoutDateModel.BLACKOUTDATE))
		{
			final Date blackoutDate = blBlackoutDateModel.getBlackoutDate();
			if (Objects.nonNull(blackoutDate))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Date selected for Blackout Date : {}", blackoutDate);
				final Date dateWithStartTime = BlDateTimeUtils.getFormattedStartDay(blackoutDate).getTime();
				blBlackoutDateModel.setBlackoutDate(dateWithStartTime);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						"Changing Date : {} to Date with time of start of the day : {} for BlBlackoutDate with PK : {}", blackoutDate,
						dateWithStartTime, blBlackoutDateModel.getPk());
			}
		}
	}

	/**
	 * Sets the default shipping group or method to ALL in case of Type set to HOLIDAY or RENTAL_END_DATE.
	 *
	 * @param blBlackoutDateModel
	 *           the new default shipping group or method to all
	 */
	private void setDefaultShippingGroupOrMethodToAll(final BlBlackoutDateModel blBlackoutDateModel)
	{
		if (Objects.nonNull(blBlackoutDateModel.getBlackoutDateType())
				&& (blBlackoutDateModel.getBlackoutDateType().equals(BlackoutDateTypeEnum.RENTAL_END_DATE)
						|| blBlackoutDateModel.getBlackoutDateType().equals(BlackoutDateTypeEnum.HOLIDAY)))
		{
			blBlackoutDateModel.setBlockedShippingMethod(BlackoutDateShippingMethodEnum.ALL);
		}
	}
}
