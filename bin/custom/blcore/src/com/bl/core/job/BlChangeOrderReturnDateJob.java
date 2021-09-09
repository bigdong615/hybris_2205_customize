package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import java.util.Date;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.blackout.date.dao.BlBlackoutDatesDao;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlBlackoutDateModel;
import com.bl.core.services.blackout.BlBlackoutDateService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;


/**
 * This cronjob will change the optimized rental end date to next available date as per the defined Blackout date for
 * Rental End Date
 *
 * @author Ravikumar
 *
 */
public class BlChangeOrderReturnDateJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlChangeOrderReturnDateJob.class);
	private BlBlackoutDateService blBlackoutDateService;
	private BlDatePickerService blDatePickerService;
	private BlBlackoutDatesDao blBlackoutDatesDao;

	@Override
	public PerformResult perform(final CronJobModel cronJobModel)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlChangeOrderReturnDateJob...");
		try
		{
			final Date currentDate = BlDateTimeUtils.getFormattedStartDay(new Date()).getTime();
			final List<BlBlackoutDateModel> allBlackoutRentalEndDates = getBlBlackoutDatesDao()
					.getAllBlackoutDatesForGivenTypeAndFromDate(currentDate, BlackoutDateTypeEnum.RENTAL_END_DATE);
			if (CollectionUtils.isNotEmpty(allBlackoutRentalEndDates))
			{
				final List<Date> blackoutDates = getBlDatePickerService()
						.getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
				allBlackoutRentalEndDates.forEach(blackoutDate -> getBlBlackoutDateService()
						.performOrderReturnDateChange(blackoutDate.getBlackoutDate(), blackoutDates));
			}
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Finished performing BlChangeOrderReturnDateJob...");
			return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), exception,
					"Error occurred while performing BlChangeOrderReturnDateJob");
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}

	}

	/**
	 * @return the blBlackoutDatesDao
	 */
	public BlBlackoutDatesDao getBlBlackoutDatesDao()
	{
		return blBlackoutDatesDao;
	}

	/**
	 * @param blBlackoutDatesDao
	 *           the blBlackoutDatesDao to set
	 */
	public void setBlBlackoutDatesDao(final BlBlackoutDatesDao blBlackoutDatesDao)
	{
		this.blBlackoutDatesDao = blBlackoutDatesDao;
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

	/**
	 * @return the blBlackoutDateService
	 */
	public BlBlackoutDateService getBlBlackoutDateService()
	{
		return blBlackoutDateService;
	}

	/**
	 * @param blBlackoutDateService
	 *           the blBlackoutDateService to set
	 */
	public void setBlBlackoutDateService(final BlBlackoutDateService blBlackoutDateService)
	{
		this.blBlackoutDateService = blBlackoutDateService;
	}

}
