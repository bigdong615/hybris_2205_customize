package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import java.util.ArrayList;
import java.util.Date;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlStockCreationCronJobModel;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;


/**
 * This cron job will create the stocks based on start date, end date and skuProductList
 *
 * @author Moumita
 */
public class BlStockCreationJob extends AbstractJobPerformable<BlStockCreationCronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlStockCreationJob.class);

	private BlStockService blStockService;

	/**
	 * It creates the stock level
	 *
	 * @param blStockCreationCronJob
	 * @return PerformResult
	 */
	@Override
	public PerformResult perform(final BlStockCreationCronJobModel blStockCreationCronJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlStockCreationJob...");
		try
		{
			createStockLevels(blStockCreationCronJob);
		}
		catch(final Exception ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while performing BlStockCreationJob and "
					+ "the error is {} ", ex);
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		resetTheParameters(blStockCreationCronJob);
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	/**
	 * It creates the stocks based on start date, end date and skuProductList
	 *
	 * @param blStockCreationCronJob
	 * @throws Exception
	 */
	private void createStockLevels(final BlStockCreationCronJobModel blStockCreationCronJob) throws Exception
	{
		final Date startDate = blStockCreationCronJob.getStartDate();
		final Date endDate = blStockCreationCronJob.getEndDate();
		if (null != startDate && null != endDate)
		{
			getBlStockService().createStockLevelForSkuProductsByDate(blStockCreationCronJob.getSkuProductList(), startDate, endDate);
		}
		else
		{
			throw new Exception("Start and end date can not be null");
		}
	}

	/**
	 * It resets the parameters once the cron job is successfully run
	 *
	 * @param blStockCreationCronJob
	 */
	private void resetTheParameters(final BlStockCreationCronJobModel blStockCreationCronJob) {
		blStockCreationCronJob.setStartDate(null);
		blStockCreationCronJob.setEndDate(null);
		blStockCreationCronJob.setSkuProductList(new ArrayList<>());
		this.modelService.save(blStockCreationCronJob);
	}

	/**
	 * @return the blStockService
	 */
	public BlStockService getBlStockService()
	{
		return blStockService;
	}

	/**
	 * @param blStockService
	 *           the blStockService to set
	 */
	public void setBlStockService(final BlStockService blStockService)
	{
		this.blStockService = blStockService;
	}

}
