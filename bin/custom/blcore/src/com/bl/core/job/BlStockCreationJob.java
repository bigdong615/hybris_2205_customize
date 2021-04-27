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
import com.bl.core.stock.BlStockManageService;
import com.bl.logging.BlLogger;


/**
 * This cron job will create the stock based on start date, end date and skuProductList
 *
 * @author Moumita
 */
public class BlStockCreationJob extends AbstractJobPerformable<BlStockCreationCronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlStockCreationJob.class);

	private BlStockManageService blStockManageService;

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
			final Date startDate = blStockCreationCronJob.getStartDate();
			final Date endDate = blStockCreationCronJob.getEndDate();
			getBlStockManageService().createStockLevelForSkuProductsByDate(blStockCreationCronJob.getSkuProductList(),
					startDate, endDate);
		}
		catch(final Exception ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while performing BlStockCreationJob and "
					+ "the error is {} ", ex);
		}
		resetTheParameters(blStockCreationCronJob);
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	/**
	 * It resets the parameters once the cron job is successfully run
	 *
	 * @param blStockCreationCronJob
	 */
	private void resetTheParameters(BlStockCreationCronJobModel blStockCreationCronJob) {
		blStockCreationCronJob.setStartDate(null);
		blStockCreationCronJob.setEndDate(null);
		blStockCreationCronJob.setSkuProductList(new ArrayList<>());
		this.modelService.save(blStockCreationCronJob);
	}

	/**
	 * @return the blStockManageService
	 */
	public BlStockManageService getBlStockManageService()
	{
		return blStockManageService;
	}

	/**
	 * @param blStockManageService
	 *           the blStockManageService to set
	 */
	public void setBlStockManageService(final BlStockManageService blStockManageService)
	{
		this.blStockManageService = blStockManageService;
	}


}
