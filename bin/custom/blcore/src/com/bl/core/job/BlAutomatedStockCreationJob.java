package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.stock.BlStockManageService;
import com.bl.logging.BlLogger;


/**
 * This job is responsible for creating the stock for the day which is after a year
 * @author Moumita
 *
 */
public class BlAutomatedStockCreationJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlAutomatedStockCreationJob.class);

	private BlStockManageService blStockManageService;

	/**
	 * It performs to create the stock for a a day
	 * @param cronJob the cron job model instance
	 * @return PerformResult
	 */
	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlAutomatedStockCreationJob...");
		try
		{
			getBlStockManageService().createOneDayStockLevelForAllSkuProducts();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "BlAutomatedStockCreationJob finished successfully");
		}
		catch(final Exception ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while performing BlAutomatedStockCreationJob and "
					+ "the error is {} ", ex);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
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
