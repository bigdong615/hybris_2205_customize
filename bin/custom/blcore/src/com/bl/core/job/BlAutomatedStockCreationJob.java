package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;


/**
 * This job is responsible for creating the stock for the day which is after a year
 * @author Moumita
 *
 */
public class BlAutomatedStockCreationJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlAutomatedStockCreationJob.class);

	private BlStockService blStockService;

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
			getBlStockService().createOneDayStockLevelForAllSkuProducts();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "BlAutomatedStockCreationJob finished successfully");
		}
		catch(final Exception ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while performing BlAutomatedStockCreationJob and "
					+ "the error is {} ", ex.getMessage(), ex);
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
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
