package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;


/**
 * This job is responsible for marking the serial as dirty priority for the serials which are ready to ship on same day
 *
 * @author Namrata
 *
 */
public class BlMarkDirtyPrioritySerialsJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlMarkDirtyPrioritySerialsJob.class);

	private BlInventoryScanToolService blInventoryScanToolService;

	/**
	 * It performs to mark dirty priority on serials for orders going on same day
	 *
	 * @param cronJob
	 *           the cron job model instance
	 * @return PerformResult
	 */
	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlMarkDirtyPrioritySerialsJob...");
		try
		{
			getBlInventoryScanToolService().flagAllDirtyPrioritySerialsOfConsignment();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "BlMarkDirtyPrioritySerialsJob finished successfully");
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), exception,
					"Error occurred while performing BlMarkDirtyPrioritySerialsJob");
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	public BlInventoryScanToolService getBlInventoryScanToolService()
	{
		return blInventoryScanToolService;
	}

	public void setBlInventoryScanToolService(final BlInventoryScanToolService blInventoryScanToolService)
	{
		this.blInventoryScanToolService = blInventoryScanToolService;
	}

}
