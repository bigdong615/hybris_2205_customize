package com.bl.core.job;

import com.bl.core.usedGearCart.clean.BlUsedGearCartCleanService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is to remove the abandoned used gear carts
 * @author Moumita
 */
public class BlUsedGearCartAbandonmentJob extends AbstractJobPerformable<CronJobModel> {

	private static final Logger LOG = Logger.getLogger(BlUsedGearCartAbandonmentJob.class);

	private BlUsedGearCartCleanService blUsedGearCartCleanService;

	/**
	 * It is to remove the abandoned used gear carts
	 * @param cronJob cron job instance
	 * @return PerformResult the cron job result
	 */
	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlUsedGearCartAbandonmentJob...");
		try
		{
			getBlUsedGearCartCleanService().cleanUsedGearAbandonedCart();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "BlUsedGearCartAbandonmentJob finished successfully");
		}
		catch(final Exception ex)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex,
					"Error occurred while performing BlUsedGearCartAbandonmentJob");
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	public BlUsedGearCartCleanService getBlUsedGearCartCleanService() {
		return blUsedGearCartCleanService;
	}

	public void setBlUsedGearCartCleanService(
			BlUsedGearCartCleanService blUsedGearCartCleanService) {
		this.blUsedGearCartCleanService = blUsedGearCartCleanService;
	}
}
