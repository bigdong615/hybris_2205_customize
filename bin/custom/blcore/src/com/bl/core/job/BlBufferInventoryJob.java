package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.bufferinventory.service.BlBufferInventoryService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;


/**
 * This job is responsible to manage the buffer inventory of SKU products
 * @author Moumita
 */
public class BlBufferInventoryJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlBufferInventoryJob.class);

	private BlBufferInventoryService blBufferInventoryService;

	@Override
	public PerformResult perform(final CronJobModel arg0)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlBufferInventoryJob...");
		try
		{
			getBlBufferInventoryService().updateBufferInventory();
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BlBufferInventoryJob finished successfully");
		}
		catch (final Exception ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
					"Error occurred while performing BlBufferInventoryJob", ex);
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	/**
	 * @return the blBufferInventoryService
	 */
	public BlBufferInventoryService getBlBufferInventoryService()
	{
		return blBufferInventoryService;
	}

	/**
	 * @param blBufferInventoryService
	 *           the blBufferInventoryService to set
	 */
	public void setBlBufferInventoryService(final BlBufferInventoryService blBufferInventoryService)
	{
		this.blBufferInventoryService = blBufferInventoryService;
	}

}
