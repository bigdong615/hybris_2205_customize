package com.bl.Ordermanagement.job;

import com.bl.Ordermanagement.reshuffler.service.BlReshufflerService;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * It is responsible for allocating the unallocated products which are going to be shipped
 * in next two days
 * @author Moumita
 */
public class BlReshufflerJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlReshufflerJob.class);
	private BlReshufflerService blReshufflerService;

	@Autowired
	private BlStockService blStockService;

	@Override
	public PerformResult perform(final CronJobModel blReshufflerJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlReshufflerJob...");
		try
		{
			getBlReshufflerService().processIncompleteOrders();
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BlReshufflerJob finished successfully");
		}
		catch (final Exception ex)
		{
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Error occurred while performing BlReshufflerJob {} ", ex.getMessage());
			BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while performing BlReshufflerJob", ex);
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	public BlReshufflerService getBlReshufflerService() {
		return blReshufflerService;
	}

	public void setBlReshufflerService(BlReshufflerService blReshufflerService) {
		this.blReshufflerService = blReshufflerService;
	}

}