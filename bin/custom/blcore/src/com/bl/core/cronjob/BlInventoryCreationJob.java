package com.bl.core.cronjob;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import java.util.ArrayList;
import java.util.Date;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlInventoryCreationCronJobModel;
import com.bl.core.stock.BlInventoryManageService;
import com.bl.logging.BlLogger;


/**
 * This cronjob will create the stock
 *
 * @author Moumita
 */
public class BlInventoryCreationJob extends AbstractJobPerformable<BlInventoryCreationCronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlInventoryCreationJob.class);

	private BlInventoryManageService blInventoryManageService;

	/**
	 * It runs to create the stock level
	 *
	 * @param blInvCreationCronJob
	 * @return PerformResult
	 */
	@Override
	public PerformResult perform(final BlInventoryCreationCronJobModel blInvCreationCronJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing AccommodationInventoryJob...");
		createStockLevels(blInvCreationCronJob);
		blInvCreationCronJob.setStartDate(null);
		blInvCreationCronJob.setEndDate(null);
		blInvCreationCronJob.setSkuProductList(new ArrayList<>());
		this.modelService.save(blInvCreationCronJob);
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	/**
	 * It creates the stock level as per the given parameters in the cron job
	 *
	 * @param blInvCreationCronJob
	 */
	private void createStockLevels(final BlInventoryCreationCronJobModel blInvCreationCronJob)
	{
		final Date startDate = blInvCreationCronJob.getStartDate();
		final Date endDate = blInvCreationCronJob.getEndDate();
		if (null != startDate && null != endDate)
		{
			if (CollectionUtils.isNotEmpty(blInvCreationCronJob.getSkuProductList()))
			{
				Date fromDate = startDate;
				final Date toDate = DateUtils.addDays(endDate, 1);
				while (fromDate.before(toDate))
				{
					getBlInventoryManageService().createStockLevelForGivenSkus(blInvCreationCronJob.getSkuProductList(), fromDate);
					fromDate = DateUtils.addDays(fromDate, 1);
				}
			}
			else
			{
				Date fromDate = startDate;
				final Date toDate = DateUtils.addDays(endDate, 1);
				while (fromDate.before(toDate))
				{
					getBlInventoryManageService().createStockLevelForAllSkus(fromDate);
					fromDate = DateUtils.addDays(fromDate, 1);
				}
			}
		}
		else if (null == startDate && null == endDate && CollectionUtils.isEmpty(blInvCreationCronJob.getSkuProductList()))
		{
			getBlInventoryManageService().createStockLevelForADayForAllSkus();
		}
		else
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "AccommodationInventoryJob is not performed...");
		}
	}

	/**
	 * @return the blInventoryManageService
	 */
	public BlInventoryManageService getBlInventoryManageService()
	{
		return blInventoryManageService;
	}

	/**
	 * @param blInventoryManageService
	 *           the blInventoryManageService to set
	 */
	public void setBlInventoryManageService(final BlInventoryManageService blInventoryManageService)
	{
		this.blInventoryManageService = blInventoryManageService;
	}

}
