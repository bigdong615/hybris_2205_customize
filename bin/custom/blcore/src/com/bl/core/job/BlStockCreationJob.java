package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.exceptions.BusinessException;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlStockCreationCronJobModel;
import com.bl.core.stock.BlStockService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;


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
			getBlStockService().createStockLevelForSkuProductsByDate(filterSubparts(blStockCreationCronJob.getSkuProductList()),
					blStockCreationCronJob.getStartDate(), blStockCreationCronJob.getEndDate());
		}
		catch (final BusinessException ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex);
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		catch(final Exception ex)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(), ex,
					"Error occurred while performing BlStockCreationJob");
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		resetParameters(blStockCreationCronJob);
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	/**
	 * @param skuProductList
	 */
	private List<BlProductModel> filterSubparts(final List<BlProductModel> skuProductList)
	{
		final List<BlProductModel> finalProductList = new ArrayList<BlProductModel>();
		for(final BlProductModel product : skuProductList) {
			if(product instanceof BlProductModel && product.getProductType()!=null && !product.getProductType().getCode().equalsIgnoreCase("SUBPARTS")) {
				finalProductList.add(product);
			}
		}
		return finalProductList;
	}

	/**
	 * It resets the parameters once the cron job is successfully run
	 *
	 * @param blStockCreationCronJob
	 */
	private void resetParameters(final BlStockCreationCronJobModel blStockCreationCronJob)
	{
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
