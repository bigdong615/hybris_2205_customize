package com.bl.core.job;

import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.List;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlPulledOrdersAwaitingQueueModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.queries.dao.impl.BlPulledQueriesDaoImpl;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;

public class BlPullOrdersAwaitingQueueJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlPullOrdersAwaitingQueueJob.class);
	private BlPulledQueriesDaoImpl pulledQueriesDao;
	private ModelService modelService;

	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlPullOrdersAwaitingQueueJob...");
		try
		{
			final List<ConsignmentModel> consignments = getPulledQueriesDao().pullOrdersAwaitingQueue();

			for (final ConsignmentModel consignment : consignments)
			{
				final BlPulledOrdersAwaitingQueueModel pullOrdersAwaitingQueue = getModelService()
						.create(BlPulledOrdersAwaitingQueueModel.class);
				if (consignment.getStatus() != null)
				{
					pullOrdersAwaitingQueue.setStatus(consignment.getStatus().toString());
				}
				if (consignment.getOrder() != null)
				{
					pullOrdersAwaitingQueue.setOptimizedShippingStartDate(consignment.getOptimizedShippingStartDate());
					pullOrdersAwaitingQueue.setCode(consignment.getOrder().getCode());
					pullOrdersAwaitingQueue.setWarehouse(consignment.getWarehouse());

				}
				for (final ConsignmentEntryModel entry : consignment.getConsignmentEntries())
				{
					final List<BlProductModel> serialProducts = entry.getSerialProducts();
					for (final BlProductModel product : serialProducts)
					{
						if (product instanceof BlSerialProductModel)
						{
							final BlSerialProductModel serialProduct = (BlSerialProductModel) product;
							pullOrdersAwaitingQueue.setSerialProducts(product.getCode());
							pullOrdersAwaitingQueue.setOcLocation(serialProduct.getOcLocation());

						}
					}
				}
				modelService.save(pullOrdersAwaitingQueue);
			}

			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BlPullOrdersAwaitingQueueJob finished successfully");
		}

		catch(final Exception ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
					"Error occurred while performing BlPullOrdersAwaitingQueueJob", ex);
			return new PerformResult(CronJobResult.FAILURE, CronJobStatus.FINISHED);
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	public BlPulledQueriesDaoImpl getPulledQueriesDao()
	{
		return pulledQueriesDao;
	}

	public void setPulledQueriesDao(final BlPulledQueriesDaoImpl pulledQueriesDao)
	{
		this.pulledQueriesDao = pulledQueriesDao;
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	@Override
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

}
