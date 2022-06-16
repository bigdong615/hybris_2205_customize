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
import com.bl.core.model.BlPullOrdersQueueModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.queries.dao.impl.BlPulledQueriesDaoImpl;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;

public class BlPulledOrdersQueueJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlPulledOrdersQueueJob.class);
	private BlPulledQueriesDaoImpl pulledQueriesDao;
	private ModelService modelService;

	@Override
	public PerformResult perform(final CronJobModel cronJob)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Start performing BlPulledOrdersQueueJob...");
		try
		{
			final List<BlPullOrdersQueueModel> lisofAllOrdersQueue = getPulledQueriesDao().ordersQueue();
			getModelService().removeAll(lisofAllOrdersQueue);
			final List<ConsignmentModel> consignments = getPulledQueriesDao().pulledOrdersQueue();

			for (final ConsignmentModel consignment : consignments)
			{
				final BlPullOrdersQueueModel pulledOrdersQueueJob = getModelService().create(BlPullOrdersQueueModel.class);
				if (consignment.getStatus() != null)
				{
					pulledOrdersQueueJob.setStatus(consignment.getStatus().toString());
				}
				if (consignment.getOrder() != null)
				{
					pulledOrdersQueueJob.setOptimizedShippingStartDate(consignment.getOptimizedShippingStartDate());
					pulledOrdersQueueJob.setCode(consignment.getOrder().getCode());
					pulledOrdersQueueJob.setWarehouse(consignment.getWarehouse());

				}
				for (final ConsignmentEntryModel entry : consignment.getConsignmentEntries())
				{
					final List<BlProductModel> serialProducts = entry.getSerialProducts();
					for (final BlProductModel product : serialProducts)
					{
						if (product instanceof BlSerialProductModel)
						{
							final BlSerialProductModel serialProduct = (BlSerialProductModel) product;
							pulledOrdersQueueJob.setSerialProducts(product.getCode());
							pulledOrdersQueueJob.setOcLocation(serialProduct.getOcLocation());

						}
					}
				}
				modelService.save(pulledOrdersQueueJob);
			}

			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BlPulledOrdersQueueJob finished successfully");
		}

		catch(final Exception ex)
		{
			BlLogger.logMessage(LOG, Level.ERROR, LogErrorCodeEnum.CRONJOB_ERROR.getCode(),
					"Error occurred while performing BlPulledOrdersQueueJob", ex);
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
