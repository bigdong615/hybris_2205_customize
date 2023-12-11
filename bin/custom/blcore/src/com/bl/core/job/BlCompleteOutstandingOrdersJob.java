package com.bl.core.job;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.List;
import java.util.Map;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.order.dao.BlOrderDao;
import com.bl.logging.BlLogger;


/**
 * @author Kumar
 *
 */
public class BlCompleteOutstandingOrdersJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlCompleteOutstandingOrdersJob.class);

	private BlOrderDao orderDao;
	private ModelService modelService;

	@Override
	public PerformResult perform(final CronJobModel arg0)
	{
		List<OrderModel> orderModelList = null;
		try
		{
			orderModelList = getOrderDao().getOutStandCompletedOrders();
		}
		catch (final Exception e)
		{
			e.printStackTrace();
		}
		if (!orderModelList.isEmpty())
		{
			for (final OrderModel order : orderModelList)
			{
				final boolean isValid;
				boolean changestatus = false;
				boolean updaterequired = true;

				for (final ConsignmentModel c : order.getConsignments())
				{
					for (final ConsignmentEntryModel ce : c.getConsignmentEntries())
					{
						for (final Map.Entry<String, ItemStatusEnum> item : ce.getItems().entrySet())
						{
							if (item.getKey() != null && item.getKey().matches("[0-9]+") && item.getValue() != null)
							{
								if (item.getValue().equals(ItemStatusEnum.RECEIVED_OR_RETURNED)
										|| item.getValue().equals(ItemStatusEnum.NOT_INCLUDED))
								{
									changestatus = true;
								}
								else
								{
									updaterequired = false;
								}
							}
						}
					}
				}
				if (updaterequired)
				{
					order.setStatus(OrderStatus.COMPLETED);
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BlCompleteOutstandingOrdersJob orders {} ", order.getCode());

					getModelService().save(order);
					for (final ConsignmentModel c : order.getConsignments())
					{
						c.setStatus(ConsignmentStatus.COMPLETED);
						getModelService().save(c);
					}
				}
			}
		}
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);

	}

	public BlOrderDao getOrderDao()
	{
		return orderDao;
	}

	public void setOrderDao(final BlOrderDao orderDao)
	{
		this.orderDao = orderDao;
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
