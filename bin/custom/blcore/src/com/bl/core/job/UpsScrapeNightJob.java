/**
 *
 */
package com.bl.core.job;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import java.util.List;

import javax.xml.parsers.ParserConfigurationException;

import com.bl.core.esp.service.impl.DefaultBlOrderFeedFTPService;
import com.bl.core.order.dao.BlOrderDao;


/**
 *
 */
public class UpsScrapeNightJob extends AbstractJobPerformable<CronJobModel>
{

	private BlOrderDao orderDao;
	private DefaultBlOrderFeedFTPService defaultBlOrderFeedFTPService;

	@Override
	public PerformResult perform(final CronJobModel job)
	{
		List<AbstractOrderModel> orderModelList = null;
		orderModelList = getOrderDao().getOrdersForOrderFeedToFTP();
		try
		{
			getDefaultBlOrderFeedFTPService().convertOrderTOFeed(orderModelList);
		}
		catch (final ParserConfigurationException e)
		{
			e.printStackTrace();
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

	public DefaultBlOrderFeedFTPService getDefaultBlOrderFeedFTPService()
	{
		return defaultBlOrderFeedFTPService;
	}

	public void setDefaultBlOrderFeedFTPService(final DefaultBlOrderFeedFTPService defaultBlOrderFeedFTPService)
	{
		this.defaultBlOrderFeedFTPService = defaultBlOrderFeedFTPService;
	}

}
