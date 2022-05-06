package com.bl.core.job;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.cronjob.enums.CronJobResult;
import de.hybris.platform.cronjob.enums.CronJobStatus;
import de.hybris.platform.cronjob.model.CronJobModel;
import de.hybris.platform.order.strategies.impl.EventPublishingSubmitOrderStrategy;
import de.hybris.platform.servicelayer.cronjob.AbstractJobPerformable;
import de.hybris.platform.servicelayer.cronjob.PerformResult;

import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.order.dao.BlOrderDao;
import com.bl.logging.BlLogger;


/**
 * This cronjob will serve for allocation of future orders for Migrated Orders only.
 *
 * @author Ravikumar
 *
 */
public class BlFutureOrderAllocationJob extends AbstractJobPerformable<CronJobModel>
{
	private static final Logger LOG = Logger.getLogger(BlFutureOrderAllocationJob.class);

	private BlOrderDao blOrderDao;
	private EventPublishingSubmitOrderStrategy eventPublishingSubmitOrderStrategy;

	@Override
	public PerformResult perform(final CronJobModel cronJobModel)
	{
		final List<OrderModel> allLegacyOrders = getBlOrderDao().getAllLegacyOrders(); //fetching all legacy orders
		if (CollectionUtils.isEmpty(allLegacyOrders))
		{
			BlLogger.logMessage(LOG, Level.INFO, "BlFutureOrderAllocationJob : perform : No Legacy Rental Orders Found");
			return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
		}

		//filtering fetched legacy order with future rental dates
		final List<OrderModel> futureLegacyRentlOrders = allLegacyOrders.stream().filter(order -> {
			if (Objects.isNull(order.getRentalStartDate()))
			{
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"BlFutureOrderAllocationJob : perform : Rental Start Date is Null on Legacy Rental Order with code : {}",
						order.getCode());
				return false;
			}
			return isEligibleForFutureAllocation(order);
		}).collect(Collectors.toList());
		//performing submission of order for allocation process.
		futureLegacyRentlOrders.forEach(legacyOrder -> {
			try
			{
				getEventPublishingSubmitOrderStrategy().submitOrder(legacyOrder);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"BlFutureOrderAllocationJob : perform : Submitted order for allocation through cronjob for Legacy Rental Order with code : {}",
						legacyOrder.getCode());
			}
			catch (final Exception exception)
			{
				BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception, "Error occured for order with code : {}",
						legacyOrder.getCode());
			}
		});
		return new PerformResult(CronJobResult.SUCCESS, CronJobStatus.FINISHED);
	}

	/**
	 * Checks if order is eligible for future allocation.
	 *
	 * @param order
	 *           the order
	 * @return true, if is eligible for future allocation
	 */
	private boolean isEligibleForFutureAllocation(final OrderModel order)
	{
		final Date currentDate = new Date();
		return (DateUtils.isSameDay(currentDate, order.getRentalStartDate())
				|| order.getRentalStartDate().compareTo(currentDate) > 0) && (CollectionUtils.isEmpty(order.getConsignments()));
	}

	/**
	 * @return the blOrderDao
	 */
	public BlOrderDao getBlOrderDao()
	{
		return blOrderDao;
	}

	/**
	 * @param blOrderDao
	 *           the blOrderDao to set
	 */
	public void setBlOrderDao(final BlOrderDao blOrderDao)
	{
		this.blOrderDao = blOrderDao;
	}

	/**
	 * @return the eventPublishingSubmitOrderStrategy
	 */
	public EventPublishingSubmitOrderStrategy getEventPublishingSubmitOrderStrategy()
	{
		return eventPublishingSubmitOrderStrategy;
	}

	/**
	 * @param eventPublishingSubmitOrderStrategy
	 *           the eventPublishingSubmitOrderStrategy to set
	 */
	public void setEventPublishingSubmitOrderStrategy(final EventPublishingSubmitOrderStrategy eventPublishingSubmitOrderStrategy)
	{
		this.eventPublishingSubmitOrderStrategy = eventPublishingSubmitOrderStrategy;
	}

}
