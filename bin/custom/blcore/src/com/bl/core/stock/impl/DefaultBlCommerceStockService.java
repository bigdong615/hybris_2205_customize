package com.bl.core.stock.impl;

import com.bl.core.data.StockResult;
import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;


/**
 * @author Moumita
 * This class is used to get the inventory for a product
 */
public class DefaultBlCommerceStockService implements BlCommerceStockService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlCommerceStockService.class);
	private BlStockLevelDao blStockLevelDao;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<StockLevelModel> getStockForDate(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate)
	{
		return getBlStockLevelDao().findStockLevelForDate(productCode, warehouses, startDate, endDate);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public StockResult getStockForEntireDuration(final String productCode, final Collection<WarehouseModel> warehouses,
			final LocalDateTime startDate, LocalDateTime endDate)
	{
		final List<Long> availableCount = new ArrayList<>();
		final List<Long> totalCount = new ArrayList<>();
		collectAvailability(startDate, endDate, productCode, warehouses, availableCount, totalCount);
		Long availability = Long.valueOf(0);
		Long totalUnits = Long.valueOf(0);
		if (CollectionUtils.isNotEmpty(totalCount) && CollectionUtils.isNotEmpty(availableCount)) {
			availability = availableCount.stream().mapToLong(Long::longValue).min().getAsLong();
			totalUnits = totalCount.stream().mapToLong(Long::longValue).min().getAsLong();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Stock Level found for product : {} and date between: {} and {} with "
					+ "total count : {} and avaiable count : {}", productCode, startDate, endDate, totalUnits, availability);
		}
		StockResult stockResult = new StockResult();
		stockResult.setTotalCount(totalUnits);
		stockResult.setAvailableCount(availability);
		return stockResult;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public StockLevelStatus getStockLevelStatus(final Collection<WarehouseModel> warehouses, final String productCode,
			final LocalDateTime startDate, final LocalDateTime endDate) {
		StockResult stockResult = getStockForEntireDuration(productCode, warehouses, startDate, endDate);
		Long totalUnits = stockResult.getTotalCount();
		Long availability = stockResult.getAvailableCount();
		StockLevelStatus resultStatus;
		if (totalUnits >= BlCoreConstants.MIN_TOTAL && totalUnits < BlCoreConstants.MAX_TOTAL
				&& availability <= BlCoreConstants.LOW_AVAILABILITY)
		{
			resultStatus = StockLevelStatus.LOWSTOCK;
		}
		else if (totalUnits >= BlCoreConstants.MAX_TOTAL && availability <= BlCoreConstants.MIN_TOTAL)
		{
			resultStatus = StockLevelStatus.LOWSTOCK;
		}
		else if (availability <= BlCoreConstants.ZERO_AVAILABILITY)
		{
			resultStatus = StockLevelStatus.OUTOFSTOCK;
		} else {
			resultStatus = StockLevelStatus.INSTOCK;
		}
		return resultStatus;
	}

	/**
	 * This is to get the total as well as available quantity of a SKU for the given date
	 *
	 * @param startDate the start date
	 * @param endDate the end date
	 * @param productCode the product code
	 * @param warehouses the list of warehouse associated to base store
	 * @param availability the available quantity
	 * @param totalUnits the total quantity
	 * @return StockLevelStatus
	 */
	protected void collectAvailability(final LocalDateTime startDate, final LocalDateTime endDate, final String productCode,
			final Collection<WarehouseModel> warehouses, final List<Long> availability, final List<Long> totalUnits)
	{
		final Date convertedStartDate = Date.from(startDate.atZone(ZoneId.systemDefault()).toInstant());
		final Date convertedEndDate = Date.from(endDate.atZone(ZoneId.systemDefault()).toInstant());
		final Collection<StockLevelModel> stockLevels = getStockForDate(productCode, warehouses,
				convertedStartDate, convertedEndDate);
		if (CollectionUtils.isNotEmpty(stockLevels))
		{
			final Map<Object, List<StockLevelModel>> stockLevelsDatewise = stockLevels.stream()
					.collect(Collectors.groupingBy(stockLevel -> stockLevel.getDate()));
			stockLevelsDatewise.forEach((date, stockLevelModels) -> {
				final Long reservedQty = stockLevelModels.stream().filter((StockLevelModel::getReservedStatus)).count();
				final Long totalQty = Long.valueOf(stockLevelModels.size());
				final Long availableQty = totalQty - reservedQty;
				availability.add(availableQty);
				totalUnits.add(totalQty);
		});
		}
		else
		{
			availability.add(Long.valueOf(0));
			totalUnits.add(Long.valueOf(0));
		}
	}


	/**
	 * @return the blStockLevelDao
	 */
	public BlStockLevelDao getBlStockLevelDao()
	{
		return blStockLevelDao;
	}

	/**
	 * @param blStockLevelDao
	 *           the blStockLevelDao to set
	 */
	public void setBlStockLevelDao(final BlStockLevelDao blStockLevelDao)
	{
		this.blStockLevelDao = blStockLevelDao;
	}

}
