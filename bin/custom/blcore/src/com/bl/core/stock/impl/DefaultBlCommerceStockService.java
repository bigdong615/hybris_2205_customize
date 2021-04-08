package com.bl.core.stock.impl;

import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.data.StockResult;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.logging.BlLogger;


/**
 * This class is used to get the inventory for a product
 * @author Moumita
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
			final Date startDate, final Date endDate)
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
		final StockResult stockResult = new StockResult();
		stockResult.setTotalCount(totalUnits);
		stockResult.setAvailableCount(availability);
		return stockResult;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Long getAvailableCount(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate)
	{
		final StockResult stockResult = getStockForEntireDuration(productCode, warehouses, startDate, endDate);
		return stockResult.getAvailableCount();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public StockLevelStatus getStockLevelStatus(final Collection<WarehouseModel> warehouses, final String productCode,
			final Date startDate, final Date endDate) {
		final StockResult stockResult = getStockForEntireDuration(productCode, warehouses, startDate, endDate);
		final Long totalUnits = stockResult.getTotalCount();
		final Long availability = stockResult.getAvailableCount();
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
	 */
	protected void collectAvailability(final Date startDate, final Date endDate, final String productCode,
			final Collection<WarehouseModel> warehouses, final List<Long> availability, final List<Long> totalUnits)
	{
		final Collection<StockLevelModel> stockLevels = getStockForDate(productCode, warehouses,
				startDate, endDate);
		if (CollectionUtils.isNotEmpty(stockLevels))
		{
			final Map<Object, List<StockLevelModel>> stockLevelsDatewise = stockLevels.stream()
					.collect(Collectors.groupingBy(stockLevel -> stockLevel.getDate()));
			final LocalDateTime rentalStartDate = getFormattedDateTime(startDate);
			final LocalDateTime rentalEndDate = getFormattedDateTime(endDate);
			final long stayDuration = ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate.plusDays(1));
			final Set<Object> datesPresentInStockTable = stockLevelsDatewise.keySet();
			//This is to check whether stock for any particular day is missing in inventory table
			if(datesPresentInStockTable.size() == stayDuration) {
				stockLevelsDatewise.forEach((date, stockLevelModels) -> {
					final Long reservedQty = stockLevelModels.stream()
							.filter((StockLevelModel::getReservedStatus)).count();
					final Long totalQty = Long.valueOf(stockLevelModels.size());
					final Long availableQty = totalQty - reservedQty;
					availability.add(availableQty);
					totalUnits.add(totalQty);
				});
			}
			else {
				makeZeroAvailability(availability, totalUnits);
			}
		}
		else
		{
			makeZeroAvailability(availability, totalUnits);
		}
	}

	/**
	 * This is to get the date in LocalDateTime format
	 *
	 * @param date the date
	 * @return LocalDateTime
	 */
	private LocalDateTime getFormattedDateTime(Date date) {
		Instant instant = Instant.ofEpochMilli(date.getTime());
		return LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
	}

	/**
	 * This is to get the date in LocalDateTime format
	 *
	 * @param availability the available count
	 * @param totalUnits the total count
	 */
	private void makeZeroAvailability(final List<Long> availability, final List<Long> totalUnits) {
		availability.add(Long.valueOf(0));
		totalUnits.add(Long.valueOf(0));
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
