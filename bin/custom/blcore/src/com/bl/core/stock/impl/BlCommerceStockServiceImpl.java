package com.bl.core.stock.impl;

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
import java.util.Objects;

import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import java.util.stream.Stream;
import org.apache.commons.collections.CollectionUtils;


/**
 * @author Moumita
 * This class is used to get the inventory for a product
 */
public class BlCommerceStockServiceImpl implements BlCommerceStockService
{
	private BlStockLevelDao blStockLevelDao;
	private static final long MAX_TOTAL = 20L;
	private static final long MIN_TOTAL = 5L;
	private static final long LOW_AVAILABILITY = 0L;
	private static final long ZERO_AVAILABILITY = 0L;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<StockLevelModel> getStockForDate(final String productCode, final Collection<WarehouseModel> warehouses, final Date date)
	{
		return getBlStockLevelDao().findStockLevelForDate(productCode, warehouses, date);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public StockLevelStatus getStockLevelStatus(final Collection<WarehouseModel> warehouses, final String productCode,
			final LocalDateTime startDate, final LocalDateTime endDate) {
		final List<Long> availableCount = new ArrayList<>();
		final List<Long> totalCount = new ArrayList<>();
		Stream.iterate(startDate, date -> date.plusDays(1))
				.limit(ChronoUnit.DAYS.between(startDate, endDate)).forEach(
				date -> collectAvailability(date, productCode, warehouses, availableCount, totalCount));
		Long availability = Long.valueOf(0);
		Long totalUnits = Long.valueOf(0);
		if (CollectionUtils.isNotEmpty(totalCount) && CollectionUtils.isNotEmpty(availableCount)) {
			availability = availableCount.stream().mapToLong(Long::longValue).min().getAsLong();
			totalUnits = totalCount.stream().mapToLong(Long::longValue).min().getAsLong();
		}
		StockLevelStatus resultStatus;
		if (totalUnits >= MIN_TOTAL && totalUnits < MAX_TOTAL && availability <= LOW_AVAILABILITY) {
			resultStatus = StockLevelStatus.LOWSTOCK;
		} else if(totalUnits >= MAX_TOTAL && availability <= MIN_TOTAL) {
			resultStatus = StockLevelStatus.LOWSTOCK;
		} else if(availability <= ZERO_AVAILABILITY){
			resultStatus = StockLevelStatus.OUTOFSTOCK;
		} else {
			resultStatus = StockLevelStatus.INSTOCK;
		}
		return resultStatus;
	}

	/**
	 * This is to get the total as well as available quantity of a SKU for the given date
	 *
	 * @param date the date
	 * @param productCode the product code
	 * @param warehouses the list of warehouse associated to base store
	 * @param availability the available quantity
	 * @param totalUnits the total quantity
	 * @return StockLevelStatus
	 */
	protected void collectAvailability(final LocalDateTime date, final String productCode,
			final Collection<WarehouseModel> warehouses, final List<Long> availability, final List<Long> totalUnits)
	{
		final Date convertedDate = Date.from(date.atZone(ZoneId.systemDefault()).toInstant());
		final Collection<StockLevelModel> stockLevels = getStockForDate(productCode, warehouses,
				convertedDate);
		if (CollectionUtils.isNotEmpty(stockLevels))
		{
			final Long reservedQty = stockLevels.stream().filter((StockLevelModel::getReservedStatus)).count();
			final Long totalQty = Long.valueOf(stockLevels.size());
			final Long availableQty = totalQty-reservedQty;
			availability.add(availableQty);
			totalUnits.add(totalQty);
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
