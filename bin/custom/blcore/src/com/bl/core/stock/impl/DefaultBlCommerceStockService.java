package com.bl.core.stock.impl;

import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.store.services.BaseStoreService;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.data.StockResult;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;

/**
 * This class is used to get the inventory for a product
 * @author Moumita
 */
public class DefaultBlCommerceStockService implements BlCommerceStockService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlCommerceStockService.class);
	private BlStockLevelDao blStockLevelDao;
	private BaseStoreService baseStoreService;

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
		final StockLevelStatus stockLevelStatus = setStockLevelStatus(stockResult);
		stockResult.setStockLevelStatus(stockLevelStatus);
		return stockResult;
	}

	/**
	 * This is to set the stock level status of a SKU
	 *
	 * @param stockResult the stockResult object
	 */
	private StockLevelStatus setStockLevelStatus(final StockResult stockResult) {
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
		return stockResult.getStockLevelStatus();
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
		collectAvailableQty(startDate, endDate, stockLevels, availability, totalUnits, productCode);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void collectAvailableQty(final Date startDate, final Date endDate, final Collection<StockLevelModel> stockLevels, 
			final List<Long> availability, final List<Long> totalUnits, final String productCode)
	{
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
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "No Stock Levels found for product : {} and date between : {} and {}",
						productCode, startDate, endDate);
			}
		}
		else
		{
			makeZeroAvailability(availability, totalUnits);
		}
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean isUsedGearSerialNotAssignedToRentalOrder(final String serialProductCode, final String productCode)
	{
		final Date currentDate = new Date();
		return getBlStockLevelDao().isUsedGearSerialNotAssignedToAnyRentalOrders(serialProductCode,
				currentDate,
				BlDateTimeUtils.getNextYearsSameDay());
	}

	/**
	 * This is to get the date in LocalDateTime format
	 *
	 * @param date the date
	 * @return LocalDateTime
	 */
	private LocalDateTime getFormattedDateTime(final Date date) {
		final Instant instant = Instant.ofEpochMilli(date.getTime());
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
	 * {@inheritDoc}
	 */
	@Override
	public Collection<StockLevelModel> getStockForProductCodesAndDate(final List<String> productCodes,
			final List<WarehouseModel> warehouses, final Date startDate, final Date endDate)
	{
		return getBlStockLevelDao().findStockLevelsForProductCodesAndDate(productCodes, warehouses, startDate, endDate);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Date getNextAvailabilityDate(final String productCode, final RentalDateDto rentalDates,
			final Collection<WarehouseModel> warehouses, final int qtyToCheck)
	{
		Date nextAvailableDate = null;
		try
		{
			if (Objects.nonNull(rentalDates))
			{
				final int quantity = qtyToCheck >= 1 ? qtyToCheck : 1;
				final Date newRentalStartDate = BlDateTimeUtils.subtractDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,
						rentalDates.getSelectedFromDate());
				final Date newRentalEndDate = BlDateTimeUtils.addDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,
						rentalDates.getSelectedToDate());
				final Date lastDateToCheck = BlDateTimeUtils.getNextYearsSameDay();
				if (newRentalEndDate.compareTo(lastDateToCheck) < 0)
				{
					final int numberOfDaysToAdd = NumberUtils.toInt(rentalDates.getNumberOfDays()) + 4;
					final Collection<WarehouseModel> lWareHouses = CollectionUtils.isNotEmpty(warehouses) ? warehouses
							: getBaseStoreService().getCurrentBaseStore().getWarehouses();

					nextAvailableDate = checkForNextAvailableDate(productCode, quantity, newRentalStartDate, newRentalEndDate,
							lastDateToCheck, numberOfDaysToAdd, lWareHouses);
				}
				return nextAvailableDate;
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Error while checking next availability for product {} with rental start date - {} and rental end date - {}",
					productCode, rentalDates.getSelectedFromDate(), rentalDates.getSelectedToDate());
		}
		return nextAvailableDate;
	}

	/**
	 * Check for next available date.
	 *
	 * @param productCode
	 *           the product code
	 * @param quantity
	 *           the quantity
	 * @param newRentalStartDate
	 *           the new rental start date
	 * @param newRentalEndDate
	 *           the new rental end date
	 * @param lastDateToCheck
	 *           the last date to check
	 * @param numberOfDaysToAdd
	 *           the number of days to add
	 * @param lWareHouses
	 *           the list of ware houses
	 * @return the date
	 */
	private Date checkForNextAvailableDate(final String productCode, final int quantity, Date newRentalStartDate,
			Date newRentalEndDate, final Date lastDateToCheck, final int numberOfDaysToAdd,
			final Collection<WarehouseModel> lWareHouses)
	{
		Date nextAvailableDate = null;
		Boolean continueCheck = Boolean.TRUE;
		while (nextAvailableDate == null && continueCheck)
		{
			Date checkStockIsAvailable = checkStockIsAvailable(productCode, lWareHouses, newRentalStartDate, newRentalEndDate,
					quantity);
			if (Objects.isNull(checkStockIsAvailable))
			{
				nextAvailableDate = BlDateTimeUtils.addDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,
						BlDateTimeUtils.convertDateToStringDate(newRentalStartDate, BlCoreConstants.DATE_FORMAT));
				continueCheck = Boolean.FALSE;
			}
			else
			{
				newRentalStartDate = checkStockIsAvailable;
				newRentalEndDate = BlDateTimeUtils.addDaysInRentalDates(numberOfDaysToAdd,
						BlDateTimeUtils.convertDateToStringDate(newRentalStartDate, BlCoreConstants.DATE_FORMAT));
				continueCheck = newRentalEndDate.compareTo(lastDateToCheck) < 0;
			}
		}
		return nextAvailableDate;
	}

	/**
	 * Check stock is available. If stock is available for given dates then null object will be returned or else last
	 * available date will be returned.
	 *
	 * @param productCode
	 *           the product code
	 * @param warehouses
	 *           the warehouses
	 * @param newRentalStartDate
	 *           the new rental start date
	 * @param newRentalEndDate
	 *           the new rental end date
	 * @param qtyToCheck
	 *           the qty to check
	 * @return the date
	 */
	private Date checkStockIsAvailable(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date newRentalStartDate, final Date newRentalEndDate, final int qtyToCheck)
	{
		final Collection<StockLevelModel> stockLevels = getStockForDate(productCode, warehouses, newRentalStartDate,
				newRentalEndDate).stream().filter(stockLevel -> !stockLevel.getReservedStatus()).collect(Collectors.toList());
		if (CollectionUtils.isNotEmpty(stockLevels))
		{
			final Map<Date, Long> stockLevelsDatewise = getStockLevelsWithDate(stockLevels);
			return getNextDateIfStockNotAvailable(stockLevelsDatewise, (int) getNumberOfDays(newRentalStartDate, newRentalEndDate),
					qtyToCheck, newRentalStartDate, newRentalEndDate);
		}
		return getNextDate(1, newRentalEndDate);
	}

	/**
	 * Gets the stock levels with date.
	 *
	 * @param stockLevels
	 *           the stock levels
	 * @return the stock levels with date
	 */
	private Map<Date, Long> getStockLevelsWithDate(final Collection<StockLevelModel> stockLevels)
	{
		final Map<Date, Long> stockLevelsDatewise = new TreeMap<>(Collections.reverseOrder());
		stockLevels.stream().collect(Collectors.groupingBy(StockLevelModel::getDate, Collectors.counting())).entrySet().stream()
				.forEach(entry -> stockLevelsDatewise.put(entry.getKey(), entry.getValue()));
		return stockLevelsDatewise;
	}

	/**
	 * Gets the next date if stock not available.
	 *
	 * @param stockLevelsDatewise
	 *           the stock levels datewise
	 * @param numberOfDays
	 *           the number of days
	 * @param qtyToCheck
	 *           the qty to check
	 * @return the next date if stock not available
	 */
	private Date getNextDateIfStockNotAvailable(final Map<Date, Long> stockLevelsDatewise, final int numberOfDays,
			final int qtyToCheck, final Date newRentalStartDate, final Date newRentalEndDate)
	{
		if (stockLevelsDatewise.size() == numberOfDays)
		{
			for (final Map.Entry<Date, Long> entry : stockLevelsDatewise.entrySet())
			{
				if (entry.getValue().intValue() < qtyToCheck)
				{
					return getNextDate(1, entry.getKey());
				}
			}
		}
		else
		{
			final List<Date> missingDatesForStock = getMissingDatesForStock(stockLevelsDatewise, newRentalStartDate,
					newRentalEndDate);
			return getNextDate(1, missingDatesForStock.get(missingDatesForStock.size() - 1));
		}
		return null;
	}

	/**
	 * Gets the missing dates for stock.
	 *
	 * @param stockLevelsDatewise
	 *           the stock levels datewise
	 * @return the missing dates for stock
	 */
	private List<Date> getMissingDatesForStock(final Map<Date, Long> stockLevelsDatewise, final Date newRentalStartDate,
			final Date newRentalEndDate)
	{
		if (stockLevelsDatewise.size() == 1)
		{
			return Lists.newArrayList(stockLevelsDatewise.keySet());
		}
		final List<Date> lAvailableDates = new ArrayList<>(stockLevelsDatewise.keySet());
		final List<Date> lMissingDates = new ArrayList<>();
		final Calendar cal = Calendar.getInstance();
		cal.setTime(newRentalStartDate);
		do
		{
			lMissingDates.add(cal.getTime());
			cal.add(Calendar.DATE, 1);
		}
		while (cal.getTime().before(newRentalEndDate));
		lMissingDates.add(newRentalEndDate);
		lMissingDates.removeAll(lAvailableDates);
		Collections.sort(lMissingDates);
		return lMissingDates;
	}

	/**
	 * Gets the number of days for the given pair of dates.
	 *
	 * @param rentalStartDate
	 *           the rental start date
	 * @param rentalEndDate
	 *           the rental end date
	 * @return the number of days
	 */
	private long getNumberOfDays(final Date rentalStartDate, final Date rentalEndDate)
	{
		final LocalDateTime startDate = getFormattedDateTime(rentalStartDate);
		final LocalDateTime endDate = getFormattedDateTime(rentalEndDate);

		return ChronoUnit.DAYS.between(startDate, endDate.plusDays(1));
	}

	/**
	 * Gets the next date.
	 *
	 * @param numberOfDaysToAdd
	 *           the number of days to add
	 * @param date
	 *           the date
	 * @return the next date
	 */
	private Date getNextDate(final int numberOfDaysToAdd, final Date date)
	{
		return Date.from(getFormattedDateTime(date).plusDays(numberOfDaysToAdd).atZone(ZoneId.systemDefault()).toInstant());
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

	/**
	 * @return the baseStoreService
	 */
	public BaseStoreService getBaseStoreService()
	{
		return baseStoreService;
	}

	/**
	 * @param baseStoreService the baseStoreService to set
	 */
	public void setBaseStoreService(BaseStoreService baseStoreService)
	{
		this.baseStoreService = baseStoreService;
	}

}
