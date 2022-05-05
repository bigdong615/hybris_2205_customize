package com.bl.core.stock.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.data.StockResult;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import de.hybris.platform.basecommerce.enums.StockLevelStatus;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class is used to get the inventory for a product
 * @author Moumita
 */
public class DefaultBlCommerceStockService implements BlCommerceStockService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlCommerceStockService.class);
	private BlStockLevelDao blStockLevelDao;
	private BaseStoreService baseStoreService;
	private BlDatePickerService blDatePickerService;
	private static final String STOCK_RESULT_MESSAGE = "Stock Level found for product : {} and date between: {} and {} with "
			+ "total count : {} and available count : {}";

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
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, STOCK_RESULT_MESSAGE, productCode, startDate, endDate, totalUnits, availability);
		} else {
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total and available count of the product {} is {} and {} for the date between {} and {}", productCode, totalUnits, availability, startDate, endDate);
		}
		final StockResult stockResult = new StockResult();
		stockResult.setTotalCount(totalUnits);
		stockResult.setAvailableCount(availability);
		final StockLevelStatus stockLevelStatus = setStockLevelStatus(stockResult);
		stockResult.setStockLevelStatus(stockLevelStatus);
		return stockResult;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public StockResult getStockForBundleProduct(final BlProductModel blProductModel,
			final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate) {
		final Collection<ProductReferenceModel> productReferenceList = blProductModel
				.getProductReferences().stream().filter(
						productReferenceModel -> ProductReferenceTypeEnum.CONSISTS_OF
								.equals(productReferenceModel.getReferenceType())).collect(Collectors.toList());
		final List<Long> availableProductCount = new ArrayList<>();
		final List<Long> totalProductCount = new ArrayList<>();
		// getting available count for all bundle product.
		collectAvailabilityForBundle(productReferenceList, warehouses, startDate, endDate,
				availableProductCount, totalProductCount);
		Long availability = Long.valueOf(0);
		Long totalUnits = Long.valueOf(0);
		if (CollectionUtils.isNotEmpty(totalProductCount) && CollectionUtils
				.isNotEmpty(availableProductCount)) {
			availability = availableProductCount.stream().mapToLong(Long::longValue).min().getAsLong();
			totalUnits = totalProductCount.stream().mapToLong(Long::longValue).min().getAsLong();
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					STOCK_RESULT_MESSAGE, blProductModel.getCode(), startDate,
					endDate, totalProductCount, availableProductCount);
		}
		final StockResult stockResult = new StockResult();
		stockResult.setTotalCount(totalUnits);
		stockResult.setAvailableCount(availability);
		stockResult.setStockLevelStatus(( availability >= 1L  ? StockLevelStatus.INSTOCK :StockLevelStatus.OUTOFSTOCK ));
		return stockResult;
	}

	/**
	 * This Method used for collecting stock for all sku of given bundle.
	 * @param productReferenceList
	 * @param warehouses
	 * @param startDate
	 * @param endDate
	 * @param availableProductCount
	 * @param totalProductCount
	 */
	private void collectAvailabilityForBundle(
			final Collection<ProductReferenceModel> productReferenceList,
			final Collection<WarehouseModel> warehouses, final Date startDate, final Date endDate,
			final List<Long> availableProductCount, final List<Long> totalProductCount) {
		productReferenceList.forEach(productReferenceModel -> {
			final List<Long> availableCount = new ArrayList<>();
			final List<Long> totalCount = new ArrayList<>();
			collectAvailability(startDate, endDate, productReferenceModel.getTarget().getCode(),
					warehouses,
					availableCount, totalCount);
			Long availability = Long.valueOf(0);
			Long totalUnits = Long.valueOf(0);
			if (CollectionUtils.isNotEmpty(totalCount) && CollectionUtils.isNotEmpty(availableCount)) {
				availability = availableCount.stream().mapToLong(Long::longValue).min().getAsLong();
				totalUnits = totalCount.stream().mapToLong(Long::longValue).min().getAsLong();
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
						STOCK_RESULT_MESSAGE,
						productReferenceModel.getTarget().getCode(), startDate, endDate, totalUnits,
						availability);
				final long noOfQuantity = productReferenceModel.getQuantity() != null ? productReferenceModel.getQuantity().longValue() : 1L;
				Long availableProduct = availability.longValue()/noOfQuantity;
				availableProductCount.add(availableProduct);
				totalProductCount.add(totalUnits);
			}
		});
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
				&& (availability > 0 && availability <= BlCoreConstants.LOW_AVAILABILITY))
		{
			resultStatus = StockLevelStatus.LOWSTOCK;
		}
		else if (totalUnits >= BlCoreConstants.MAX_TOTAL && (availability > 0 && availability <= BlCoreConstants.MIN_TOTAL))
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
  public Long getAvailableCountForBundle(final BlProductModel productModel, final Collection<WarehouseModel> warehouses,
      final Date startDate, final Date endDate)
  {
    StockResult stockResult= getStockForBundleProduct(productModel,warehouses,startDate,endDate);
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
	 * This is to get the total as well as available quantity of a SKU for the given date
	 *
	 * @param startDate
	 *           the start date
	 * @param endDate
	 *           the end date
	 * @param stockLevels
	 *           the stock levels
	 * @param availability
	 *           the availability
	 * @param totalUnits
	 *           the total units
	 * @param productCode
	 *           the product code
	 */
	private void collectAvailableQty(final Date startDate, final Date endDate, final Collection<StockLevelModel> stockLevels,
			final List<Long> availability, final List<Long> totalUnits, final String productCode)
	{
		if (CollectionUtils.isNotEmpty(stockLevels))
		{
			final LocalDateTime rentalStartDate = BlDateTimeUtils.getFormattedDateTime(startDate);
			final LocalDateTime rentalEndDate = BlDateTimeUtils.getFormattedDateTime(endDate);
			final long stayDuration = ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate.plusDays(1));
			final Map<Object, List<StockLevelModel>> stockLevelsSerialwise = stockLevels.stream()
					.collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
			final Long totalQty = Long.valueOf(stockLevelsSerialwise.size());
			totalUnits.add(totalQty);
			final AtomicLong availableQty = new AtomicLong();
				stockLevelsSerialwise.forEach((serial, stockLevelModels) -> {
					if (stockLevelModels.size() == stayDuration) {
						if (stockLevelModels.stream().noneMatch(StockLevelModel::getReservedStatus)) {
							availableQty.set(availableQty.incrementAndGet());
						}
						BlLogger.logFormatMessageInfo(LOG, Level.INFO,
								"Total count {} and available count {} for product : {} and date between : {} and {}",
								totalQty, availableQty, productCode, startDate, endDate);
					}
					else
					{
						makeZeroAvailability(availability, totalUnits);
						BlLogger.logFormatMessageInfo(LOG, Level.INFO, "No Stock Levels found for product : {} and date between : {} and {}",
								productCode, startDate, endDate);
					}
				});
				availability.add(availableQty.get());
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
	public boolean isUsedGearSerialNotAssignedToRentalOrder(final String productCode)
	{
		final Date currentDate = new Date();
		return getBlStockLevelDao().isUsedGearSerialNotAssignedToAnyRentalOrders(productCode,
				currentDate,
				BlDateTimeUtils.getNextYearsSameDay());
	}

	/**
	 * This is to get the date in LocalDateTime format
	 *
	 * @param availability the available count
	 * @param totalUnits the total count
	 */
	private void makeZeroAvailability(final List<Long> availability, final List<Long> totalUnits) {
//		availability.add(Long.valueOf(0));
//		totalUnits.add(Long.valueOf(0));
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
	 * This is to get the stock details for a collection of SKUs for the given date range with availability (reserved status as false).
	 *
	 * @param productCodes the product codes
	 * @param warehouse    the warehouse
	 * @param startDate    the start date
	 * @param endDate      the end date
	 * @return Collection<StockLevelModel> The list of stockLevelModels associated to the SKUs
	 */
	@Override
	public Collection<StockLevelModel> getStockForProductCodesAndDate(final Set<String> productCodes,
			final WarehouseModel warehouse, final Date startDate, final Date endDate) {
		final Collection<StockLevelModel> stockLevels = getBlStockLevelDao().findStockLevelsForProductCodesAndDate(
				productCodes, warehouse, startDate, endDate);
		final Map<Object, List<StockLevelModel>> stockLevelsProductWise = stockLevels.stream()
				.collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
		final LocalDateTime rentalStartDate = BlDateTimeUtils.getFormattedDateTime(startDate);
		final LocalDateTime rentalEndDate = BlDateTimeUtils.getFormattedDateTime(endDate);
		final long stayDuration = ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate.plusDays(1));
		final Collection<StockLevelModel> finalStockLevels = new ArrayList<>();
		for(Map.Entry<Object, List<StockLevelModel>> entry : stockLevelsProductWise.entrySet()) {
			if(entry.getValue().size() == stayDuration) {
				finalStockLevels.addAll(entry.getValue());
			} else {
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"No stock found for serial product : {} and date between : {} and {}", entry.getKey(),
						startDate, endDate);
			}
		}
		return finalStockLevels;
	}

	/**
	 * {@inheritDoc}
	 * @param productCodes the list of product code
	 * @param warehouses the list of warehouse
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return map which is product with quantity
	 */
	public Map<String, Long> getStockForUnallocatedProduct(final List<String> productCodes,
			final List<WarehouseModel> warehouses, final Date startDate, final Date endDate) {
		final Collection<StockLevelModel> stockLevels = getBlStockLevelDao().getStockForUnallocatedProduct(productCodes,
				warehouses, startDate, endDate);
		final Map<String, Long> productsWithQty = new HashMap<>();
		final Map<Object, List<StockLevelModel>> stockLevelsProductWise = stockLevels.stream()
				.collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
		for(Map.Entry<Object, List<StockLevelModel>> entry : stockLevelsProductWise.entrySet()) {
				final String productCode = entry.getValue().get(0).getProductCode();
				final Long quantity = ObjectUtils.defaultIfNull(productsWithQty.get(productCode), Long.valueOf(0));
				productsWithQty.put(productCode, quantity + 1);
		}
		return productsWithQty;
	}

	/**
	 * {@inheritDoc}
	 * @param stockLevels the stock levels
	 * @return map of product code and stock level models
	 */
	public Map<String, List<StockLevelModel>> groupBySkuProductWithAvailability(
			final Collection<StockLevelModel> stockLevels) {
		
		Map<String, List<StockLevelModel>> stockLevelsProductWise = new HashMap<>();
		if (CollectionUtils.isNotEmpty(stockLevels)) {
			stockLevelsProductWise = stockLevels.stream()
					.collect(Collectors.groupingBy(StockLevelModel::getProductCode));
		}
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "No Stock Levels found for grouping");
		return stockLevelsProductWise;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, Long> groupByProductsAvailability(final Date startDate, final Date endDate,
			final List<String> lProductCodes, final List<WarehouseModel> warehouses)
	{
		final Collection<StockLevelModel> stockLevelsforProducts = getStockForProductCodesAndDate(lProductCodes, warehouses,
				startDate, endDate);
		final Map<String, Long> stockLevelProductWise = new HashMap<>();
		addEmptyListOfStockIfNotAvailable(startDate, endDate, stockLevelsforProducts, lProductCodes)
				.forEach((productCode, lStockLevels) -> {
					final List<Long> availableCount = new ArrayList<>();
					final List<Long> totalCount = new ArrayList<>();
					collectAvailableQty(startDate, endDate, lStockLevels, availableCount, totalCount, productCode);
					Long availability = Long.valueOf(0);
					if (CollectionUtils.isNotEmpty(availableCount))
					{
						availability = availableCount.stream().mapToLong(Long::longValue).min().getAsLong();
						BlLogger.logFormatMessageInfo(LOG, Level.INFO,
								"{} available stock found for product : {} and date between : {} and {}", availability, productCode,
								startDate, endDate);

					}
					stockLevelProductWise.put(productCode, availability);
				});
		return stockLevelProductWise;
	}

	/**
	 * Adds the empty list of stock level if not available against the product.
	 *
	 * @param startDate
	 *           the start date
	 * @param endDate
	 *           the end date
	 * @param stockLevelsforProducts
	 *           the stock levels for products
	 * @param lProductCodes
	 *           the l product codes
	 * @return the map
	 */
	private Map<String, List<StockLevelModel>> addEmptyListOfStockIfNotAvailable(final Date startDate, final Date endDate,
			final Collection<StockLevelModel> stockLevelsforProducts, final List<String> lProductCodes)
	{
		final Map<String, List<StockLevelModel>> newProductWiseStocks = new HashMap<>();
		final Map<String, List<StockLevelModel>> productWiseStocks = stockLevelsforProducts.stream()
				.collect(Collectors.groupingBy(StockLevelModel::getProductCode));
		lProductCodes.removeIf(productWiseStocks::containsKey);
		if (CollectionUtils.isNotEmpty(lProductCodes))
		{
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "No Stock Levels found for product : {} and date between : {} and {}",
					lProductCodes, startDate, endDate);
		}
		newProductWiseStocks.putAll(productWiseStocks);
		lProductCodes.forEach(productCode -> newProductWiseStocks.put(productCode, Collections.emptyList()));
		return newProductWiseStocks;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getNextAvailabilityDateInCheckout(final String productCode, final RentalDateDto rentalDates,
			final Collection<WarehouseModel> warehouses, final int qtyToCheck)
	{
		final Date nextAvailabilityDate = getNextAvailabilityDate(productCode, rentalDates, warehouses, qtyToCheck);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "For Checkout : Next Available Date for product {} is {}", productCode,
				nextAvailabilityDate);
		if (Objects.nonNull(nextAvailabilityDate))
		{
			final String newAvailableDate = BlDateTimeUtils.convertDateToStringDate(nextAvailabilityDate,
					BlCoreConstants.RENTAL_DATE_FORMAT);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "From Checkout : Until Date for product {} is {}", productCode,
					newAvailableDate);
			return newAvailableDate;
		}
		return StringUtils.EMPTY;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getNextAvailabilityDateInPDP(final String productCode, final RentalDateDto rentalDate)
	{
		final Date nextAvailabilityDate = getNextAvailabilityDate(productCode, rentalDate, null, 1);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "For PDP : Next Available Date for product {} is {}", productCode,
				nextAvailabilityDate);
		if (Objects.nonNull(nextAvailabilityDate))
		{
			final String newAvailableDate = BlDateTimeUtils.convertDateToStringDate(nextAvailabilityDate,
					BlCoreConstants.RENTAL_DATE_FORMAT);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "For PDP : Available Rental Date for product {} is {}", productCode,
					newAvailableDate);
			return newAvailableDate;
		}
		return StringUtils.EMPTY;
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
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Availability check for Qty {} of product {}", quantity, productCode);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"Before adding shipping days to Rental Start Date {} and Rental End Date {}", rentalDates.getSelectedFromDate(),
						rentalDates.getSelectedToDate());
				final Date lastDateToCheck = BlDateTimeUtils.getFormattedStartDay(BlDateTimeUtils.getNextYearsSameDay()).getTime();
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Last Date to check : {}", lastDateToCheck);
				final List<Date> blackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
				final Date newRentalStartDate = BlDateTimeUtils.subtractDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,
						rentalDates.getSelectedFromDate(), blackOutDates);
				final Date newRentalEndDate = BlDateTimeUtils.getRentalEndDate(blackOutDates, rentalDates, lastDateToCheck);				
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"After adding shipping days. New Rental Start Date {} and new Rental End Date {}", newRentalStartDate,
						newRentalEndDate);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Last Date to check {}", lastDateToCheck);
				if (newRentalEndDate.compareTo(lastDateToCheck) <= 0)
				{
					final int numberOfDaysToAdd = NumberUtils.toInt(rentalDates.getNumberOfDays()) + 4;
					final Collection<WarehouseModel> lWareHouses = CollectionUtils.isNotEmpty(warehouses) ? warehouses
							: getBaseStoreService().getCurrentBaseStore().getWarehouses();

					nextAvailableDate = checkForNextAvailableDate(productCode, quantity, newRentalStartDate, newRentalEndDate,
							lastDateToCheck, numberOfDaysToAdd, lWareHouses);
					BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Next Available Date {}", nextAvailableDate);
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
	 * Checks and get the next available date for the product.
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
		return getNextDateIfStockNotAvailable(productCode, lWareHouses, quantity, newRentalStartDate,
					newRentalEndDate, lastDateToCheck, numberOfDaysToAdd);
	}

	/**
	 * Gets the stock levels grouped by dates in reverse order.
	 *
	 * @param stockLevels
	 *           the stock levels
	 * @param numberOfDays
	 * @return the stock levels with date
	 */
	private Long getStockLevels(final Collection<StockLevelModel> stockLevels, final int numberOfDays)
	{
		final Map<Object, List<StockLevelModel>> stockLevelsSerialwise = stockLevels.stream()
				.collect(Collectors.groupingBy(StockLevelModel::getSerialProductCode));
		final AtomicLong availableQty = new AtomicLong();
		stockLevelsSerialwise.forEach((serial, stockLevelModels) -> {
			if (stockLevelModels.size() == numberOfDays) {
					availableQty.set(availableQty.incrementAndGet());
			}
		});
		return availableQty.get();
	}

	/**
	 * Gets the next date if stock not available for any particular date.
	 * @param qtyToCheck
	 *           the qty to check
	 * @param lastDateToCheck
	 * @param numberOfDaysToAdd
	 * @return the next date if stock not available
	 */
	private Date getNextDateIfStockNotAvailable(final String productCode,
			final Collection<WarehouseModel> warehouses,
			final int qtyToCheck, final Date newRentalStartDate,
			final Date newRentalEndDate, final Date lastDateToCheck, final int numberOfDaysToAdd)
	{
		final List<Date> listOfBlackOutDates = getBlDatePickerService().getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Blackout Dates : {}", listOfBlackOutDates);
		for (Date startDate = newRentalStartDate, endDate = newRentalEndDate; endDate.compareTo(lastDateToCheck) <= 0;
				startDate = getNextDate(1, startDate, listOfBlackOutDates), endDate = BlDateTimeUtils
						.addDaysInRentalDates(numberOfDaysToAdd, BlDateTimeUtils.convertDateToStringDate(startDate, BlCoreConstants.DATE_FORMAT), listOfBlackOutDates)) {
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Next Dates to check availability is {} and {}", startDate,
					endDate);
			final Collection<StockLevelModel> stockLevelModels = getStockForDate(productCode, warehouses, startDate,
					endDate).stream().filter(stockLevel -> !stockLevel.getReservedStatus()).collect(Collectors.toList());
			final int numberOfDays = (int) getNumberOfDays(startDate, endDate);
			final Long availableQty = getStockLevels(stockLevelModels, numberOfDays);
			BlLogger.logFormatMessageInfo(LOG, Level.INFO,
				"Available Stock dates map size : {} and number of days : {} and dates are : {} and {} for qty : {}",
				availableQty, numberOfDays, startDate, endDate, qtyToCheck);
			if (availableQty.intValue() >= qtyToCheck)
			{
				return BlDateTimeUtils.addDaysInRentalDates(BlCoreConstants.SKIP_TWO_DAYS,
						BlDateTimeUtils.convertDateToStringDate(startDate, BlCoreConstants.DATE_FORMAT), listOfBlackOutDates);
			}
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
		final List<Date> startAndEndDateList = new ArrayList<>();
		final List<Date> lMissingDates = new ArrayList<>();
		final Calendar cal = Calendar.getInstance();
		cal.setTime(newRentalStartDate);
		do
		{
			startAndEndDateList.add(cal.getTime());
			cal.add(Calendar.DATE, 1);
		}
		while (cal.getTime().before(newRentalEndDate));
		startAndEndDateList.add(newRentalEndDate);
		startAndEndDateList.forEach(missingDate -> {
			if(lAvailableDates.stream().noneMatch(date -> DateUtils.isSameDay(date, missingDate)))
			{
				lMissingDates.add(missingDate);
			}
		});
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
		final LocalDateTime startDate = BlDateTimeUtils.getFormattedDateTime(rentalStartDate);
		final LocalDateTime endDate = BlDateTimeUtils.getFormattedDateTime(rentalEndDate);

		return ChronoUnit.DAYS.between(startDate, endDate.plusDays(1));
	}


	/**
	 * This method created to get stock based on serial products from existing order
	 *
	 */
	@Override
	public StockResult getStockForEntireExtendDuration(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate)
	{
		final List<Long> availableCount = new ArrayList<>();
		final List<Long> totalCount = new ArrayList<>();
		collectAvailabilityForExtend(startDate, endDate, productCode, warehouses, availableCount, totalCount);
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
	 * This method created for collecting stock availability for extend rental duration
	 */
	protected void collectAvailabilityForExtend(final Date startDate, final Date endDate, final String productCode,
			final Collection<WarehouseModel> warehouses, final List<Long> availability, final List<Long> totalUnits)
	{
		final Collection<StockLevelModel> stockLevels = getStockForExtendDate(productCode, warehouses,
				startDate, endDate);
		collectAvailableQty(startDate, endDate, stockLevels, availability, totalUnits, productCode);
	}

	/**
	 * This method created to get stock for extend rental products
	 * @param productCode the product code
	 * @param warehouses the warehouse
	 * @param startDate the start date
	 * @param endDate the end date
	 * @return list of stock level model
	 */
	public Collection<StockLevelModel> getStockForExtendDate(final String productCode, final Collection<WarehouseModel> warehouses,
			final Date startDate, final Date endDate)
	{
		return getBlStockLevelDao().findSerialStockLevelForExtendDate(productCode, warehouses, startDate, endDate);
	}

	/**
	 * Gets the next date by adding number of days.
	 *
	 * @param numberOfDaysToAdd
	 *           the number of days to add
	 * @param date
	 *           the date
	 * @return the next date
	 */
	private Date getNextDate(final int numberOfDaysToAdd, final Date date, final List<Date> listOfBlackOutDates)
	{
		return BlDateTimeUtils.addDaysInRentalDates(numberOfDaysToAdd,
				BlDateTimeUtils.convertDateToStringDate(date, BlCoreConstants.DATE_FORMAT), listOfBlackOutDates);
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

	/**
	 * @return the blDatePickerService
	 */
	public BlDatePickerService getBlDatePickerService()
	{
		return blDatePickerService;
	}

	/**
	 * @param blDatePickerService the blDatePickerService to set
	 */
	public void setBlDatePickerService(BlDatePickerService blDatePickerService)
	{
		this.blDatePickerService = blDatePickerService;
	}

}
