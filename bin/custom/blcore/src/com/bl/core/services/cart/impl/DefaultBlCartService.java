package com.bl.core.services.cart.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;

import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commerceservices.order.CommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.impl.DefaultCartService;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Default implementation of the {@link BlCartService}.
 *
 * @author Neeraj Singh
 */
public class DefaultBlCartService extends DefaultCartService implements BlCartService {

  private static final Logger LOGGER = Logger.getLogger(DefaultBlCartService.class);

  private CommerceCartService commerceCartService;
  
  private CommerceCartCalculationStrategy commerceCartCalculationStrategy;

	private BlCommerceStockService blCommerceStockService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void clearCartEntries() {

    final CartModel cartModel = getSessionCart();

    if (CollectionUtils.isNotEmpty(cartModel.getEntries())) {

      final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
      commerceCartParameter.setEnableHooks(true);
      commerceCartParameter.setCart(cartModel);
      getCommerceCartService().removeAllEntries(commerceCartParameter);

      BlLogger.logFormattedMessage(LOGGER, Level.DEBUG, BlCoreConstants.EMPTY_STRING,
          "All entries removed from cart with code : {}", cartModel.getCode());
    }
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
	public void resetCartCalculationFlag()
	{
		final CartModel cartModel = getSessionCart();
		if (CollectionUtils.isNotEmpty(cartModel.getEntries()))
		{
			cartModel.getEntries().forEach(entry -> {
				entry.setCalculated(Boolean.FALSE);
				getModelService().save(entry);
			});
		}
		cartModel.setCalculated(Boolean.FALSE);
		getModelService().save(cartModel);
		getModelService().refresh(cartModel);
	}

  /**
   * {@inheritDoc}
   */
	@Override
	public void recalculateCartIfRequired()
	{
		final CartModel cartModel = getSessionCart();
		if (BooleanUtils.isFalse(cartModel.getCalculated()))
		{
			final CommerceCartParameter parameter = getCommerceCartParameter(cartModel);
			getCommerceCartCalculationStrategy().recalculateCart(parameter);
		}
	}

	 /**
	   * {@inheritDoc}
	   */
	@Override
	public void updateCartEntryDamageWaiver(final long entryNumber, final String damageWaiverType)
	{
		final CartModel cartModel = getSessionCart();
		final Integer cartEntryNumber = Integer.valueOf((int) entryNumber);
		if (CollectionUtils.isNotEmpty(cartModel.getEntries()))
		{
			final AbstractOrderEntryModel cartEntryModel = cartModel.getEntries().stream()
					.filter(cartEntry -> cartEntryNumber.equals(cartEntry.getEntryNumber())).findFirst().orElse(null);
			checkAndSetFlagForSelectedDamageWaiver(cartEntryModel, damageWaiverType);
			cartModel.setCalculated(Boolean.FALSE);
			getModelService().save(cartEntryModel);
			getModelService().save(cartModel);
			final CommerceCartParameter parameter = getCommerceCartParameter(cartModel);
			getCommerceCartCalculationStrategy().recalculateCart(parameter);
		}
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setRentalDatesOnCart(final Date rentalStartDate, final Date rentalEndDate)
	{
		final CartModel cartModel = getSessionCart();
		final String cartCode = cartModel.getCode();
		cartModel.setRentalStartDate(rentalStartDate);
		cartModel.setRentalEndDate(rentalEndDate);
		try 
		{			
			getModelService().save(cartModel);
			BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, "Setting Rental Start Date: {} and End Date: {} on Cart: {}", rentalStartDate, rentalEndDate, cartCode);
		}
		catch(final Exception exception)
		{
			BlLogger.logFormattedMessage(LOGGER, Level.ERROR, StringUtils.EMPTY, exception, 
					"Error while saving rental Start Date: {} and End Date: {} on cart - {}", rentalStartDate, rentalEndDate, cartCode);
		}
	}

	/**
	 * Gets the commerce cart parameter.
	 *
	 * @param cartModel the cart model
	 * @return the commerce cart parameter
	 */
	private CommerceCartParameter getCommerceCartParameter(final CartModel cartModel)
	{
		final CommerceCartParameter parameter = new CommerceCartParameter();
		parameter.setCart(cartModel);
		parameter.setEnableHooks(Boolean.TRUE);
		return parameter;
	}

	/**
	 * Check and set flag for selected damage Waiver.
	 *
	 * @param cartEntryModel the cart entry model
	 * @param damageWaiverType the damage Waiver type
	 */
	private void checkAndSetFlagForSelectedDamageWaiver(final AbstractOrderEntryModel cartEntryModel,
			final String damageWaiverType)
	{
		switch (damageWaiverType)
		{
			case BlCoreConstants.GEAR_GUARD_PRO_FULL:
				setFlags(cartEntryModel, Boolean.TRUE, Boolean.FALSE, Boolean.FALSE);
				break;
			case BlCoreConstants.GEAR_GUARD:
				setFlags(cartEntryModel, Boolean.FALSE, Boolean.TRUE, Boolean.FALSE);
				break;
			case BlCoreConstants.NO_GEAR_GUARD:
				setFlags(cartEntryModel, Boolean.FALSE, Boolean.FALSE, Boolean.TRUE);
				break;
			default:
				break;
		}
	}

	/**
	 * Sets the flags for Damage Waiver.
	 *
	 * @param cartEntryModel the cart entry model
	 * @param gearGuardProFullWaiverSelected the gear Guard pro full waiver selected
	 * @param gearGuardWaiverSelected the gear Guard waiver selected
	 * @param noGearGuardWaiverSelected the no gear Guard waiver selected
	 */
	private void setFlags(final AbstractOrderEntryModel cartEntryModel, final Boolean gearGuardProFullWaiverSelected,
			final Boolean gearGuardWaiverSelected, final Boolean noGearGuardWaiverSelected)
	{
		cartEntryModel.setGearGuardProFullWaiverSelected(gearGuardProFullWaiverSelected);
		cartEntryModel.setGearGuardWaiverSelected(gearGuardWaiverSelected);
		cartEntryModel.setNoDamageWaiverSelected(noGearGuardWaiverSelected);
		cartEntryModel.setCalculated(Boolean.FALSE);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, Long> checkAvailabilityForRentalCart(final CartData cartData, final List<WarehouseModel> warehouses,
			final RentalDateDto rentalDatesFromSession)
	{
		final List<String> lProductCodes = new ArrayList<>();
		cartData.getEntries().forEach(entry -> lProductCodes.add(entry.getProduct().getCode()));
		final Date startDate = BlDateTimeUtils.getDate(rentalDatesFromSession.getSelectedFromDate(), BlCoreConstants.DATE_FORMAT);
		final Date endDate = BlDateTimeUtils.getDate(rentalDatesFromSession.getSelectedToDate(), BlCoreConstants.DATE_FORMAT);
		final Collection<StockLevelModel> stockForProductCodesAndDate = getBlCommerceStockService()
				.getStockForProductCodesAndDate(lProductCodes, warehouses, startDate, endDate);
		return groupByProductsAvailability(stockForProductCodesAndDate, startDate, endDate, lProductCodes);

	}

	/**
	 * Group by products availability.
	 *
	 * @param stockLevels
	 *           the stock levels
	 * @param startDate
	 *           the start date
	 * @param endDate
	 *           the end date
	 * @param lProductCodes
	 *           the l product codes
	 * @return the map
	 */
	private Map<String, Long> groupByProductsAvailability(final Collection<StockLevelModel> stockLevels, final Date startDate,
			final Date endDate, final List<String> lProductCodes)
	{
		final Map<String, Long> stockLevelProductWise = new HashMap<>();
		makeZeroQtyIfNotAvailable(stockLevels, lProductCodes).forEach((productCode, lStockLevels) -> {
			final List<Long> availableCount = new ArrayList<>();
			final List<Long> totalCount = new ArrayList<>();
			getBlCommerceStockService().collectAvailableQty(startDate, endDate, lStockLevels, availableCount, totalCount,
					productCode);
			Long availability = Long.valueOf(0);
			Long totalUnits = Long.valueOf(0);
			if (CollectionUtils.isNotEmpty(totalCount) && CollectionUtils.isNotEmpty(availableCount))
			{
				availability = availableCount.stream().mapToLong(Long::longValue).min().getAsLong();
				totalUnits = totalCount.stream().mapToLong(Long::longValue).min().getAsLong();
			}
			stockLevelProductWise.put(productCode, availability);
		});
		return stockLevelProductWise;
	}

	/**
	 * Make zero qty if not available.
	 *
	 * @param stockLevels
	 *           the stock levels
	 * @param lProductCodes
	 *           the l product codes
	 * @return the map
	 */
	private Map<String, List<StockLevelModel>> makeZeroQtyIfNotAvailable(final Collection<StockLevelModel> stockLevels,
			final List<String> lProductCodes)
	{
		final Map<String, List<StockLevelModel>> newProductWiseStocks = new HashMap<>();
		final Map<String, List<StockLevelModel>> productWiseStocks = stockLevels.stream()
				.collect(Collectors.groupingBy(stockLevel -> stockLevel.getProductCode()));
		lProductCodes.removeIf(productCode -> productWiseStocks.containsKey(productCode));
		newProductWiseStocks.putAll(productWiseStocks);
		lProductCodes.forEach(productCode -> newProductWiseStocks.put(productCode, Collections.emptyList()));
		return newProductWiseStocks;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getNextAvailabilityDate(final String productCode, final RentalDateDto rentalDates,
			final Collection<WarehouseModel> warehouses, final int qtyToCheck)
	{
		final Date nextAvailabilityDate = getBlCommerceStockService().getNextAvailabilityDate(productCode, rentalDates, warehouses,
				qtyToCheck);
		if (Objects.nonNull(nextAvailabilityDate))
		{
			final Date newAvailableDate = Date
					.from(getFormattedDateTime(nextAvailabilityDate).minusDays(1).atZone(ZoneId.systemDefault()).toInstant());
			return BlDateTimeUtils.convertDateToStringDate(newAvailableDate, BlCoreConstants.RENTAL_DATE_FORMAT);
		}
		return StringUtils.EMPTY;
	}

	/**
	 * Gets the formatted date time.
	 *
	 * @param date
	 *           the date
	 * @return the formatted date time
	 */
	private LocalDateTime getFormattedDateTime(final Date date)
	{
		final Instant instant = Instant.ofEpochMilli(date.getTime());
		return LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
	}

  public CommerceCartService getCommerceCartService() {
    return commerceCartService;
  }

  public void setCommerceCartService(CommerceCartService commerceCartService) {
    this.commerceCartService = commerceCartService;
  }
  
  /**
	 * @return the commerceCartCalculationStrategy
	 */
	public CommerceCartCalculationStrategy getCommerceCartCalculationStrategy()
	{
		return commerceCartCalculationStrategy;
	}

	/**
	 * @param commerceCartCalculationStrategy the commerceCartCalculationStrategy to set
	 */
	public void setCommerceCartCalculationStrategy(final CommerceCartCalculationStrategy commerceCartCalculationStrategy)
	{
		this.commerceCartCalculationStrategy = commerceCartCalculationStrategy;
	}

	/**
	 * @return the blCommerceStockService
	 */
	public BlCommerceStockService getBlCommerceStockService()
	{
		return blCommerceStockService;
	}

	/**
	 * @param blCommerceStockService the blCommerceStockService to set
	 */
	public void setBlCommerceStockService(BlCommerceStockService blCommerceStockService)
	{
		this.blCommerceStockService = blCommerceStockService;
	}

}
