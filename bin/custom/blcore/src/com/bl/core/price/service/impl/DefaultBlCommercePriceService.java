package com.bl.core.price.service.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.commerceservices.price.impl.DefaultCommercePriceService;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.jalo.order.price.PriceInformation;

import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.BlProductModel;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.price.strategies.BlProductDynamicPriceStrategy;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;


/**
 * Extended OOTB DefaultCommercePriceService to get the Dynamic Prices for the Rental Products on the basis of the
 * selection of rental days.
 *
 * @author Ravikumar
 *
 */
public class DefaultBlCommercePriceService extends DefaultCommercePriceService implements BlCommercePriceService
{
	private static final Logger LOG = Logger.getLogger(DefaultBlCommercePriceService.class);
	private BlProductDynamicPriceStrategy blProductDynamicPriceStrategy;
	private BlDatePickerService blDatePickerService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public PriceInformation getWebPriceForProduct(final ProductModel product)
	{
		if (PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product))
		{
			validateParameterNotNull(product, "Product model cannot be null");
			final List<PriceInformation> prices = getPriceService().getPriceInformationsForProduct(product);
			if (CollectionUtils.isNotEmpty(prices))
			{
				final PriceInformation defaultPriceInformation = prices.get(0);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Default Price Value is {} for product {}",
						defaultPriceInformation.getPriceValue().getValue(), product.getCode());
				final Long rentalDays = getRentalDays();
				return Objects.nonNull(rentalDays) && isRentalDaysEligible(rentalDays) ? getBlProductDynamicPriceStrategy()
						.getDynamicPriceInformationForProduct((BlProductModel) product, defaultPriceInformation, rentalDays)
						: defaultPriceInformation;
			}
			return null;
		}
		return super.getWebPriceForProduct(product);
	}

	/**
	 * Gets the dynamic price data for product.
	 *
	 * @param isConstrainedProduct
	 *           the is constrained product
	 * @param priceValue
	 *           the price value
	 * @return the dynamic price data for product
	 */
	@Override
	public BigDecimal getDynamicPriceDataForProduct(final Boolean isConstrainedProduct, final Double priceValue)
	{
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Default Price Value is {}", priceValue);
		final Long rentalDays = getRentalDays();
		return Objects.nonNull(rentalDays) && isRentalDaysEligible(rentalDays)
				? getBlProductDynamicPriceStrategy().getDynamicPriceDataForProduct(isConstrainedProduct, priceValue, rentalDays)
				: BigDecimal.valueOf(priceValue);
	}

	/**
	 * Checks if the rental days is eligible for Dynamic pricing.
	 *
	 * @param rentalDays
	 *           the rental days
	 * @return true, if is rental days eligible
	 */
	private boolean isRentalDaysEligible(final Long rentalDays)
	{
		return rentalDays.longValue() >= BlCoreConstants.MINIMUM_RENTAL_DAYS
				&& rentalDays.longValue() <= BlCoreConstants.MAXIMUM_RENTAL_DAYS
				&& rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY;
	}

	/**
	 * Gets the rental days.
	 *
	 * @return the rental days
	 */
	private Long getRentalDays()
	{
		final RentalDateDto rentalDatesFromSession = getBlDatePickerService().getRentalDatesFromSession();
		return Objects.nonNull(rentalDatesFromSession) && StringUtils.isNotBlank(rentalDatesFromSession.getNumberOfDays())
				? Long.valueOf(rentalDatesFromSession.getNumberOfDays())
				: null;
	}


	/**
	 * @return the blProductDynamicPriceStrategy
	 */
	public BlProductDynamicPriceStrategy getBlProductDynamicPriceStrategy()
	{
		return blProductDynamicPriceStrategy;
	}

	/**
	 * @param blProductDynamicPriceStrategy
	 *           the blProductDynamicPriceStrategy to set
	 */
	public void setBlProductDynamicPriceStrategy(final BlProductDynamicPriceStrategy blProductDynamicPriceStrategy)
	{
		this.blProductDynamicPriceStrategy = blProductDynamicPriceStrategy;
	}

	/**
	 * @return the blDatePickerService
	 */
	public BlDatePickerService getBlDatePickerService()
	{
		return blDatePickerService;
	}

	/**
	 * @param blDatePickerService
	 *           the blDatePickerService to set
	 */
	public void setBlDatePickerService(final BlDatePickerService blDatePickerService)
	{
		this.blDatePickerService = blDatePickerService;
	}
}
