package com.bl.core.price.strategies.impl;

import de.hybris.platform.europe1.jalo.PriceRow;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.util.PriceValue;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.core.price.strategies.BlProductDynamicPriceStrategy;
import com.bl.core.services.pricingratio.BlPricingRatioService;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;


/**
 * This class generates the Dynamic price of the product on the basis of number of rental days selected by user, for
 * Constrained or Standard Products. To get the dynamic price for product, the product must be a Rental Product.
 *
 * @author Ravikumar
 *
 */
public class DefaultBlProductDynamicPriceStrategy implements BlProductDynamicPriceStrategy
{
	private static final Logger LOG = Logger.getLogger(DefaultBlProductDynamicPriceStrategy.class);
	private BlPricingRatioService blPricingRatioService;

	/**
	 * Gets the dynamic price information for rental product for the selected number of rental days.
	 *
	 * @param blProductModel
	 *           the bl product model
	 * @param priceInformation
	 *           the price information
	 * @param selectedNumberOfDays
	 *           the selected number of days
	 * @return the dynamic price information for product
	 */
	@Override
	public PriceInformation getDynamicPriceInformationForProduct(final BlProductModel blProductModel,
			final PriceInformation priceInformation, final Long selectedNumberOfDays)
	{
		try
		{
			final BigDecimal sevenDayPrice = BigDecimal.valueOf(priceInformation.getPriceValue().getValue());
			return BooleanUtils.isTrue(blProductModel.getConstrained())
					? createNewPriceInformation(priceInformation, getConstrainedDynamicPrice(sevenDayPrice, selectedNumberOfDays))
					: createNewPriceInformation(priceInformation, getStandardDynamicPrice(sevenDayPrice, selectedNumberOfDays));
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, LogErrorCodeEnum.DYNAMIC_PRICING_ERROR.getCode(), exception,
					"Error while calculating dynamic price for Product with code : {}", blProductModel.getCode());
			return priceInformation;
		}

	}

	/**
	 * Gets the dynamic price data for rental product on the basis of selected number of rental days.
	 *
	 * @param isConstrainedProduct
	 *           the is constrained product
	 * @param priceValue
	 *           the price value
	 * @param selectedNumberOfDays
	 *           the selected number of days
	 * @return the dynamic price data for product
	 */
	@Override
	public BigDecimal getDynamicPriceDataForProduct(final Boolean isConstrainedProduct, final Double priceValue,
			final Long selectedNumberOfDays)
	{
		return BooleanUtils.isTrue(isConstrainedProduct)
				? getConstrainedDynamicPrice(BigDecimal.valueOf(priceValue), selectedNumberOfDays)
				: getStandardDynamicPrice(BigDecimal.valueOf(priceValue), selectedNumberOfDays);
	}

	/**
	 * Gets the constrained dynamic price.
	 *
	 * @param priceValue
	 *           the price value
	 * @param selectedNumberOfDays
	 *           the selected number of days
	 * @return the constrained dynamic price
	 */
	private BigDecimal getConstrainedDynamicPrice(final BigDecimal priceValue, final Long selectedNumberOfDays)
	{
		final Map<Integer, Double> constrainedRatiosMap = getConstrainedRatiosMap(
				getBlPricingRatioService().getConstrainedPricingRatio());

		return calculateDynamicPriceforRentalDays(constrainedRatiosMap, selectedNumberOfDays,
				BigDecimal.valueOf(priceValue.longValue()));
	}

	/**
	 * Gets the standard dynamic price.
	 *
	 * @param priceValue
	 *           the price value
	 * @param selectedNumberOfDays
	 *           the selected number of days
	 * @return the standard dynamic price
	 */
	private BigDecimal getStandardDynamicPrice(final BigDecimal priceValue, final Long selectedNumberOfDays)
	{
		final Map<Integer, Double> standardRatiosMap = getStandardRatiosMap(getBlPricingRatioService().getStandardPricingRatio());

		return calculateDynamicPriceforRentalDays(standardRatiosMap, selectedNumberOfDays,
				BigDecimal.valueOf(priceValue.longValue()));
	}


	/**
	 * Calculates the dynamic price for rental days on the basis of pricing ratio and selected rental days.
	 *
	 * @param ratioMap
	 *           the ratio map
	 * @param rentalDays
	 *           the rental days
	 * @param sevenDayPrice
	 *           the seven day price
	 * @return the big decimal
	 */
	private BigDecimal calculateDynamicPriceforRentalDays(final Map<Integer, Double> ratioMap, final Long rentalDays,
			final BigDecimal sevenDayPrice)
	{
		int lowestDuration = 0;
		int highestDuration = 0;
		final int noOfDays = rentalDays.intValue();

		//if the ratio duration matched with the duration we have in DB then returning the dynamic price for the particular durations price ratio
		if (ratioMap.containsKey(noOfDays))
		{
			return (sevenDayPrice.multiply(BigDecimal.valueOf(ratioMap.get(noOfDays)))).setScale(BlCoreConstants.DECIMAL_PRECISION,
					BlCoreConstants.ROUNDING_MODE);
		}
		//Sort those durations which we are getting from keys
		final Set<Integer> sortedDurationsKey = ratioMap.keySet().stream().sorted()
				.collect(Collectors.toCollection(LinkedHashSet::new));
		//find nearest lowest duration and nearest highest duration
		for (final Integer duration : sortedDurationsKey)
		{
			if (duration.intValue() < noOfDays)
			{
				lowestDuration = duration.intValue();
			}
			else
			{
				highestDuration = duration.intValue();
				break;
			}
		}
		//calculating the price for lowest duration
		final BigDecimal lowestPrice = (sevenDayPrice.multiply(BigDecimal.valueOf(ratioMap.get(lowestDuration))))
				.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
		//calculating the price for highest duration
		final BigDecimal highestPrice = (sevenDayPrice.multiply(BigDecimal.valueOf(ratioMap.get(highestDuration))))
				.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
		//getting the differences between the highest and the lowest durations
		final int diffInDurations = highestDuration - lowestDuration;
		//calculating the differences between the highest price and the lowest price
		final BigDecimal diffInPrice = highestPrice.subtract(lowestPrice);
		//calculating per day price from differences in price and duration
		final BigDecimal perDayPrice = diffInPrice.divide(BigDecimal.valueOf(diffInDurations),
				new MathContext(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
		//returning the calculated dynamic price for the selected rental days
		return lowestPrice.add((perDayPrice.multiply(BigDecimal.valueOf(Math.subtractExact(noOfDays, lowestDuration))))
				.setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
	}

	/**
	 * Gets the constrained price ratios map.
	 *
	 * @param <T>
	 *
	 * @param ratioList
	 *           the ratio list
	 * @return the constrained ratios map
	 */
	private Map<Integer, Double> getConstrainedRatiosMap(final List<BlConstrainedPricingRatioModel> ratioList)
	{
		return CollectionUtils.isNotEmpty(ratioList)
				? ratioList.stream().collect(
						Collectors.toMap(ratio -> Integer.parseInt(ratio.getDuration().getCode()), ratio -> ratio.getPricingRatio()))
				: Collections.emptyMap();
	}

	/**
	 * Gets the standard price ratios map.
	 *
	 * @param ratioList
	 *           the ratio list
	 * @return the standard ratios map
	 */
	private Map<Integer, Double> getStandardRatiosMap(final List<BlStandardPricingRatioModel> ratioList)
	{
		return CollectionUtils.isNotEmpty(ratioList)
				? ratioList.stream().collect(
						Collectors.toMap(ratio -> Integer.parseInt(ratio.getDuration().getCode()), ratio -> ratio.getPricingRatio()))
				: Collections.emptyMap();
	}

	/**
	 * Creates the new price information.
	 *
	 * @param defaultPriceInformation
	 *           the default price information
	 * @param priceForNumberOfDays
	 *           the price for number of days
	 * @return the price information
	 */
	private PriceInformation createNewPriceInformation(final PriceInformation defaultPriceInformation,
			final BigDecimal priceForNumberOfDays)
	{
		final PriceRow priceRow = ((PriceRow) defaultPriceInformation.getQualifiers().get(BlCoreConstants.PRICE_ROW));
		final String isoCode = priceRow.getCurrency().getIsocode();
		final boolean netAsPrimitive = priceRow.isNetAsPrimitive();
		final PriceValue newPricevalue = new PriceValue(isoCode, priceForNumberOfDays.doubleValue(), netAsPrimitive);
		return new PriceInformation(defaultPriceInformation.getQualifiers(), newPricevalue);
	}


	/**
	 * Gets the bl pricing ratio service.
	 *
	 * @return the blPricingRatioService
	 */
	public BlPricingRatioService getBlPricingRatioService()
	{
		return blPricingRatioService;
	}

	/**
	 * Sets the bl pricing ratio service.
	 *
	 * @param blPricingRatioService
	 *           the blPricingRatioService to set
	 */
	public void setBlPricingRatioService(final BlPricingRatioService blPricingRatioService)
	{
		this.blPricingRatioService = blPricingRatioService;
	}

}
