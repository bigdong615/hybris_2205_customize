package com.bl.core.price.strategies;

import de.hybris.platform.jalo.order.price.PriceInformation;

import java.math.BigDecimal;

import com.bl.core.model.BlProductModel;


/**
 * The Interface BlProductDynamicPriceStrategy.
 *
 * @author Ravikumar
 */
public interface BlProductDynamicPriceStrategy
{

	/**
	 * Gets the dynamic price information for product.
	 *
	 * @param blProductModel
	 *           the bl product model
	 * @param priceInformation
	 *           the price information
	 * @param selectedNumberOfDays
	 *           the selected number of days
	 * @return the dynamic price information for product
	 */
	public PriceInformation getDynamicPriceInformationForProduct(final BlProductModel blProductModel,
			final PriceInformation priceInformation, final Long selectedNumberOfDays);

	/**
	 * Gets the dynamic price data for product.
	 *
	 * @param isConstrainedProduct
	 *           the is constrained product
	 * @param priceValue
	 *           the price value
	 * @param selectedNumberOfDays
	 *           the selected number of days
	 * @return the dynamic price data for product
	 */
	public BigDecimal getDynamicPriceDataForProduct(final Boolean isConstrainedProduct, final Double priceValue,
			final Long selectedNumberOfDays);
}
