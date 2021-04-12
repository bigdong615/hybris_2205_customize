package com.bl.core.price.service;

import de.hybris.platform.commerceservices.price.CommercePriceService;

import java.math.BigDecimal;


/**
 * BL Custom Commerce Price Service.
 *
 * @author Ravikumar
 *
 */
public interface BlCommercePriceService extends CommercePriceService
{

	/**
	 * Gets the dynamic price data for product.
	 *
	 * @param isConstrainedProduct
	 *           the is constrained product
	 * @param priceValue
	 *           the price value
	 * @return the dynamic price data for product
	 */
	public BigDecimal getDynamicPriceDataForProduct(final Boolean isConstrainedProduct, final Double priceValue);
}
