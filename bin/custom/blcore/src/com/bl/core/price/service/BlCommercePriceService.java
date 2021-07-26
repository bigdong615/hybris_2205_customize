package com.bl.core.price.service;

import de.hybris.platform.commerceservices.price.CommercePriceService;

import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
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

	/**
	 * Gets the dynamic price data for product.
	 *
	 * @param isConstrainedProduct
	 *           the is constrained product
	 * @param priceValue
	 *           the price value
	 * @param rentalDays
	 *         the rentalDays
	 * @return the dynamic price data for product
	 */
	public BigDecimal getDynamicPriceDataForProduct(final Boolean isConstrainedProduct, final Double priceValue, final Long rentalDays);

	/**
	 * Gets the dynamic price data for for extend rental product.
	 * @param product product
	 * @param rentalDays number of rental days
	 * @return the dynamic price data for product
	 */
	public PriceInformation getWebPriceForExtendProduct(final ProductModel product , final Long rentalDays);

}
