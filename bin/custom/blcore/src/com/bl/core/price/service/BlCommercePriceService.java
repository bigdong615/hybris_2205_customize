package com.bl.core.price.service;

import de.hybris.platform.commerceservices.price.CommercePriceService;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.order.exceptions.CalculationException;
import de.hybris.platform.util.PriceValue;
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
	 * @return the dynamic price data for product
	 */
	public BigDecimal getDynamicPriceDataForBundleProduct(final Boolean isConstrainedProduct, final ProductModel product);

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

	PriceInformation getWebPriceForTax(final ProductModel product , final AbstractOrderModel abstractOrderModel);

	/**
	 * Gets dynamic price data for rental bundle product.
	 * @param product product
	 * @return the dynamic price data for bundle product
	 */
	 PriceInformation getWebPriceForBundleProduct(final ProductModel product);

	/**
	 * Gets dynamic price data for rental bundle product for given rental days.
	 * @param product product
	 * @param noOfRentalDays rental days
	 * @return price
	 * @throws CalculationException exception
	 */
	public PriceValue getDynamicBasePriceForBundle(
			final ProductModel product,final int noOfRentalDays) throws CalculationException;

	/**
	 * Gets the dynamic price data for product for order.
	 *
	 * @param isConstrainedProduct the is constrained product
	 * @param priceValue the price value
	 * @param rentedDays the rented days
	 * @return the dynamic price data for product for order
	 */
	BigDecimal getDynamicPriceDataForProductForOrder(final Boolean isConstrainedProduct, final Double priceValue, final Long rentedDays);

	/**
	 * Get Dynamic price for bundle product for order.
	 * @param product bundle product
	 * @param order order
	 * @return price data
	 * @throws CalculationException exception
	 */
	public PriceValue  getDynamicPriceForBundle(
			final ProductModel product,AbstractOrderModel order) throws CalculationException;
}
