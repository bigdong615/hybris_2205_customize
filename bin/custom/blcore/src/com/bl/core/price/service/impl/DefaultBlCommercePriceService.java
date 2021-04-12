package com.bl.core.price.service.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.commerceservices.price.impl.DefaultCommercePriceService;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.servicelayer.session.SessionService;

import java.math.BigDecimal;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.price.strategies.BlProductDynamicPriceStrategy;


/**
 * Extended OOTB DefaultCommercePriceService to get the Dynamic Prices for the Rental Products on the basis of the
 * selection of rental days.
 *
 * @author Ravikumar
 *
 */
public class DefaultBlCommercePriceService extends DefaultCommercePriceService implements BlCommercePriceService
{
	private SessionService sessionService;

	private BlProductDynamicPriceStrategy blProductDynamicPriceStrategy;

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
				final Long rentalDays = getSessionService().getAttribute(BlCoreConstants.SELECTED_RENTAL_DAYS);
				return isRentalDaysEligible(rentalDays)
						? getBlProductDynamicPriceStrategy().getDynamicPriceInformationForProduct((BlProductModel) product,
								defaultPriceInformation, rentalDays)
						: defaultPriceInformation;
			}
			return null;
		}
		return super.getWebPriceForProduct(product);
	}

	@Override
	public BigDecimal getDynamicPriceDataForProduct(final Boolean isConstrainedProduct, final Double priceValue)
	{
		final Long rentalDays = getSessionService().getAttribute(BlCoreConstants.SELECTED_RENTAL_DAYS);
		return isRentalDaysEligible(rentalDays)
				? getBlProductDynamicPriceStrategy().getDynamicPriceDataForProduct(isConstrainedProduct, priceValue, rentalDays)
				: BigDecimal.valueOf(priceValue);
	}

	private boolean isRentalDaysEligible(final Long rentalDays)
	{
		return PredicateUtils.notNullPredicate().evaluate(rentalDays)
				&& rentalDays.longValue() >= BlCoreConstants.MINIMUM_RENTAL_DAYS
				&& rentalDays.longValue() <= BlCoreConstants.MAXIMUM_RENTAL_DAYS
				&& rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY;
	}

	/**
	 * @return the sessionService
	 */
	public SessionService getSessionService()
	{
		return sessionService;
	}

	/**
	 * @param sessionService
	 *           the sessionService to set
	 */
	public void setSessionService(final SessionService sessionService)
	{
		this.sessionService = sessionService;
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
}
