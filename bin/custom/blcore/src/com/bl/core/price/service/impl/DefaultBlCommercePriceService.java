package com.bl.core.price.service.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.model.BlProductModel;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.price.strategies.BlProductDynamicPriceStrategy;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.BlBundleReferenceData;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.commerceservices.price.impl.DefaultCommercePriceService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


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
	private BaseStoreService baseStoreService;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public PriceInformation getWebPriceForProduct(final ProductModel product)
	{
		if (PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product) && !((BlProductModel) product).isBundleProduct())
		{
			validateParameterNotNull(product, "Product cannot be null");
			final List<PriceInformation> prices = getPriceService().getPriceInformationsForProduct(product);
			if (CollectionUtils.isNotEmpty(prices))
			{
				final PriceInformation defaultPriceInformation = prices.get(0);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Default Price is {} for product {}",
						defaultPriceInformation.getPriceValue().getValue(), product.getCode());
				final Long rentalDays = getRentalDaysFromSession();
				return Objects.nonNull(rentalDays) && rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY
						? getBlProductDynamicPriceStrategy().getDynamicPriceInformationForProduct((BlProductModel) product,
								defaultPriceInformation, rentalDays)
						: defaultPriceInformation;
			}
			return null;
		}
		else if(PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(product) && ((BlProductModel) product).isBundleProduct()){
					final PriceInformation productReferencesPrice;
          List<PriceInformation> lPrices = new ArrayList<>();
			final List<ProductReferenceModel> productReferences = Lists.newArrayList(CollectionUtils.emptyIfNull(((BlProductModel) product)
					.getProductReferences()));
			final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
			List<BlBundleReferenceData> list= new ArrayList<>();
			if (CollectionUtils.isNotEmpty(productReferences)) {
				productReferences.stream().filter(refer -> ProductReferenceTypeEnum.CONSISTS_OF.equals(refer.getReferenceType())).forEach(productReferenceModel -> {
					final ProductModel target = productReferenceModel.getTarget();
					final List<PriceInformation> prices = getPriceService().getPriceInformationsForProduct(target);
					if (CollectionUtils.isNotEmpty(prices))
					{
						final PriceInformation defaultPriceInformation = prices.get(0);
						BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Default Price is {} for product {}",
								defaultPriceInformation.getPriceValue().getValue(), product.getCode());
						final Long rentalDays = getRentalDaysFromSession();
						final PriceInformation info = Objects.nonNull(rentalDays) && rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY
								? getBlProductDynamicPriceStrategy().getDynamicPriceInformationForProduct((BlProductModel) product,
								defaultPriceInformation, rentalDays)
								: defaultPriceInformation;
						lPrices.add(info);

					}
				});
				if(CollectionUtils.isEmpty(lPrices)){
					return null;
				}
				AtomicDouble rPrice = new AtomicDouble(0.0d);
				lPrices.forEach(refPrice -> rPrice.addAndGet(refPrice.getPriceValue().getValue()));
				final PriceInformation newPriceInformation = getBlProductDynamicPriceStrategy()
						.createNewPriceInformation(lPrices.get(0), BigDecimal.valueOf(rPrice.get()));
				return newPriceInformation;
			}

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
		final Long rentalDays = getRentalDaysFromSession();
		return Objects.nonNull(rentalDays) && rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY
				? getBlProductDynamicPriceStrategy().getDynamicPriceDataForProduct(isConstrainedProduct, priceValue, rentalDays)
				: BigDecimal.valueOf(priceValue);
	}

	/**
	 * Gets the dynamic price data for product.
	 *
	 * @param isConstrainedProduct the is constrained product
	 * @param priceValue           the price value
	 * @param rentalDays           the rentalDays
	 * @return the dynamic price data for product
	 */
	@Override
	public BigDecimal getDynamicPriceDataForProduct(final Boolean isConstrainedProduct,final Double priceValue,final  Long rentalDays) {
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Default Price Value is {}", priceValue);
		return Objects.nonNull(rentalDays) && rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY
				? getBlProductDynamicPriceStrategy().getDynamicPriceDataForProduct(isConstrainedProduct, priceValue, rentalDays)
				: BigDecimal.valueOf(priceValue);
	}

	/**
	 * Get rental days from the session.
	 *
	 * @return the rental days
	 */
	private Long getRentalDaysFromSession()
	{
		final RentalDateDto rentalDatesFromSession = getBlDatePickerService().getRentalDatesFromSession();
		return Objects.nonNull(rentalDatesFromSession) && StringUtils.isNotBlank(rentalDatesFromSession.getNumberOfDays())
				? getRentalDays(Long.valueOf(rentalDatesFromSession.getNumberOfDays()))
				: null;
	}

	/**
	 * Gets the rental days.
	 *
	 * @param rentalDaysFromSession
	 *           the rental days from session
	 * @return the rental days
	 */
	private Long getRentalDays(final Long rentalDaysFromSession)
	{
		return rentalDaysFromSession >= BlCoreConstants.ONE_RENTAL_DAY && rentalDaysFromSession <= BlCoreConstants.THREE_RENTAL_DAYS
				? Long.valueOf(BlCoreConstants.THREE_RENTAL_DAYS)
				: rentalDaysFromSession;
	}

	/**
	 * This method created to get dynamic price for extend rental products
	 */

	@Override
	public PriceInformation getWebPriceForExtendProduct(final ProductModel product , final Long rentalDays)
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
				return Objects.nonNull(rentalDays) && rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY
						? getBlProductDynamicPriceStrategy().getDynamicPriceInformationForProduct((BlProductModel) product,
						defaultPriceInformation, rentalDays)
						: defaultPriceInformation;
			}
			return null;
		}
		return super.getWebPriceForProduct(product);
	}


	@Override
	public PriceInformation getWebPriceForTax(final ProductModel product , final AbstractOrderModel abstractOrderModel)
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
				final Long rentalDays = BlDateTimeUtils.getDaysBetweenDates(abstractOrderModel.getRentalStartDate() , abstractOrderModel.getRentalEndDate()) + 1;
				return Objects.nonNull(rentalDays) && rentalDays.longValue() != BlCoreConstants.DEFAULT_RENTAL_DAY // NOSONAR
						? getBlProductDynamicPriceStrategy().getDynamicPriceInformationForProduct((BlProductModel) product,
						defaultPriceInformation, rentalDays)
						: defaultPriceInformation;
			}
			return null;
		}
		return super.getWebPriceForProduct(product);
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
	public void setBaseStoreService(BaseStoreService baseStoreService) {
		this.baseStoreService = baseStoreService;
	}

	public BaseStoreService getBaseStoreService() {
		return baseStoreService;
	}
}
