package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.facades.product.data.SerialProductData;
import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.product.data.PromotionData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.promotions.PromotionsService;
import de.hybris.platform.promotions.model.AbstractPromotionModel;
import de.hybris.platform.promotions.model.PromotionGroupModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.time.TimeService;
import de.hybris.platform.site.BaseSiteService;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.BooleanUtils;



/**
 * This populator is used for populating bl Serial Product related specific product attribute.
 * 
 * @author Ravikumar
 *
 */
public class BlSerialProductPopulator extends AbstractBlProductPopulator implements Populator<BlProductModel, ProductData>
{
	private PriceDataFactory priceDataFactory;
	private CommonI18NService commonI18NService;
	private BlCommerceStockService blCommerceStockService;
	private PromotionsService promotionsService;
	private Converter<AbstractPromotionModel, PromotionData> promotionsConverter;
	private TimeService timeService;
	private BaseSiteService baseSiteService;

	@Override
	public void populate(final BlProductModel source, final ProductData target)
	{
		//populates list of serial product data assigned to SKU
		populateSerialProducts(source, target);

		target.setUsedIncludes(source.getUsedIncludes());
		target.setForSale(BooleanUtils.toBoolean(source.getForSale()));
		target.setUsedGearVideosLink(populateVideo(CollectionUtils.emptyIfNull(source.getUsedGearVideosLink())));

		target.setUsedDescription(source.getUsedDescription());
	}

	/*
	 * This method is used for populating list of serial products assigned to SKU.
	 */
	private void populateSerialProducts(final BlProductModel source, final ProductData target)
	{
		final List<SerialProductData> serialProductDataList = new ArrayList<>();
		final List<BlSerialProductModel> blSerialProductModels = (List<BlSerialProductModel>) CollectionUtils
				.emptyIfNull(source.getSerialProducts());
		blSerialProductModels.forEach(serialProductModel -> {
			final SerialProductData serialProductData = new SerialProductData();
			serialProductData.setConditionRating(serialProductModel.getConditionRatingOverallScore());
			serialProductData.setCosmeticRating(serialProductModel.getCosmeticRating());
			serialProductData.setFunctionalRating(serialProductModel.getFunctionalRating());
			serialProductData.setSerialId(serialProductModel.getProductId());
			setPromotionsForSerials(serialProductModel,serialProductData);
			//onSale changes
			serialProductData.setOnSale(serialProductModel.getOnSale() != null && serialProductModel.getOnSale());
			if (PredicateUtils.notNullPredicate().evaluate(serialProductModel.getFinalSalePrice()))
			{
				serialProductData.setFinalSalePrice(getProductPriceData(serialProductModel.getFinalSalePrice()));
			}
			if (PredicateUtils.notNullPredicate().evaluate(serialProductModel.getIncentivizedPrice()))
			{
				serialProductData.setFinalIncentivizedPrice(getProductPriceData(serialProductModel.getIncentivizedPrice()));
				target.setHasIncentivizedPrice(Boolean.TRUE);
			}

			//Added Check for serial product
			if(BooleanUtils.isTrue(source.getForRent()))
			{
			   final boolean isUsedGearSerialNotAssignedToRentalOrder = blCommerceStockService
					.isUsedGearSerialNotAssignedToRentalOrder(serialProductModel.getProductId(), source.getCode());
			  serialProductData.setIsSerialNotAssignedToRentalOrder(isUsedGearSerialNotAssignedToRentalOrder);
			}
			//Added Serial status for used gear product
			if (serialProductModel.getSerialStatus() != null)
			{
				serialProductData.setSerialStatus(serialProductModel.getSerialStatus());
			}
			serialProductDataList.add(serialProductData);
		});
		sortSerialBasedOnConditionRating(serialProductDataList);
		target.setSerialproducts(serialProductDataList);
	}

	/**
	 * Populating serial Specific promotions on serial product data
	 * @param serialProductModel
	 * @param serialProductData
	 */
	private void setPromotionsForSerials(final BlSerialProductModel serialProductModel, final SerialProductData serialProductData) {
		final BaseSiteModel baseSiteModel = getBaseSiteService().getCurrentBaseSite();
		if (baseSiteModel != null) {
			final PromotionGroupModel defaultPromotionGroup = baseSiteModel.getDefaultPromotionGroup();
			final Date currentTimeRoundedToMinute = DateUtils
					.round(getTimeService().getCurrentTime(), Calendar.MINUTE);

			if (defaultPromotionGroup != null) {
				final List<AbstractPromotionModel> promotions = (List<AbstractPromotionModel>) getPromotionsService()
						.getAbstractProductPromotions(Collections.singletonList(defaultPromotionGroup),
								serialProductModel, true,
								currentTimeRoundedToMinute);
				serialProductData.setPotentialPromotions(getPromotionsConverter().convertAll(promotions));
			}
		}
	}


	/**
	 * Gets the product price data.
	 *
	 * @param priceValue
	 *           the price value
	 * @return the product price data
	 */
	private PriceData getProductPriceData(final BigDecimal priceValue)
	{
		return getPriceDataFactory().create(PriceDataType.BUY, priceValue, getCommonI18NService().getCurrentCurrency());
	}

	/**
	 * Sorting serial products in Ascending Order based on condition rating.
	 *
	 * @param serialProductDataList
	 *           the serial product data list
	 * @return the list
	 */
	private List<SerialProductData> sortSerialBasedOnConditionRating(final List<SerialProductData> serialProductDataList)
	{
		if (CollectionUtils.isNotEmpty(serialProductDataList))
		{
			final Comparator<SerialProductData> serialProductDataComparator = Comparator
					.comparing(SerialProductData::getConditionRating);
			Collections.sort(serialProductDataList, serialProductDataComparator);
		}
		return serialProductDataList;
	}


	/**
	 * @return the priceDataFactory
	 */
	public PriceDataFactory getPriceDataFactory()
	{
		return priceDataFactory;
	}

	/**
	 * @param priceDataFactory
	 *           the priceDataFactory to set
	 */
	public void setPriceDataFactory(final PriceDataFactory priceDataFactory)
	{
		this.priceDataFactory = priceDataFactory;
	}

	/**
	 * @return the commonI18NService
	 */
	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	/**
	 * @param commonI18NService
	 *           the commonI18NService to set
	 */
	public void setCommonI18NService(final CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
	}

	/**
	 * @return the blCommerceStockService
	 */
	public BlCommerceStockService getBlCommerceStockService()
	{
		return blCommerceStockService;
	}

	/**
	 * @param blCommerceStockService
	 *           the blCommerceStockService to set
	 */
	public void setBlCommerceStockService(final BlCommerceStockService blCommerceStockService)
	{
		this.blCommerceStockService = blCommerceStockService;
	}


	public PromotionsService getPromotionsService() {
		return promotionsService;
	}

	public void setPromotionsService(PromotionsService promotionsService) {
		this.promotionsService = promotionsService;
	}

	public Converter<AbstractPromotionModel, PromotionData> getPromotionsConverter() {
		return promotionsConverter;
	}

	public void setPromotionsConverter(
			Converter<AbstractPromotionModel, PromotionData> promotionsConverter) {
		this.promotionsConverter = promotionsConverter;
	}

	public TimeService getTimeService() {
		return timeService;
	}

	public void setTimeService(TimeService timeService) {
		this.timeService = timeService;
	}

	public BaseSiteService getBaseSiteService() {
		return baseSiteService;
	}

	public void setBaseSiteService(BaseSiteService baseSiteService) {
		this.baseSiteService = baseSiteService;
	}
}
