package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.promotions.promotionengineservices.service.BlPromotionService;
import com.bl.core.services.calculation.BlPricingService;
import com.bl.core.stock.BlCommerceStockService;
import com.bl.facades.product.data.SerialProductData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang.StringUtils;
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
	private BaseStoreService baseStoreService;
	private BlPromotionService blPromotionService;
	private BlPricingService blPricingService;
	private BlCommerceStockService blCommerceStockService;
	private BlProductService blProductService;

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
					if(BooleanUtils.isTrue(serialProductModel.getForSale()) && isSerialActive(serialProductModel.getSerialStatus())) {
						final SerialProductData serialProductData = new SerialProductData();
						if (getBlProductService().isFunctionalAndCosmeticIsAvailable(serialProductModel)) {
							serialProductData.setCosmeticRating(
									Float.parseFloat(serialProductModel.getCosmeticRating().getCode()));
							serialProductData.setFunctionalRating(
									Float.parseFloat(serialProductModel.getFunctionalRating().getCode()));
						} else {
							serialProductData.setCosmeticRating(0.0f);
							serialProductData.setFunctionalRating(0.0f);
						}
						serialProductData
								.setConditionRating(serialProductModel.getConditionRatingOverallScore());
						serialProductData.setSerialId(serialProductModel.getProductId());
						populateSerialPromotionData(serialProductModel, serialProductData);
						//onSale changes
						serialProductData.setOnSale(
								serialProductModel.getOnSale() != null && serialProductModel.getOnSale());
						if (PredicateUtils.notNullPredicate()
								.evaluate(serialProductModel.getFinalSalePrice())) {
							serialProductData
									.setFinalSalePrice(getProductPriceData(serialProductModel.getFinalSalePrice()));
						}
						if (PredicateUtils.notNullPredicate()
								.evaluate(serialProductModel.getIncentivizedPrice())) {
							serialProductData.setFinalIncentivizedPrice(
									getProductPriceData(serialProductModel.getIncentivizedPrice()));
							target.setHasIncentivizedPrice(Boolean.TRUE);
						}

						//Added Check for serial product
						if (BooleanUtils.isTrue(source.getForRent())) {
							final boolean isUsedGearSerialNotAssignedToRentalOrder = blCommerceStockService
									.isUsedGearSerialNotAssignedToRentalOrder(serialProductModel.getProductId(),
											source.getCode());
							serialProductData
									.setIsSerialNotAssignedToRentalOrder(isUsedGearSerialNotAssignedToRentalOrder);
						}
						//Added Serial status for used gear product
						if (serialProductModel.getSerialStatus() != null) {
							serialProductData.setSerialStatus(serialProductModel.getSerialStatus());
						}
						serialProductData.setSerialSoftAssignedOrHardAssigned(
								BooleanUtils.isFalse((Objects.isNull(serialProductModel.getHardAssigned())
										? Boolean.FALSE : serialProductModel.getHardAssigned())) &&
										BooleanUtils.isFalse(Objects.isNull(serialProductModel.getSoftAssigned())
												? Boolean.FALSE : serialProductModel
												.getSoftAssigned()));  // To display the serial if hard assigned and soft assigned as false
						if(isToAddSerialInList(target, serialProductData))
						{
							serialProductDataList.add(serialProductData);
						}						
					}
		});
		sortSerialBasedOnConditionRating(serialProductDataList);
		target.setSerialproducts(serialProductDataList);
	}
	
	/**
	 * Checks if is to add serial in list.
	 *
	 * @param sku the sku
	 * @param serial the serial
	 * @return true, if is to add serial in list
	 */
	private boolean isToAddSerialInList(final ProductData sku, final SerialProductData serial)
	{
		if(BooleanUtils.isTrue(sku.getHasIncentivizedPrice()))
		{
			return BooleanUtils.isTrue(serial.isSerialSoftAssignedOrHardAssigned()) && checkSerial(sku, serial);
		}
		return checkSerial(sku, serial);
	}

	/**
	 * Check serial status and check weather the serial assigned to any order or not.
	 *
	 * @param sku the sku
	 * @param serial the serial
	 * @return true, if successful
	 */
	private boolean checkSerial(final ProductData sku, final SerialProductData serial)
	{
		return BooleanUtils.isFalse(SerialStatusEnum.SOLD.equals(serial.getSerialStatus()))
				|| (BooleanUtils.isTrue(sku.getForRent()) && BooleanUtils.isTrue(serial.isIsSerialNotAssignedToRentalOrder()));
	}

	/**
	 * Check if serial has active status
	 * @param serialStatusEnum serialStatusEnum
	 * @return boolean boolean
	 */
	private boolean isSerialActive(final SerialStatusEnum serialStatusEnum) {
		switch (serialStatusEnum.getCode()) {
			case BlCoreConstants.ACTIVE_STATUS:
			case BlCoreConstants.RECEIVED_OR_RETURNED:
			case BlCoreConstants.IN_HOUSE:
			case BlCoreConstants.ADDED_TO_CART:
				return Boolean.TRUE;
			default :
		}
		return Boolean.FALSE;
	}


	/**
	 * populate the promotion message for category wide promotion and price
	 * @param serialProductModel
	 * @param serialProductData
	 */
	private void populateSerialPromotionData(final BlSerialProductModel serialProductModel, final SerialProductData serialProductData) {
    final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
    final boolean baseStoreHasDiscount = baseStoreModel != null && baseStoreModel.getUsedGearPromotionDiscount()!= null && baseStoreModel.getUsedGearPromotionDiscount() > 0;
    final boolean baseStoreHasMessage = baseStoreModel != null && StringUtils.isNotBlank(baseStoreModel.getUsedGearPromotionMessage());
			if( baseStoreHasDiscount && getBlPromotionService().isUsedGearCategoryPromotionActive() && baseStoreHasMessage){
				serialProductData.setUgPromotionMessage(baseStoreModel.getUsedGearPromotionMessage());
				serialProductData.setSerialPromotionPrice(getSerialPromotionPrice(serialProductModel,baseStoreModel.getUsedGearPromotionDiscount()));
			}
	}

	/**
	 * Set Serial promo Price
	 * @param serialProductModel
	 * @param usedGearPromotionDiscount
	 * @return
	 */
	private PriceData getSerialPromotionPrice(final BlSerialProductModel serialProductModel,final Integer usedGearPromotionDiscount) {
		BigDecimal promoPrice = BigDecimal.ZERO;
		if(serialProductModel.getIncentivizedPrice() != null && serialProductModel.getIncentivizedPrice().compareTo(BigDecimal.ZERO) > 0 ){
	   	 promoPrice = getBlPricingService().getSerialPromotionPrice(serialProductModel.getIncentivizedPrice(),usedGearPromotionDiscount);
		 }
	   else  if(serialProductModel.getFinalSalePrice() != null && serialProductModel.getFinalSalePrice().compareTo(BigDecimal.ZERO) > 0 ){
			 promoPrice = getBlPricingService().getSerialPromotionPrice(serialProductModel.getFinalSalePrice(),usedGearPromotionDiscount);
		 }
	   return getProductPriceData(promoPrice);
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

	/**
	 * @return the blProductService
	 */
	public BlProductService getBlProductService()
	{
		return blProductService;
	}

	/**
	 * @param blProductService the blProductService to set
	 */
	public void setBlProductService(BlProductService blProductService)
	{
		this.blProductService = blProductService;
	}


	public BaseStoreService getBaseStoreService() {
		return baseStoreService;
	}

	public void setBaseStoreService(BaseStoreService baseStoreService) {
		this.baseStoreService = baseStoreService;
	}

	public BlPromotionService getBlPromotionService() {
		return blPromotionService;
	}

	public void setBlPromotionService(
			BlPromotionService blPromotionService) {
		this.blPromotionService = blPromotionService;
	}

	public BlPricingService getBlPricingService() {
		return blPricingService;
	}

	public void setBlPricingService(BlPricingService blPricingService) {
		this.blPricingService = blPricingService;
	}
}
