package com.bl.facades.populators;

import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlProductModel;

import com.bl.facades.product.data.BlBundleReferenceData;
import com.bl.facades.product.data.BlOptionData;
import com.google.common.collect.Lists;
import de.hybris.platform.catalog.enums.ProductReferenceTypeEnum;
import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.commercefacades.order.converters.populator.OrderEntryPopulator;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.model.product.ProductModel;


/**
 * Extended OOTB OrderEntryPopulator to populate custom attributes
 *
 * @author Ravikumar
 *
 */
public class BlOrderEntryPopulator extends OrderEntryPopulator
{

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void populate(final AbstractOrderEntryModel source, final OrderEntryData target)
	{
		if(source.isBundleEntry()) {
      target.setBundleEntry(Boolean.TRUE);
			addCommon(source, target);
			addProduct(source,target);
			addEntryGroups(source, target);
			addComments(source, target);
		}else{
			super.populate(source, target);
			populateDamageWaiverValues(source, target);
			populateOptionsValues(source, target);
			populateGiftCartPurcahseValues(source, target);
			target.setAqautechProduct(BooleanUtils.isTrue(source.getAqautechProduct()));
		}
	}

	/**
	 * Populate damage Waiver attribute values.
	 *
	 * @param source
	 *           the source
	 * @param target
	 *           the target
	 */
	private void populateDamageWaiverValues(final AbstractOrderEntryModel source, final OrderEntryData target)
	{
		final Double gearGuardWaiverPrice = source.getGearGuardWaiverPrice();
		target.setGearGuardWaiverPrice(
				createPrice(source, Objects.nonNull(gearGuardWaiverPrice) ? gearGuardWaiverPrice : Double.valueOf(0.0d)));
		final Double gearGuardProFullWaiverPrice = source.getGearGuardProFullWaiverPrice();
		target.setGearGuardProFullWaiverPrice(createPrice(source,
				Objects.nonNull(gearGuardProFullWaiverPrice) ? gearGuardProFullWaiverPrice : Double.valueOf(0.0d)));
		target.setNoDamageWaiverSelected(BooleanUtils.toBoolean(source.getNoDamageWaiverSelected()));
		target.setGearGuardWaiverSelected(BooleanUtils.toBoolean(source.getGearGuardWaiverSelected()));
		target.setGearGuardProFullWaiverSelected(BooleanUtils.toBoolean(source.getGearGuardProFullWaiverSelected()));
	}
	
	/**
	 * Populate gift cart purchase value from order entry
	 * @param source
	 *           the source
	 * @param target
	 *           the target
	 */

	private void populateGiftCartPurcahseValues(final AbstractOrderEntryModel source, final OrderEntryData target)
	{
			target.setRecipientEmail(source.getRecipientEmail());
			target.setRecipientName(source.getRecipientName());
			target.setRecipientMessage(source.getRecipientMessage());
	}
	
	/**
	 * Populate options from order entry
	 * @param source
	 *           the source
	 * @param target
	 *           the target
	 */

	private void populateOptionsValues(final AbstractOrderEntryModel source, final OrderEntryData target)
	{
		final List<BlOptionData> optionsDataList = new ArrayList<>();
		final ProductModel product = source.getProduct();
		if(product instanceof BlProductModel){
			final BlProductModel blProductModel = (BlProductModel) product;
			final List<BlOptionsModel> options = blProductModel.getOptions();

			if(CollectionUtils.isNotEmpty(options)){
				final BlOptionsModel blOptionsModel = options.iterator().next();
				final BlOptionData blOptionData = new BlOptionData();
				blOptionData.setOptionCode(blOptionsModel.getCode());
				blOptionData.setOptionName(blOptionsModel.getName());

				List<BlOptionsModel> subOptions = Lists.newArrayList(CollectionUtils.emptyIfNull(blOptionsModel.getSubOptions()));
				subOptions.forEach(option -> {
					final BlOptionData subBlOptionData = new BlOptionData();
					subBlOptionData.setOptionCode(option.getCode());
					subBlOptionData.setOptionName(option.getName());
					subBlOptionData.setOptionPrice(createPrice(source,
							Objects.nonNull(option.getUnitPrice()) ? option.getUnitPrice() : Double.valueOf(0.0d)));
					optionsDataList.add(subBlOptionData);
				});
				blOptionData.setSubOptions(optionsDataList);
				if(CollectionUtils.isNotEmpty(source.getOptions())){
					final BlOptionsModel selectedBlOptionsModel = source.getOptions().iterator().next();
					blOptionData.setOptionCode(selectedBlOptionsModel.getCode());
					blOptionData.setOptionName(selectedBlOptionsModel.getName());
					blOptionData.setOptionPrice(createPrice(source,
							Objects.nonNull(selectedBlOptionsModel.getUnitPrice()) ? selectedBlOptionsModel.getUnitPrice() : Double.valueOf(0.0d)));
					
				}
				target.setOption(blOptionData);

			}
		}


	}

	
	/**
	 * Adds the product data.
	 *
	 * @param orderEntry
	 *           the order entry
	 * @param entry
	 *           the entry
	 */
	@Override
	protected void addProduct(final AbstractOrderEntryModel orderEntry, final OrderEntryData entry)
	{
		final ProductModel product = orderEntry.getProduct();
		entry.setProduct(getProductConverter().convert(Objects.nonNull(product) && product instanceof BlSerialProductModel
				? ((BlSerialProductModel) product).getBlProduct() : product));
		if(product!= null) {
			populatingBlProductSpecificData(product,entry.getProduct());
		}
	}

	/**
	 * This method used for populating BlProduct specific data for OrderEntry.
	 * @param productModel
	 * @param productData
	 */
	private void populatingBlProductSpecificData(final ProductModel productModel,
			final ProductData productData) {
		BlProductModel blProductModel = (BlProductModel) productModel;
		productData.setProductId(blProductModel.getProductId());
		productData.setIsVideo(blProductModel.getIsVideo());
		productData.setIsBundle(blProductModel.isBundleProduct());
		populateBundleEntryData(blProductModel, productData);
	}

	/**
	 * This method is used for populating all sku name for single bundle product.
	 * @param blProductModel
	 * @param productData
	 */
	private void populateBundleEntryData(final BlProductModel blProductModel,
			final ProductData productData) {
		List<ProductReferenceModel> referenceModel = blProductModel.getProductReferences().stream()
				.filter(refer -> ProductReferenceTypeEnum.CONSISTS_OF.equals(refer.getReferenceType()))
				.collect(
						Collectors.toList());
		if (CollectionUtils.isNotEmpty(referenceModel)) {
			List<BlBundleReferenceData> bundleProductReference = new ArrayList<>();
			referenceModel.forEach(productReferenceModel -> {
				BlBundleReferenceData blBundleReferenceData = new BlBundleReferenceData();
				blBundleReferenceData.setProductReferenceName(productReferenceModel.getTarget().getName());
				bundleProductReference.add(blBundleReferenceData);
			});
			productData.setBundleProductReference(bundleProductReference);
		}
	}



}
