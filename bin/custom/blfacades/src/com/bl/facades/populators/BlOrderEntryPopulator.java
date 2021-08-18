package com.bl.facades.populators;

import com.bl.core.model.BlOptionsModel;
import com.bl.core.model.BlProductModel;

import com.bl.facades.product.data.BlOptionData;
import com.google.common.collect.Lists;
import de.hybris.platform.commercefacades.order.converters.populator.OrderEntryPopulator;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

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
		super.populate(source, target);
		populateDamageWaiverValues(source, target);
		populateOptionsValues(source, target);
		populateGiftCartPurcahseValues(source, target);
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
			entry.getProduct().setProductId(((BlProductModel) product).getProductId());
			entry.getProduct().setIsVideo(((BlProductModel) product).getIsVideo());
		}
	}
}
