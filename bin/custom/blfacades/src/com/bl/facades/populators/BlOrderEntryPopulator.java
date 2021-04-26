package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.OrderEntryPopulator;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;

import org.apache.commons.lang3.BooleanUtils;


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
		populateDamageWavierValues(source, target);
	}

	/**
	 * Populate damage wavier attribute values.
	 *
	 * @param source
	 *           the source
	 * @param target
	 *           the target
	 */
	private void populateDamageWavierValues(final AbstractOrderEntryModel source, final OrderEntryData target)
	{
		target.setGearGaurdWaiverPrice(createPrice(source, source.getGearGaurdWaiverPrice()));
		target.setGearGaurdProFullWaiverPrice(createPrice(source, source.getGearGaurdProFullWaiverPrice()));
		target.setNoDamageWaiverSelected(BooleanUtils.toBoolean(source.getNoDamageWaiverSelected()));
		target.setGearGaurdWaiverSelected(BooleanUtils.toBoolean(source.getGearGaurdWaiverSelected()));
		target.setGearGaurdProFullWaiverSelected(BooleanUtils.toBoolean(source.getGearGaurdProFullWaiverSelected()));
	}
}
