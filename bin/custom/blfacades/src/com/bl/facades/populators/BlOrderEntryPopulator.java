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
		populateDamageWaiverValues(source, target);
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
		target.setGearGuardWaiverPrice(createPrice(source, source.getGearGuardWaiverPrice()));
		target.setGearGuardProFullWaiverPrice(createPrice(source, source.getGearGuardProFullWaiverPrice()));
		target.setNoDamageWaiverSelected(BooleanUtils.toBoolean(source.getNoDamageWaiverSelected()));
		target.setGearGuardWaiverSelected(BooleanUtils.toBoolean(source.getGearGuardWaiverSelected()));
		target.setGearGuardProFullWaiverSelected(BooleanUtils.toBoolean(source.getGearGuardProFullWaiverSelected()));
	}
}
