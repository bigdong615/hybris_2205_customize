package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.OrderEntryPopulator;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;

import java.util.Objects;

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
}
