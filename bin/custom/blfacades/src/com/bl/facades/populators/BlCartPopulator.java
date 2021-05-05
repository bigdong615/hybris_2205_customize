package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.CartPopulator;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.core.model.order.CartModel;


/**
 * Extended OOTB CartPopulator to populate custom attributes
 *
 * @author Ravikumar
 *
 */
public class BlCartPopulator extends CartPopulator<CartData>
{

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void populate(final CartModel source, final CartData target)
	{
		super.populate(source, target);
		target.setTotalDamageWaiverCost(createPrice(source, source.getTotalDamageWaiverCost()));
		target.setFirstName(source.getFirstName());
		target.setLastName(source.getLastName());
		target.setEmail(source.getEmail());
		target.setPhone(source.getPhone());
		target.setTaxAvalaraCalculated(createPrice(source , source.getTotalAvalaraTaxCalculated()));
	}

}
