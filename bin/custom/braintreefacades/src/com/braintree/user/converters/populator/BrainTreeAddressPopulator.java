package com.braintree.user.converters.populator;

import de.hybris.platform.commercefacades.user.converters.populator.AddressPopulator;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.model.user.AddressModel;


public class BrainTreeAddressPopulator extends AddressPopulator
{

	@Override
	public void populate(AddressModel source, AddressData target)
	{
		super.populate(source, target);
		target.setBrainTreeAddressId(source.getBrainTreeAddressId());
	}
}
