/*
 * [y] hybris Platform
 *
 * Copyright (c) 2018 SAP SE or an SAP affiliate company.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of SAP
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with SAP.
 *
 */
package com.bl.Ordermanagement.converters;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.storelocator.data.AddressData;


/**
 * Converter implementation for {@link de.hybris.platform.core.model.user.AddressModel} as source and
 * {@link de.hybris.platform.storelocator.data.AddressData} as target type.
 */
public class AddressPopulator implements Populator<AddressModel, AddressData>
{
	@Override
	public void populate(final AddressModel source, final AddressData target)
	{
		if (source != null && target != null)
		{
			if (source.getCountry() != null)
			{
				target.setCountryCode(source.getCountry().getIsocode());
			}
			target.setCity(source.getTown());
			target.setStreet(source.getStreetname());
			target.setZip(source.getPostalcode());
		}
	}
}
