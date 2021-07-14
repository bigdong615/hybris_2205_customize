package com.braintree.servicelayer.i18n;


import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.c2l.RegionModel;


public interface BraintreeRegionI18NService
{
	/**
	 * find region by name/isocode/shortIsocode
	 *
	 * @param countryModel country
	 * @param regionSign   name of state/province/region
	 * @return region
	 */
	RegionModel findRegion(final CountryModel countryModel, final String regionSign);
}
