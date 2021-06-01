package com.braintree.converters;

import com.braintree.servicelayer.i18n.BraintreeRegionI18NService;
import com.braintreegateway.Address;
import de.hybris.platform.converters.impl.AbstractPopulatingConverter;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;


public class BraintreeAddressConverter extends AbstractPopulatingConverter<Address, AddressModel>
{
	private CommonI18NService commonI18NService;
	private BraintreeRegionI18NService regionI18NService;

	@Override
	public void populate(final Address address, final AddressModel model)
	{
		if (address != null && model != null)
		{
			final CountryModel country = findCountry(address.getCountryCodeAlpha2(), address.getCountryCodeAlpha3());
			model.setCountry(country);
			model.setRegion(getRegionI18NService().findRegion(country, address.getRegion()));
			model.setZone(address.getRegion());
			model.setStreetname(address.getStreetAddress());
			model.setStreetnumber(address.getExtendedAddress());
			model.setBrainTreeAddressId(address.getId());
			model.setCompany(address.getCompany());
			model.setFirstname(address.getFirstName());
			model.setLastname(address.getLastName());
			model.setTown(address.getLocality());
			model.setPostalcode(address.getPostalCode());
		}
	}

	private CountryModel findCountry(final String countryCodeAlpha2, final String countryCodeAlpha3)
	{
		CountryModel country = getCommonI18NService().getCountry(countryCodeAlpha2);
		if (country == null)
		{
			country = getCommonI18NService().getCountry(countryCodeAlpha3);
		}
		return country;
	}


	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	public void setCommonI18NService(CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
	}

	public BraintreeRegionI18NService getRegionI18NService()
	{
		return regionI18NService;
	}

	public void setRegionI18NService(BraintreeRegionI18NService regionI18NService)
	{
		this.regionI18NService = regionI18NService;
	}
}
