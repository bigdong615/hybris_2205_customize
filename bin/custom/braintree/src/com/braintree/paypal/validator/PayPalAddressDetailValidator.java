package com.braintree.paypal.validator;

import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import org.apache.log4j.Logger;

import static org.apache.commons.lang.StringUtils.isNotEmpty;


public class PayPalAddressDetailValidator
{
	private final static Logger LOG = Logger.getLogger(PayPalAddressDetailValidator.class);

	private CommonI18NService commonI18NService;

	public boolean validatePayPalCountryCode(final String countryCode)
	{
		if (isNotEmpty(countryCode))
		{
			for (final CountryModel country : commonI18NService.getAllCountries())
			{
				if (country.getIsocode().equals(countryCode))
				{
					return true;
				}
			}
		}
		LOG.error(String.format("Country with code %s is not supported!", countryCode));

		return false;
	}

	public boolean validatePayPalRegionCode(final String countryCode, final String regionCode)
	{
		if (isNotEmpty(regionCode))
		{
			for (final RegionModel region : commonI18NService.getAllRegions())
			{
				if (region.getIsocode().equals(countryCode + "-" + regionCode))
				{
					return true;
				}
			}
		}

		LOG.error(String.format("Region with code %s and Country with code %s is not supported!", regionCode, countryCode));

		return false;
	}

	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	public void setCommonI18NService(final CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
	}


}
