package com.braintree.servicelayer.i18n.impl;


import com.braintree.servicelayer.i18n.BraintreeRegionI18NService;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import org.apache.commons.lang.StringUtils;


public class DefaultBraintreeRegionI18NService implements BraintreeRegionI18NService
{

	private CommonI18NService commonI18NService;

	@Override
	public RegionModel findRegion(final CountryModel countryModel, final String regionSign)
	{
		if (countryModel != null && StringUtils.isNotBlank(regionSign))
		{
			try
			{
				return getCommonI18NService().getRegion(countryModel, regionSign);
			}
			catch (final UnknownIdentifierException unknownIdentifierException)
			{
				return findRegionByCanonicalIsoCode(countryModel, regionSign);
			}
		}
		return findRegionByName(countryModel, regionSign);
	}

	private RegionModel findRegionByCanonicalIsoCode(final CountryModel countryModel, final String regionName)
	{
		try
		{
			final String canonicalIsoCode = countryModel.getIsocode() + "-" + regionName;
			return getCommonI18NService().getRegion(countryModel, canonicalIsoCode);
		}
		catch (final UnknownIdentifierException unknownIdentifierException)
		{
			return findRegionByName(countryModel, regionName);
		}
	}

	private RegionModel findRegionByName(final CountryModel countryModel, final String regionName)
	{
		if (StringUtils.isNotBlank(regionName))
		{
			for (final RegionModel regionModel : countryModel.getRegions())
			{
				if (regionModel != null && regionName.equals(regionModel.getName()))
				{
					return regionModel;
				}
			}
		}
		return null;
	}

	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	public void setCommonI18NService(CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
	}
}
