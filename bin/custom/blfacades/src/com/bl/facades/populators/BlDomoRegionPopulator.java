package com.bl.facades.populators;

import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.util.stream.Collectors;


public class BlDomoRegionPopulator implements Populator<RegionModel, RegionData>
{
	@Override
	public void populate(final RegionModel source, final RegionData target) throws ConversionException
	{
		target.setCreatedTS(source.getCreationtime());
		target.setModifiedTS(source.getModifiedtime());
		target.setName(source.getName());
		target.setIsocode(source.getIsocode());
		target.setIsocodeShort(source.getIsocodeShort());
		target.setActive(source.getActive());
		if (source.getCountry() != null)
		{
			target.setCountryIso(source.getCountry().getIsocode());
		}
		target.setComments(source.getComments().stream().map(String::valueOf).collect(Collectors.joining(",")));
	}
}
