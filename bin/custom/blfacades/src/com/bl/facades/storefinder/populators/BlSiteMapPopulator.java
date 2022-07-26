/**
 *
 */
package com.bl.facades.storefinder.populators;

import de.hybris.platform.acceleratorservices.sitemap.data.SiteMapUrlData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;


/**
 * @author srinivas
 *
 */
public class BlSiteMapPopulator<T extends ItemModel> implements Populator<T, SiteMapUrlData>
{
	@Override
	public void populate(final T source, final SiteMapUrlData target) throws ConversionException
	{
		if (source instanceof ProductModel)
		{
			target.setLastmod(convertDateToString(source.getModifiedtime()));
			target.setImages(null);
		}
		else
		{
			target.setLastmod(convertDateToString(source.getModifiedtime()));
		}
	}
	public String convertDateToString(final Date date)
	{
		final DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
		final String strDate = dateFormat.format(date);
		return strDate;
	}
}
