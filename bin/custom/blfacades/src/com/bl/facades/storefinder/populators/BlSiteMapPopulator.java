package com.bl.facades.storefinder.populators;

import de.hybris.platform.acceleratorservices.sitemap.data.SiteMapUrlData;
import de.hybris.platform.commerceservices.url.UrlResolver;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.lang.StringEscapeUtils;

import com.bl.core.model.BlProductModel;


public class BlSiteMapPopulator<T extends ItemModel> implements Populator<T, SiteMapUrlData>
{
	private UrlResolver<ProductModel> urlResolver;


	@Override
	public void populate(final T source, final SiteMapUrlData target) throws ConversionException
	{
		String relUrl = null;
		if (source instanceof ProductModel)
		{
			final BlProductModel sourcePrd = (BlProductModel) source;
			target.setLastmod(convertDateToString(source.getModifiedtime()));
			if (sourcePrd.getForRent())
			{
				relUrl = StringEscapeUtils.escapeXml("/rent" + getUrlResolver().resolve(sourcePrd));
			}
			else if (sourcePrd.getForSale())
			{
				relUrl = StringEscapeUtils.escapeXml("/buy" + getUrlResolver().resolve(sourcePrd));
			}
			target.setForRent(sourcePrd.getForRent());
			target.setImages(null);
			target.setLoc(relUrl);
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

	public UrlResolver<ProductModel> getUrlResolver()
	{
		return urlResolver;
	}

	public void setUrlResolver(final UrlResolver<ProductModel> urlResolver)
	{
		this.urlResolver = urlResolver;
	}
}
