/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.sitemap.generator;

import de.hybris.platform.acceleratorservices.sitemap.data.SiteMapUrlData;
import de.hybris.platform.catalog.enums.ArticleApprovalStatus;
import de.hybris.platform.cms2.model.site.CMSSiteModel;
import de.hybris.platform.converters.Converters;
import de.hybris.platform.core.model.product.ProductModel;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;


public class BlProductPageSiteMapGenerator extends BlAbstractSiteMapGenerator<ProductModel>
{
	private static final Logger LOG = Logger.getLogger(BlProductPageSiteMapGenerator.class);

	@Override
	public List<SiteMapUrlData> getSiteMapUrlData(final List<ProductModel> models)
	{
		return Converters.convertAll(models, getSiteMapUrlDataConverter());
	}

	@Override
	protected List<ProductModel> getDataInternal(final CMSSiteModel siteModel)
	{
		LOG.info("BlProductPageSiteMapGenerator getDataInternal");
		try
		{
			final String query = "select {prd.pk},{prd.code} FROM {BlProduct! as prd "
					+ "JOIN Catalog AS cat ON {prd.catalog}={cat.pk} " + "JOIN CatalogVersion as cv ON {prd.catalogVersion}={cv.pk}} "
					+ "where {cat.id}='blProductCatalog' AND {cv.version}='Online'  and {prd.approvalStatus} = ?approvalStatus";

			final Map<String, Object> params = new HashMap<String, Object>();
			params.put("approvalStatus", ArticleApprovalStatus.APPROVED);

			final List<ProductModel> productList = doSearch(query, params, ProductModel.class);
			System.out.println("BlProductPageSiteMapGenerator Product count : " + productList.size());
			LOG.info("BlProductPageSiteMapGenerator ProductCount : " + productList.size());
			return productList;
		}
		catch (final Exception e)
		{
			LOG.error("BlProductPageSiteMapGenerator Exception : " + e.getStackTrace());
		}
		return null;
	}
}
