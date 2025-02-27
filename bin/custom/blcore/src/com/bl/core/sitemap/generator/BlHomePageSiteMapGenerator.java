/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.core.sitemap.generator;

import de.hybris.platform.acceleratorservices.sitemap.data.SiteMapUrlData;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.cms2.model.site.CMSSiteModel;
import de.hybris.platform.cms2.servicelayer.services.CMSPageService;
import de.hybris.platform.converters.Converters;

import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Required;


public class BlHomePageSiteMapGenerator extends BlAbstractSiteMapGenerator<ContentPageModel>
{

	private CMSPageService cmsPageService;

	@Override
	public List<SiteMapUrlData> getSiteMapUrlData(final List<ContentPageModel> models)
	{
		return Converters.convertAll(models, getSiteMapUrlDataConverter());
	}

	@Override
	protected List<ContentPageModel> getDataInternal(final CMSSiteModel siteModel)
	{
		final ContentPageModel homepage = getCmsPageService().getHomepage();
		return Collections.singletonList(homepage);
	}

	protected CMSPageService getCmsPageService()
	{
		return cmsPageService;
	}

	@Required
	public void setCmsPageService(final CMSPageService cmsPageService)
	{
		this.cmsPageService = cmsPageService;
	}
}
