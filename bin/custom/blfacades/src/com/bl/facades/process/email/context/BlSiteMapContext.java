/**
 *
 */
package com.bl.facades.process.email.context;

import de.hybris.platform.acceleratorservices.enums.SiteMapPageEnum;
import de.hybris.platform.acceleratorservices.sitemap.renderer.SiteMapContext;
import de.hybris.platform.cms2.model.site.CMSSiteModel;


/**
 * @author srinivas
 *
 */
public class BlSiteMapContext extends SiteMapContext
{
	private static final String PAGE_TYPE = "pageType";

	@Override
	public void init(final CMSSiteModel site, final SiteMapPageEnum siteMapPageEnum)
	{
		super.init(site, siteMapPageEnum);
		put(PAGE_TYPE, siteMapPageEnum.getCode());
	}
}
