package com.bl.core.utils;

import de.hybris.platform.acceleratorservices.urlresolver.SiteBaseUrlResolutionService;
import de.hybris.platform.site.BaseSiteService;
import org.apache.commons.lang.StringUtils;

/**
 * @author vijay vishwakarma
 * This class created for generating url
 */
public class BlURLServicesUtils {

  private SiteBaseUrlResolutionService siteBaseUrlResolutionService;
  private BaseSiteService baseSiteService;

  public String getRequestedURl(String urlString){
    return getSiteBaseUrlResolutionService()
        .getWebsiteUrlForSite(getBaseSiteService().getCurrentBaseSite(),
            StringUtils.EMPTY, Boolean.TRUE, urlString);
  }

  public SiteBaseUrlResolutionService getSiteBaseUrlResolutionService() {
    return siteBaseUrlResolutionService;
  }

  public void setSiteBaseUrlResolutionService(
      SiteBaseUrlResolutionService siteBaseUrlResolutionService) {
    this.siteBaseUrlResolutionService = siteBaseUrlResolutionService;
  }

  public BaseSiteService getBaseSiteService() {
    return baseSiteService;
  }

  public void setBaseSiteService(BaseSiteService baseSiteService) {
    this.baseSiteService = baseSiteService;
  }
}
