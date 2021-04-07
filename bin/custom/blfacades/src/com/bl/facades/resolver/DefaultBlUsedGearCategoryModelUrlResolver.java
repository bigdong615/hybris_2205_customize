package com.bl.facades.resolver;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.url.impl.DefaultCategoryModelUrlResolver;

/**
 * @author Manikandan
 * This Class created to override the OOB DefaultCategoryModelUrlResolver to get URL pattern for used gear category
 */
public class DefaultBlUsedGearCategoryModelUrlResolver  extends DefaultCategoryModelUrlResolver {

  private String usedGearPattern;

  /**
   * This Method is overiden for ressolving URL pattern for used gear category
   */
  @Override
  protected String resolveInternal(final CategoryModel source)
  {
    //  Added Condition for used gear category
    if(BlCoreConstants.USED_CATEGORY_CODE.equalsIgnoreCase(source.getCode()) || source.getCode().startsWith(BlCoreConstants.USED)) {
      // Work out values
      // Replace pattern values
      String url = getUsedGearPattern();
      if (url.contains(BlCoreConstants.BASE_SITE_UID)) {
        url = url.replace(BlCoreConstants.BASE_SITE_UID, urlEncode(getBaseSiteUid().toString()));
      }
      if (url.contains(BlCoreConstants.CATEGORY_PATTERN_CODE)) {
        final String categoryCode = urlEncode(source.getCode()).replaceAll("\\+", BlCoreConstants.REPLACE_STRING);
        url = url.replace(BlCoreConstants.CATEGORY_PATTERN_CODE, categoryCode);
      }
      if (url.contains(BlCoreConstants.CATALOG_ID)) {
        url = url
            .replace(BlCoreConstants.CATALOG_ID, urlEncode(source.getCatalogVersion().getCatalog().getId()));
      }
      if (url.contains(BlCoreConstants.CATALOG_VERSION)) {
        url = url.replace(BlCoreConstants.CATALOG_VERSION, urlEncode(source.getCatalogVersion().getVersion()));
      }
      return url;
    }
    return super.resolveInternal(source);
  }

  // Get The Custom pattern for used gear category
  public String getUsedGearPattern() {
    return usedGearPattern;
  }

  public void setUsedGearPattern(String usedGearPattern) {
    this.usedGearPattern = usedGearPattern;
  }
}
