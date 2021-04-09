package com.bl.facades.resolver;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.url.impl.DefaultCategoryModelUrlResolver;
import org.apache.commons.collections4.CollectionUtils;

/**
 * @author Manikandan This Class created to override the OOB DefaultCategoryModelUrlResolver to get
 * URL pattern for used gear category
 */
public class DefaultBlUsedGearAndRentalGearCategoryModelUrlResolver extends
    DefaultCategoryModelUrlResolver {

  private String usedGearPattern;
  private String rentalGearPattern;
  private String defaultPattern;

  /**
   * This Method is overiden for ressolving URL pattern for used gear category
   */
  @Override
  protected String resolveInternal(final CategoryModel source) {
    String url = "";
    if (!source.isRentalCategory()) {
      url = getUsedGearPattern();
    } else if (source.isRentalCategory() && CollectionUtils.isEmpty(source.getSupercategories())) {
      url = getDefaultPattern();
    } else if (source.isRentalCategory()) {
      url = getRentalGearPattern();
    }
    if (url.contains(BlCoreConstants.BASE_SITE_UID)) {
      url = url.replace(BlCoreConstants.BASE_SITE_UID, urlEncode(getBaseSiteUid().toString()));
    }
    if (url.contains(BlCoreConstants.PARENT_CATEGORY)) {
      String parentCode = "";
      for (CategoryModel superCategory : source.getSupercategories()) {
        if (!BlCoreConstants.USED_GEAR_CODE.equalsIgnoreCase(superCategory.getName())) {
          parentCode = superCategory.getCode();
        }
      }
      final String parentCategoryCode = urlEncode(parentCode)
          .replaceAll("\\+", BlCoreConstants.REPLACE_STRING);
      url = url.replace(BlCoreConstants.PARENT_CATEGORY, parentCategoryCode);
    }
    if (url.contains(BlCoreConstants.CATEGORY_PATTERN_CODE)) {
      final String categoryCode = urlEncode(source.getCode())
          .replaceAll("\\+", BlCoreConstants.REPLACE_STRING);
      url = url.replace(BlCoreConstants.CATEGORY_PATTERN_CODE, categoryCode);
    }
    if (url.contains(BlCoreConstants.CATALOG_ID)) {
      url = url
          .replace(BlCoreConstants.CATALOG_ID,
              urlEncode(source.getCatalogVersion().getCatalog().getId()));
    }
    if (url.contains(BlCoreConstants.CATALOG_VERSION)) {
      url = url.replace(BlCoreConstants.CATALOG_VERSION,
          urlEncode(source.getCatalogVersion().getVersion()));
    }
    return url;
  }

  // Get The Custom pattern for used gear category
  public String getUsedGearPattern() {
    return usedGearPattern;
  }

  public void setUsedGearPattern(String usedGearPattern) {
    this.usedGearPattern = usedGearPattern;
  }


  public String getRentalGearPattern() {
    return rentalGearPattern;
  }

  public void setRentalGearPattern(String rentalGearPattern) {
    this.rentalGearPattern = rentalGearPattern;
  }

  public String getDefaultPattern() {
    return defaultPattern;
  }

  public void setDefaultPattern(String defaultPattern) {
    this.defaultPattern = defaultPattern;
  }

}
