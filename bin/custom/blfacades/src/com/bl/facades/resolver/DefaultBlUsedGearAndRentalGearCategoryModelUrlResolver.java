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
   * This Method is overiden for ressolving URL pattern for used gear category , rental gear
   */
  @Override
  protected String resolveInternal(final CategoryModel source) {
    // if source is rental get the url for rental gear else for usedgear
    if (source.isRentalCategory()) {
      return CollectionUtils.isEmpty(source.getSupercategories()) ? getCategoryUrl(
          getDefaultPattern(), source) : getCategoryUrl(getRentalGearPattern(), source);
    } else {
      return getCategoryUrl(getUsedGearPattern(), source);
    }
  }

  /**
   * This method is created to get the url for rental and used gear category
   * @param url url pattern
   * @param source defines categorymodel
   * @return String of url
   */
  private String getCategoryUrl(String url ,final CategoryModel source) {

    if (url.contains(BlCoreConstants.BASE_SITE_UID)) {
      url = url.replace(BlCoreConstants.BASE_SITE_UID, urlEncode(getBaseSiteUid().toString()));
    }
    // Add to check whether the url contains level 1 category code for rental gear
    if (url.contains(BlCoreConstants.PARENT_CATEGORY)) {
        for (CategoryModel superCategory : source.getSupercategories()) {
          if(superCategory instanceof CategoryModel) {
            final String parentCategoryCode = urlEncode(superCategory.getCode().toLowerCase())
                .replaceAll("\\+", BlCoreConstants.REPLACE_STRING);
            url = url.replace(BlCoreConstants.PARENT_CATEGORY, parentCategoryCode);
            break;
          }
        }
    }
    // added to check whether the url contains category code for rental and used gear
    if (url.contains(BlCoreConstants.CATEGORY_PATTERN_CODE)) {
      final String categoryCode = urlEncode(source.getCode().toLowerCase())
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
