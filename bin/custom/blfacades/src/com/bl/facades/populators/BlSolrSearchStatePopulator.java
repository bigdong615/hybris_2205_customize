package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.commercefacades.product.data.CategoryData;
import de.hybris.platform.commercefacades.search.solrfacetsearch.converters.populator.SolrSearchStatePopulator;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.url.UrlResolver;

/**
 * @author ManiKandan
 *
 * This populator override for category URL resolver for usedgear categories
 */
public class BlSolrSearchStatePopulator extends SolrSearchStatePopulator
{
  private UrlResolver<CategoryData> blDefaultCategoryDataUrlResolver;

  /**
   * BL-80 This Method Override for UsedGearCategory Resolver
   */
  @Override
protected String getCategoryUrl(final SolrSearchQueryData source)
{
  final CategoryData categoryData = new CategoryData();
  categoryData.setCode(source.getCategoryCode());
  // BL-80 Added Condition for Used Gear Categories
  if(BlCoreConstants.USED_CATEGORY_CODE.equalsIgnoreCase(source.getCategoryCode()) || source.getCategoryCode().startsWith(BlCoreConstants.USED)) {
    return getBlDefaultCategoryDataUrlResolver().resolve(categoryData);
  }
  return getCategoryDataUrlResolver().resolve(categoryData);
}

  private UrlResolver<CategoryData> getBlDefaultCategoryDataUrlResolver() {
    return blDefaultCategoryDataUrlResolver;
  }

  public void setBlDefaultCategoryDataUrlResolver(
      UrlResolver<CategoryData> blDefaultCategoryDataUrlResolver) {
    this.blDefaultCategoryDataUrlResolver = blDefaultCategoryDataUrlResolver;
  }

}

