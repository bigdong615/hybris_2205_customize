package com.bl.core.resolver;

import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commercefacades.product.data.CategoryData;
import de.hybris.platform.commerceservices.category.CommerceCategoryService;
import de.hybris.platform.commerceservices.url.UrlResolver;
import de.hybris.platform.commerceservices.url.impl.AbstractUrlResolver;

/**
 * @author Manikandan
 * This Resolver is added for UsedGear Category
 */
public class BlDefaultCategoryDataUrlResolver extends AbstractUrlResolver<CategoryData> {

  private final String CACHE_KEY = BlDefaultCategoryDataUrlResolver.class.getName(); // NOSONAR

  private CommerceCategoryService commerceCategoryService;
  private UrlResolver<CategoryModel> blDefaultCategoryModelUrlResolver;


  @Override
  protected String getKey(final CategoryData source) {
    return CACHE_KEY + "." + source.getCode();
  }

  /*
   * This Method resolve the URL pattern for UsedGearCategories
   */
  @Override
  protected String resolveInternal(final CategoryData source) {
    final CategoryModel categoryModel = getCommerceCategoryService()
        .getCategoryForCode(source.getCode());
    return getBlDefaultCategoryModelUrlResolver().resolve(categoryModel);
  }

  private UrlResolver<CategoryModel> getBlDefaultCategoryModelUrlResolver() {
    return blDefaultCategoryModelUrlResolver;
  }

  public void setBlDefaultCategoryModelUrlResolver(
      UrlResolver<CategoryModel> blDefaultCategoryModelUrlResolver) {
    this.blDefaultCategoryModelUrlResolver = blDefaultCategoryModelUrlResolver;
  }

  protected CommerceCategoryService getCommerceCategoryService() {
    return commerceCategoryService;
  }


  public void setCommerceCategoryService(final CommerceCategoryService commerceCategoryService) {
    this.commerceCategoryService = commerceCategoryService;
  }
}
