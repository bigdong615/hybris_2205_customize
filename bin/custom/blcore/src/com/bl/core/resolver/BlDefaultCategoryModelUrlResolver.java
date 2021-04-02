package com.bl.core.resolver;

import de.hybris.platform.basecommerce.model.site.BaseSiteModel;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.category.CommerceCategoryService;
import de.hybris.platform.commerceservices.url.impl.AbstractUrlResolver;
import de.hybris.platform.commerceservices.url.impl.DefaultCategoryModelUrlResolver;
import de.hybris.platform.site.BaseSiteService;
import java.util.Collection;
import java.util.List;

/**
 * @author ManiKandan
 *
 * This Resolver is created for usedgear category with different category pattern
 */
public class BlDefaultCategoryModelUrlResolver  extends AbstractUrlResolver<CategoryModel>
{
  private final String CACHE_KEY = DefaultCategoryModelUrlResolver.class.getName(); // NOSONAR

  private CommerceCategoryService commerceCategoryService;
  private BaseSiteService baseSiteService;
  private String pattern;


  @Override
  protected String resolveInternal(final CategoryModel source)
  {
    // Work out values
    // Replace pattern values
    String url = getPattern();
    if (url.contains("{baseSite-uid}"))
    {
      url = url.replace("{baseSite-uid}", urlEncode(getBaseSiteUid().toString()));
    }
    if (url.contains("{category-code}"))
    {
      final String categoryCode = urlEncode(source.getCode()).replaceAll("\\+", "%20");
      url = url.replace("{category-code}", categoryCode);
    }
    if (url.contains("{catalog-id}"))
    {
      url = url.replace("{catalog-id}", urlEncode(source.getCatalogVersion().getCatalog().getId()));
    }
    if (url.contains("{catalogVersion}"))
    {
      url = url.replace("{catalogVersion}", urlEncode(source.getCatalogVersion().getVersion()));
    }

    return url;

  }

  protected CharSequence getBaseSiteUid()
  {
    final BaseSiteModel currentBaseSite = getBaseSiteService().getCurrentBaseSite();
    if (currentBaseSite == null)
    {
      return "{baseSiteUid}";
    }
    else
    {
      return currentBaseSite.getUid();
    }
  }

  protected String buildPathString(final List<CategoryModel> path)
  {
    final StringBuilder result = new StringBuilder();

    for (int i = 0; i < path.size(); i++)
    {
      if (i != 0)
      {
        result.append('/');
      }
      result.append(urlSafe(path.get(i).getName()));
    }

    return result.toString();
  }

  @Override
  protected String getKey(final CategoryModel source)
  {
    return CACHE_KEY + "." + source.getPk().toString();
  }


  protected List<CategoryModel> getCategoryPath(final CategoryModel category)
  {
    final Collection<List<CategoryModel>> paths = getCommerceCategoryService().getPathsForCategory(category);
    // Return first - there will always be at least 1
    return paths.iterator().next();
  }

  protected CommerceCategoryService getCommerceCategoryService()
  {
    return commerceCategoryService;
  }

  public void setCommerceCategoryService(final CommerceCategoryService commerceCategoryService)
  {
    this.commerceCategoryService = commerceCategoryService;
  }

  protected BaseSiteService getBaseSiteService()
  {
    return baseSiteService;
  }

  public void setBaseSiteService(final BaseSiteService baseSiteService)
  {
    this.baseSiteService = baseSiteService;
  }

  protected String getPattern()
  {
    return pattern;
  }

  public void setPattern(final String pattern)
  {
    this.pattern = pattern;
  }

}

