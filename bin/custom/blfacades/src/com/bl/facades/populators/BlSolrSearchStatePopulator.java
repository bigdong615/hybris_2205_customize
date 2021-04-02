package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import de.hybris.platform.commercefacades.product.data.CategoryData;
import de.hybris.platform.commercefacades.search.data.SearchQueryData;
import de.hybris.platform.commercefacades.search.data.SearchStateData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.url.UrlResolver;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.StringUtils;

/**
 * @author ManiKandan
 *
 * This populator added for category URL resolver for usedgear categories
 */
public class BlSolrSearchStatePopulator implements Populator<SolrSearchQueryData, SearchStateData>
{
  private String searchPath;
  private UrlResolver<CategoryData> categoryDataUrlResolver;
  private UrlResolver<CategoryData> blDefaultCategoryDataUrlResolver;
  private Converter<SolrSearchQueryData, SearchQueryData> searchQueryConverter;

  @Override
  public void populate(final SolrSearchQueryData source, final SearchStateData target)
  {
    target.setQuery(getSearchQueryConverter().convert(source));

    if (source.getCategoryCode() != null)
    {
      populateCategorySearchUrl(source, target);
    }
    else
    {
      populateFreeTextSearchUrl(source, target);
    }
  }

  protected void populateCategorySearchUrl(final SolrSearchQueryData source, final SearchStateData target)
  {
    target.setUrl(getCategoryUrl(source) + buildUrlQueryString(source, target));
  }

  protected void populateFreeTextSearchUrl(final SolrSearchQueryData source, final SearchStateData target)
  {
    target.setUrl(getSearchPath() + buildUrlQueryString(source, target));
  }


  protected String getCategoryUrl(final SolrSearchQueryData source)
  {
    final CategoryData categoryData = new CategoryData();
    categoryData.setCode(source.getCategoryCode());
    // BL-80 Added Condition for Used Gear Categories
    if(BlCoreConstants.USED_CATEGORY_CODE.equalsIgnoreCase(source.getCategoryCode()) || source.getCategoryCode().startsWith("Used")) {
         return getBlDefaultCategoryDataUrlResolver().resolve(categoryData);
    }
    return getCategoryDataUrlResolver().resolve(categoryData);
  }

  protected String buildUrlQueryString(final SolrSearchQueryData source, final SearchStateData target)
  {
    final String searchQueryParam = target.getQuery().getValue();
    if (StringUtils.isNotBlank(searchQueryParam))
    {
      try
      {
        return "?q=" + URLEncoder.encode(searchQueryParam, "UTF-8");
      }
      catch (final UnsupportedEncodingException e)
      {
        return "?q=" + StringEscapeUtils.escapeHtml(searchQueryParam);
      }
    }
    return "";
  }



  protected String getSearchPath()
  {
    return searchPath;
  }


  public void setSearchPath(final String searchPath)
  {
    this.searchPath = searchPath;
  }

  protected UrlResolver<CategoryData> getCategoryDataUrlResolver()
  {
    return categoryDataUrlResolver;
  }

  public void setCategoryDataUrlResolver(final UrlResolver<CategoryData> categoryDataUrlResolver)
  {
    this.categoryDataUrlResolver = categoryDataUrlResolver;
  }

  protected Converter<SolrSearchQueryData, SearchQueryData> getSearchQueryConverter()
  {
    return searchQueryConverter;
  }

  public void setSearchQueryConverter(final Converter<SolrSearchQueryData, SearchQueryData> searchQueryConverter)
  {
    this.searchQueryConverter = searchQueryConverter;
  }

  public UrlResolver<CategoryData> getBlDefaultCategoryDataUrlResolver() {
    return blDefaultCategoryDataUrlResolver;
  }

  public void setBlDefaultCategoryDataUrlResolver(
      UrlResolver<CategoryData> blDefaultCategoryDataUrlResolver) {
    this.blDefaultCategoryDataUrlResolver = blDefaultCategoryDataUrlResolver;
  }

}

