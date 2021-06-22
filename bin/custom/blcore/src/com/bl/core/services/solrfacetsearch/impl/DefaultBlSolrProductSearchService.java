package com.bl.core.services.solrfacetsearch.impl;

import com.bl.core.services.solrfacetsearch.BlProductSearchService;
import com.bl.core.services.strategy.impl.DefaultBlSolrFacetSearchProductSearchStrategy;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.facetdata.ProductCategorySearchPageData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.impl.DefaultSolrProductSearchService;

/**
 * This Class class created to override OOB service to set custom parameter
 * @author Manikandan
 */
public class DefaultBlSolrProductSearchService<ITEM> extends
    DefaultSolrProductSearchService<ITEM> implements BlProductSearchService<ITEM> {

  private DefaultBlSolrFacetSearchProductSearchStrategy<ITEM>  defaultBlSolrFacetSearchProductSearchStrategy;

  /**
   * {@inheritDoc}
   *
   */
  @Override
  public ProductCategorySearchPageData<SolrSearchQueryData, ITEM, CategoryModel> textSearch(final String text, final SearchQueryContext searchQueryContext,
      final PageableData pageableData, final String blPageType) { //NOSONAR
    return getDefaultBlSolrFacetSearchProductSearchStrategy().textSearch(text, searchQueryContext, pageableData ,blPageType);
  }


  public DefaultBlSolrFacetSearchProductSearchStrategy getDefaultBlSolrFacetSearchProductSearchStrategy() {
    return defaultBlSolrFacetSearchProductSearchStrategy;
  }

  public void setDefaultBlSolrFacetSearchProductSearchStrategy(
      DefaultBlSolrFacetSearchProductSearchStrategy defaultBlSolrFacetSearchProductSearchStrategy) {
    this.defaultBlSolrFacetSearchProductSearchStrategy = defaultBlSolrFacetSearchProductSearchStrategy;
  }
}
