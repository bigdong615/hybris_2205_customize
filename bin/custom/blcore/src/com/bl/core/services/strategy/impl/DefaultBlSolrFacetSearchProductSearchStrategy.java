package com.bl.core.services.strategy.impl;

import com.bl.core.services.strategy.BlSolrFacetSearchProductSearchStrategy;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.facetdata.ProductCategorySearchPageData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryTermData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.impl.DefaultSolrFacetSearchProductSearchStrategy;
import java.util.Collections;

/**
 * This class created to set the custom value to searchQueryData
 * @author Manikandan
 */
public class DefaultBlSolrFacetSearchProductSearchStrategy<ITEM> extends
    DefaultSolrFacetSearchProductSearchStrategy<ITEM> implements
    BlSolrFacetSearchProductSearchStrategy<ITEM> {

  /**
   * {@inheritDoc}
   */

  @Override
  public ProductCategorySearchPageData<SolrSearchQueryData, ITEM, CategoryModel> textSearch(final String text, final SearchQueryContext searchQueryContext,
      final PageableData pageableData, final String blPageType) { //NOSONAR

    final SolrSearchQueryData searchQueryData = createSearchQueryData();
    searchQueryData.setFreeTextSearch(text);
    searchQueryData.setFilterTerms(Collections.<SolrSearchQueryTermData>emptyList());
    searchQueryData.setSearchQueryContext(searchQueryContext);
    searchQueryData.setBlPage(blPageType);

    return doSearch(searchQueryData, pageableData);
  }
}
