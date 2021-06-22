package com.bl.core.services.strategy;

import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.facetdata.ProductCategorySearchPageData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.SolrFacetSearchProductSearchStrategy;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;

/**
 * This Interface created for custom method to populate custon parameter
 * @author Manikandan
 */
public interface BlSolrFacetSearchProductSearchStrategy<ITEM> extends
    SolrFacetSearchProductSearchStrategy<ITEM> {

  /**
   * This Method created to set the custom value to searchQueryData
   */
  ProductCategorySearchPageData<SolrSearchQueryData, ITEM, CategoryModel> textSearch(String text, SearchQueryContext searchQueryContext, PageableData pageableData , String blPageType);

}
