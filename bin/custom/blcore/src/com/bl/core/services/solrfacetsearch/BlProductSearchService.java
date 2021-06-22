package com.bl.core.services.solrfacetsearch;

import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.ProductSearchService;
import de.hybris.platform.commerceservices.search.facetdata.ProductCategorySearchPageData;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;

/**
 * This Interface Created to create custom method to set custom parameter
 * @author Manikandan
 */
public interface BlProductSearchService<ITEM> extends
    ProductSearchService<SolrSearchQueryData, ITEM, ProductCategorySearchPageData<SolrSearchQueryData, ITEM, CategoryModel>> {

  /**
   * This method created to populate custom parameter
   */
  ProductCategorySearchPageData<SolrSearchQueryData, ITEM, CategoryModel> textSearch(final String text, final SearchQueryContext searchQueryContext, final PageableData pageableData , final String blPageType); //NOSONAR
}
