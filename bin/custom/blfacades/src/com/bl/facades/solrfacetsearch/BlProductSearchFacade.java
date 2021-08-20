package com.bl.facades.solrfacetsearch;

import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.search.ProductSearchFacade;
import de.hybris.platform.commercefacades.search.data.SearchStateData;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData;

/**
 * @author Manikandan
 * This interface created for adding custom parameter for method
 */

public interface BlProductSearchFacade<ITEM extends ProductData> extends ProductSearchFacade<ITEM> {

  /**
   * This method created for adding custom parameter
   */
  ProductSearchPageData<SearchStateData, ITEM> textSearch(final String text, final SearchQueryContext searchQueryContext , final String blPageType);
}
