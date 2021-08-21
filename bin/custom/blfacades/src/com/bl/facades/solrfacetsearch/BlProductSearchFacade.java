package com.bl.facades.solrfacetsearch;

import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.search.ProductSearchFacade;
import de.hybris.platform.commercefacades.search.data.SearchStateData;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData;

/**
 * This interface created for adding custom parameter for method
 *  @author Manikandan
 */

public interface BlProductSearchFacade<ITEM extends ProductData> extends ProductSearchFacade<ITEM> {

  /**
   * This method created for adding custom parameter
   */
  ProductSearchPageData<SearchStateData, ITEM> textSearch(final String text, final SearchQueryContext searchQueryContext , final String blPageType);
}
