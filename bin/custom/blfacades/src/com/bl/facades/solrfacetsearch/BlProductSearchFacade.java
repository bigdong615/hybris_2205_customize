package com.bl.facades.solrfacetsearch;

import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.search.ProductSearchFacade;
import de.hybris.platform.commercefacades.search.data.SearchStateData;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData;

public interface BlProductSearchFacade<ITEM extends ProductData> extends ProductSearchFacade<ITEM> {

  ProductSearchPageData<SearchStateData, ITEM> textSearch(String text, SearchQueryContext searchQueryContext , String blPageType);
}
