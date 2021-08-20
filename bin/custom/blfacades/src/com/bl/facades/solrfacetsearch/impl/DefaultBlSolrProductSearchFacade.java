package com.bl.facades.solrfacetsearch.impl;

import com.bl.core.services.solrfacetsearch.BlProductSearchService;
import com.bl.facades.solrfacetsearch.BlProductSearchFacade;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.search.data.SearchStateData;
import de.hybris.platform.commercefacades.search.solrfacetsearch.impl.DefaultSolrProductSearchFacade;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.facetdata.ProductSearchPageData;
import de.hybris.platform.commerceservices.threadcontext.ThreadContextService;

/**
 * This class created for adding custom parameter for auto sugesstion
 * @author Manikandan
 */
public class DefaultBlSolrProductSearchFacade<ITEM extends ProductData> extends DefaultSolrProductSearchFacade<ITEM> implements
    BlProductSearchFacade<ITEM> {

  private BlProductSearchService blProductSearchService;

  /**
   * This method created to add the custom parameter for auto sugesstion search
   */
  @Override
  public ProductSearchPageData textSearch(final String text, final SearchQueryContext searchQueryContext, final String blPageType) { // NOSONAR
      return getThreadContextService().executeInContext(
          new ThreadContextService.Executor<ProductSearchPageData<SearchStateData, ITEM>, ThreadContextService.Nothing>()
          {
            @Override
            public ProductSearchPageData<SearchStateData, ITEM> execute()
            {
              return getProductCategorySearchPageConverter()
                  .convert(getBlProductSearchService().textSearch(text, searchQueryContext, null , blPageType));
            }
          });
    }

  public BlProductSearchService getBlProductSearchService() {
    return blProductSearchService;
  }

  public void setBlProductSearchService(
      BlProductSearchService blProductSearchService) {
    this.blProductSearchService = blProductSearchService;
  }

}
