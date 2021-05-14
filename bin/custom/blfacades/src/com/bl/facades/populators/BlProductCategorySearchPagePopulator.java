package com.bl.facades.populators;

import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.search.converters.populator.ProductCategorySearchPagePopulator;
import de.hybris.platform.commerceservices.search.facetdata.ProductCategorySearchPageData;
import de.hybris.platform.commerceservices.search.resultdata.SearchResultValueData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;

/**
 * This Class created to populate custom attribute to search results
 * @author Manikandan
 */
public class BlProductCategorySearchPagePopulator<QUERY, STATE, RESULT, ITEM extends ProductData, SCAT, CATEGORY> //NOSONAR
    extends ProductCategorySearchPagePopulator<QUERY, STATE, RESULT, ProductData, SCAT, CATEGORY> {

  /**
   * This method created to populate custom attribut to search results
   * @param source the source object
   * @param target the target to fill
   */
  @Override
  public void populate(final ProductCategorySearchPageData<QUERY, RESULT, SCAT> source,
      final ProductCategorySearchPageData<STATE, ProductData, CATEGORY> target) {
    final String blPageType = ((SolrSearchQueryData) source.getCurrentQuery()).getBlPage();
    for (final RESULT result : source.getResults()) {
      final SearchResultValueData searchResultValueData = (SearchResultValueData) result;
      searchResultValueData.setBlPage(blPageType);
    }
    super.populate(source, target);

  }
}
