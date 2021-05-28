package com.bl.facades.populators;

import de.hybris.platform.commerceservices.search.facetdata.FacetSearchPageData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchResponse;
import de.hybris.platform.commerceservices.search.solrfacetsearch.populators.SearchResponseQueryPopulator;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.IndexedTypeSort;

/**
 * This class is created to populate custom attribute
 * @author Manikandan
 */
public class BlSearchResponseQueryPopulator <FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_TYPE, SEARCH_QUERY_TYPE, SEARCH_RESULT_TYPE,ITEM> extends
    SearchResponseQueryPopulator<FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_TYPE, SEARCH_QUERY_TYPE, SEARCH_RESULT_TYPE, ITEM> implements
    Populator<SolrSearchResponse<FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_TYPE, IndexedProperty, SEARCH_QUERY_TYPE, IndexedTypeSort, SEARCH_RESULT_TYPE>, FacetSearchPageData<SolrSearchQueryData, ITEM>>
{

  /**
   * This method created to populate custom attribute to search query data
   * @param source the source object
   * @param target the target to fill
   */
  @Override
  public void populate(
      final SolrSearchResponse<FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_TYPE, IndexedProperty, SEARCH_QUERY_TYPE, IndexedTypeSort, SEARCH_RESULT_TYPE> source,
      final FacetSearchPageData<SolrSearchQueryData, ITEM> target)
  {
    super.populate(source , target);
    target.getCurrentQuery().setBlPage(source.getRequest().getSearchQueryData().getBlPage());
  }

}
