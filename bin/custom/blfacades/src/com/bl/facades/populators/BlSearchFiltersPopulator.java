package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.google.common.base.Splitter;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.FilterQueryOperator;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.IndexedPropertyValueData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SearchQueryPageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchFilterQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryTermData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchRequest;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.IndexedType;
import de.hybris.platform.solrfacetsearch.search.QueryField;
import de.hybris.platform.solrfacetsearch.search.SearchQuery;
import de.hybris.platform.util.Config;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;

public class BlSearchFiltersPopulator<FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_SORT_TYPE> implements
    Populator<SearchQueryPageableData<SolrSearchQueryData>, SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE>> {

  @Override
  public void populate(final SearchQueryPageableData<SolrSearchQueryData> source,
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    // Convert the facet filters into IndexedPropertyValueData
    final List<IndexedPropertyValueData<IndexedProperty>> indexedPropertyValues = new ArrayList<>();
    final Map<String, SolrSearchFilterQueryData> filterQueriesMap = new HashMap<>();
    final List<SolrSearchQueryTermData> terms = target.getSearchQueryData().getFilterTerms();
    target.setIndexedPropertyValues(addTerms(terms, target, indexedPropertyValues));

    populateFilterQueries(target.getSearchQueryData(), filterQueriesMap);

    // Add the facet filters
    for (final IndexedPropertyValueData<IndexedProperty> indexedPropertyValue : target
        .getIndexedPropertyValues()) {
      target.getSearchQuery().addFacetValue(indexedPropertyValue.getIndexedProperty().getName(),
          indexedPropertyValue.getValue());
    }

    // Add category restriction
    categoryRestriction(target);


    // Add filter queries
    final List<SolrSearchFilterQueryData> filterQueries = target.getSearchQueryData()
        .getFilterQueries();

    if (CollectionUtils.isNotEmpty(filterQueries)) {
      for (final SolrSearchFilterQueryData solrSearchFilterQuery : filterQueries) {
        target.getSearchQuery().addFilterQuery(convertFilterQuery(solrSearchFilterQuery));
      }
    }
  }

  private void populateFilterQueries(final SolrSearchQueryData solrSearchQueryData,
      final Map<String, SolrSearchFilterQueryData> filterQueriesMap) {
    if (solrSearchQueryData.getFilterQueries() == null) {
      solrSearchQueryData.setFilterQueries(new ArrayList<>());
    }

    solrSearchQueryData.getFilterQueries().addAll(filterQueriesMap.values());
  }

  private QueryField convertFilterQuery(final SolrSearchFilterQueryData solrSearchFilterQuery) {
    final FilterQueryOperator queryOperator = solrSearchFilterQuery.getOperator();

    final SearchQuery.Operator operator;

    if (queryOperator != null) {
      operator = SearchQuery.Operator.valueOf(queryOperator.toString());
    } else {
      operator = SearchQuery.Operator.AND;
    }

    return new QueryField(solrSearchFilterQuery.getKey(), operator,
        solrSearchFilterQuery.getValues());
  }

  private void addFilterQueryFalse(
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    target.getSearchQuery().addFilterQuery("forRent", String.valueOf(true));
    target.getSearchQuery().addFilterQuery("itemtype", "BlProduct");
  }

  private void addFilterQueryTrue(
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    if("Used-Video".equalsIgnoreCase(target.getSearchQueryData().getCategoryCode())) {
      target.getSearchQuery().addFilterQuery("isVideo", String.valueOf(true));
    }
    if("Used-NewArrivals".equalsIgnoreCase(target.getSearchQueryData().getCategoryCode())) {
      target.getSearchQuery().addFilterQuery("isNew", String.valueOf(true));
    }
    target.getSearchQuery().addFilterQuery("forSale", String.valueOf(true));
    target.getSearchQuery().addFilterQuery("itemtype", "BlProduct");
  }

  private List<IndexedPropertyValueData<IndexedProperty>> addTerms(
      List<SolrSearchQueryTermData> terms,
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target,
      List<IndexedPropertyValueData<IndexedProperty>> indexedPropertyValues) {
    if (terms != null && !terms.isEmpty()) {
      for (final SolrSearchQueryTermData term : terms) {
        final IndexedProperty indexedProperty = target.getIndexedType().getIndexedProperties()
            .get(term.getKey());

        if (indexedProperty != null) {
          final IndexedPropertyValueData<IndexedProperty> indexedPropertyValue = new IndexedPropertyValueData<>();
          indexedPropertyValue.setIndexedProperty(indexedProperty);
          indexedPropertyValue.setValue(term.getValue());
          indexedPropertyValues.add(indexedPropertyValue);
        }
      }
    }
    return indexedPropertyValues;
  }

  private String checkCategory(String categoryCode) {

    String categoryParam = Config.getParameter("usedgear.rentalgear.map");
    final Map<String, String> categoryCodeMap = Splitter.on(BlCoreConstants.DELIMETER).withKeyValueSeparator(BlCoreConstants.RATIO).split(categoryParam);
    return categoryCodeMap.get(categoryCode);
  }

  private boolean isUsedGearCategory(String categoryCode) {
    return categoryCode.startsWith("Used") || BlCoreConstants.USED_CATEGORY_CODE
        .equalsIgnoreCase(categoryCode);
  }

  private void categoryRestriction(final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    if (target.getSearchQueryData().getCategoryCode() != null && ! isUsedGearCategory(target.getSearchQueryData().getCategoryCode()))
    {
      target.getSearchQuery().addFilterQuery(BlCoreConstants.ALL_CATEGORIES, target.getSearchQueryData().getCategoryCode());
      addFilterQueryFalse(target);
    }
    else if (target.getSearchQueryData().getCategoryCode() != null){
        String catCode = target.getSearchQueryData().getCategoryCode();
        if(!"Used-NewArrivals".equalsIgnoreCase(catCode) && !"usedgear".equalsIgnoreCase(catCode) && !"Used-Video".equalsIgnoreCase(catCode)) {
          target.getSearchQuery().addFilterQuery(BlCoreConstants.ALL_CATEGORIES,
              checkCategory(target.getSearchQueryData().getCategoryCode()));
        }
         addFilterQueryTrue(target);
    }
    if(null == target.getSearchQueryData().getCategoryCode()) {
      if (BlCoreConstants.USED_CATEGORY_CODE.equalsIgnoreCase(target.getSearchQueryData().getBlPage())) {
        addFilterQueryTrue(target);
      }
      else {
        addFilterQueryFalse(target);
      }
    }

  }
}

