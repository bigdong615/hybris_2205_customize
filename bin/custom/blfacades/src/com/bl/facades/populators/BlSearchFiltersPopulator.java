package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.google.common.base.Splitter;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.IndexedPropertyValueData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SearchQueryPageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchFilterQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryTermData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchRequest;
import de.hybris.platform.commerceservices.search.solrfacetsearch.populators.SearchFiltersPopulator;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.IndexedType;
import de.hybris.platform.solrfacetsearch.search.SearchQuery;
import de.hybris.platform.util.Config;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;


/**
 * @author ManiKandan
 * The Populator Added for Adding extra parameters to Solr Query
 */

public class BlSearchFiltersPopulator<FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_SORT_TYPE> extends
    SearchFiltersPopulator<FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_SORT_TYPE> {

  /**
   * Overrided the OOB populate to customize the solr parameters
   */
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
    // BL-80 Add category restriction for used and rental gear category
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

  /**
   * This Method add forRent Property is true when we hit rental gear page
   */
  private void addFilterQueryFalse(
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    target.getSearchQuery().addFilterQuery(BlCoreConstants.FOR_RENT, BlCoreConstants.TRUE);
    target.getSearchQuery().addFilterQuery(BlCoreConstants.ITEM_TYPE, BlCoreConstants.BLPRODUCT);
  }

  /**
   * This Method add forSale Property is true when we hit usedgear Page
   */
  private void addFilterQueryTrue(
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    // Added condition for used gear video category
    if(BlCoreConstants.USED_VIDEO.equalsIgnoreCase(target.getSearchQueryData().getCategoryCode())) {
      target.getSearchQuery().addFilterQuery(BlCoreConstants.IS_VIDEO, BlCoreConstants.TRUE);
    }
    // Added Condition for used gear new arrivals category
    if(BlCoreConstants.USED_NEW_ARRIVALS.equalsIgnoreCase(target.getSearchQueryData().getCategoryCode())) {
      target.getSearchQuery().addFilterQuery(BlCoreConstants.IS_NEW, BlCoreConstants.TRUE);
    }
    target.getSearchQuery().addFilterQuery(BlCoreConstants.FOR_SALE, BlCoreConstants.TRUE);
    target.getSearchQuery().addFilterQuery(BlCoreConstants.ITEM_TYPE, BlCoreConstants.BLPRODUCT);
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

  /**
   * To check the category mapping for used gear categories
   */
  private String checkCategory(String categoryCode) {
    String categoryParam = Config.getParameter(BlCoreConstants.CATEGORY_MAP);
    final Map<String, String> categoryCodeMap = Splitter.on(BlCoreConstants.DELIMETER).withKeyValueSeparator(BlCoreConstants.RATIO).split(categoryParam);
    return categoryCodeMap.get(categoryCode);
  }

  /**
   * To check whether the category is used gear
   */
  private boolean isUsedGearCategory(String categoryCode) {
    return categoryCode.startsWith(BlCoreConstants.USED) || BlCoreConstants.USED_CATEGORY_CODE
        .equalsIgnoreCase(categoryCode);
  }

  /**
   * Adding some category restriction for used gear categories
   */
  private void categoryRestriction(final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    if (target.getSearchQueryData().getCategoryCode() != null && ! isUsedGearCategory(target.getSearchQueryData().getCategoryCode()))
    {
      rentalCategory(target);
      addFilterQueryFalse(target);
    }
    else if (target.getSearchQueryData().getCategoryCode() != null){
        String catCode = target.getSearchQueryData().getCategoryCode();
        if(! BlCoreConstants.USED_NEW_ARRIVALS.equalsIgnoreCase(catCode) && ! BlCoreConstants.USED_CATEGORY_CODE.equalsIgnoreCase(catCode)
            && ! BlCoreConstants.USED_VIDEO.equalsIgnoreCase(catCode)) {
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

  private void rentalCategory(final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    final String categoryCode = target.getSearchQueryData().getCategoryCode();
    if(!BlCoreConstants.RENTAL_GEAR.equalsIgnoreCase(categoryCode)) {
      if(categoryCode.startsWith("New")) {
        String categoryParam = Config.getParameter("key.rentalgear.new");
        final Map<String, String> categoryCodeMap = Splitter.on(BlCoreConstants.DELIMETER).withKeyValueSeparator(BlCoreConstants.RATIO).split(categoryParam);
        target.getSearchQuery().addFilterQuery(BlCoreConstants.ALL_CATEGORIES, categoryCodeMap.get(categoryCode));
        target.getSearchQuery().addFilterQuery(BlCoreConstants.IS_NEW, BlCoreConstants.TRUE);
      }
      else {
        target.getSearchQuery().addFilterQuery(BlCoreConstants.ALL_CATEGORIES,
            categoryCode);
      }
    }
  }
}

