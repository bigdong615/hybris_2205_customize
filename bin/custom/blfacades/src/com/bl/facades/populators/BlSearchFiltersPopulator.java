package com.bl.facades.populators;

import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.category.CommerceCategoryService;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.IndexedPropertyValueData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SearchQueryPageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchFilterQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryTermData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchRequest;
import de.hybris.platform.commerceservices.search.solrfacetsearch.populators.SearchFiltersPopulator;
import de.hybris.platform.core.model.security.PrincipalGroupModel;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.IndexedType;
import de.hybris.platform.solrfacetsearch.config.IndexedTypeSort;
import de.hybris.platform.solrfacetsearch.config.IndexedTypeSortField;
import de.hybris.platform.solrfacetsearch.search.SearchQuery;
import de.hybris.platform.util.Config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;

import com.bl.core.constants.BlCoreConstants;
import com.google.common.base.Splitter;


/**
 * The Populator Added for Adding extra parameters to Solr Query to fetch the results from solr
 * @author Manikandan
 */


public class BlSearchFiltersPopulator<FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_SORT_TYPE> extends
    SearchFiltersPopulator<FACET_SEARCH_CONFIG_TYPE, INDEXED_TYPE_SORT_TYPE> {

  private CommerceCategoryService commerceCategoryService;
  private UserService userService;
  private SessionService sessionService;

  /**
   * This method is overriden for adding search filter query to existing query to fetch results accordingly
   *
   * @param source
   *           determines the SolrSearchQueryData
   * @param target
   *           determines the SolrSearchRequest
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
    addCategoryRestriction(target);
    if(!getUserService().isAnonymousUser(getUserService().getCurrentUser())){
      addUserGroupRestrictionForProduct(target);
    }
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
   *  Add restricted user groups to exclude products from results
   * @param target
   */
  private void addUserGroupRestrictionForProduct( final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery,INDEXED_TYPE_SORT_TYPE> target) {
    final StringBuilder stringBuilder = new StringBuilder();
    final Optional<PrincipalGroupModel> blGroup = getUserService().getCurrentUser().getGroups().stream()
        .filter(group -> StringUtils.containsIgnoreCase(group.getUid(), BlCoreConstants.BL_GROUP)).findAny();
    if (blGroup.isPresent()) {
      final String rawQuery = stringBuilder.append(BlCoreConstants.RESTRICTED_PRINCIPALS_STRING_MV).append(BlCoreConstants.COLON).append(blGroup.get().getUid()).toString();
      target.getSearchQuery().addFilterRawQuery(rawQuery);
    }
  }

  /**
   * This Method is created for adding term into indexedPropertyValues to prepare solr query
   */
  private List<IndexedPropertyValueData<IndexedProperty>> addTerms(
      final List<SolrSearchQueryTermData> terms,
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target,
      final List<IndexedPropertyValueData<IndexedProperty>> indexedPropertyValues) {
    if (CollectionUtils.isNotEmpty(terms)) {
      for (final SolrSearchQueryTermData term : terms) {
        final IndexedProperty indexedProperty = target.getIndexedType().getIndexedProperties()
            .get(term.getKey());
        if (null != indexedProperty) {
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
   * This method is created for adding restriction for usedgear and rentalgear cateory
   * @param target target objects to fill
   */

  private void addCategoryRestriction(final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {

    final String categoryCode = target.getSearchQueryData().getCategoryCode();
    if (StringUtils.isBlank(categoryCode)) {
      addSaleAndRentalQueryParameters(target);
    } else {
      addSearchParameterTrueForRentalPages(target, categoryCode);
    }
  }

  /**
   *  this method is created for adding forRent as true for all rental categories
   * @param target target object to fill
   * @param categoryCode defines catehoryCode
   */
  private void addSearchParameterTrueForRentalPages(
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target,
     final String categoryCode) {
    final CategoryModel category = getCommerceCategoryService().getCategoryForCode(categoryCode);
    addQueryForCategory(target,BlCoreConstants.IS_GIFT_CARD,BlCoreConstants.FALSE);
    if (category.isRentalCategory()) {
      if (!BlCoreConstants.RENTAL_GEAR.equalsIgnoreCase(categoryCode)) {
        addIsNewToRentalSearchQuery(target, categoryCode);
      }
      addQueryForCategory(target,BlCoreConstants.FOR_RENT,BlCoreConstants.TRUE);
      addSaleAndRentQuery(target);
    }
    else if (BlCoreConstants.NEW_GEAR.equalsIgnoreCase(categoryCode)) {
      addQueryForCategory(target,BlCoreConstants.RETAILGEAR,BlCoreConstants.TRUE);
    }
    else {
      if (!BlCoreConstants.USED_NEW_ARRIVALS.equalsIgnoreCase(categoryCode)
          && !BlCoreConstants.USED_GEAR_CODE.equalsIgnoreCase(categoryCode)
          && !BlCoreConstants.USED_VIDEO.equalsIgnoreCase(categoryCode)) {
        getCategoryFromProperties(target,categoryCode,BlCoreConstants.CATEGORY_MAP);
      }
      addFilterQueryTrue(target, categoryCode);
    }
  }


  /**
   *  this method is created for adding forSale as true for used gear categories
   * @param target target object to fill
   */
  private void addSaleAndRentalQueryParameters(
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
    if (BlCoreConstants.USED_GEAR_CODE.equalsIgnoreCase(target.getSearchQueryData().getBlPage())) {
      addQueryForCategory(target,BlCoreConstants.FOR_SALE,BlCoreConstants.TRUE);
      addSaleAndRentQuery(target);
    } else {
      addQueryForCategory(target,BlCoreConstants.FOR_RENT,BlCoreConstants.TRUE);
		if (getSessionService().getAttribute("isApliCall") == null)
		{
			addQueryForCategory(target, BlCoreConstants.IS_DISCONTINUED, BlCoreConstants.FALSE);
		}
      addSaleAndRentQuery(target);
    }
  }

  /**
   * This method is craeted for adding IsNew to search query to get the product set as IsNew true for rental category
   */
  private void addIsNewToRentalSearchQuery(
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target ,final String categoryCode) {
      if (categoryCode.startsWith(BlCoreConstants.NEW)) {
        getCategoryFromProperties(target,categoryCode,BlCoreConstants.RENTAL_GEAR_MAP);
        target.getSearchQuery().addFilterQuery(BlCoreConstants.IS_NEW, BlCoreConstants.TRUE);
      } else {
        target.getSearchQuery().addFilterQuery(BlCoreConstants.ALL_CATEGORIES,
            categoryCode);
      }
  }

  /**
   * this method is created for getting values from the properties based on rental or used gear category
   * @param target target object to be fill
   * @param categoryCode categoryCode
   * @param paramMap defines the property key
   */
  private void getCategoryFromProperties(final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target ,
      final String categoryCode,final String paramMap) {
    final String categoryParam = Config.getParameter(paramMap);
    if(StringUtils.isNotBlank(categoryParam)) {
      final Map<String, String> categoryCodeMap = Splitter.on(BlCoreConstants.DELIMETER)
          .withKeyValueSeparator(BlCoreConstants.RATIO).split(categoryParam);
      if(StringUtils.isNotBlank(categoryCodeMap.get(categoryCode))) {
        target.getSearchQuery()
            .addFilterQuery(BlCoreConstants.ALL_CATEGORIES, categoryCodeMap.get(categoryCode));
      }
    }
  }

  /**
   * this method created for adding boolean true for used video and used new arrivals to query
   * @param target target object to fill
   * @param categoryCode category code
   */
  private void addFilterQueryTrue(
      final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target ,final String categoryCode) {
    // Added condition for used gear video category
    if (BlCoreConstants.USED_VIDEO.equalsIgnoreCase(categoryCode)) {
      addQueryForCategory(target,BlCoreConstants.IS_VIDEO, BlCoreConstants.TRUE);
    }
    // Added Condition for used gear new arrivals category
    if (BlCoreConstants.USED_NEW_ARRIVALS.equalsIgnoreCase(categoryCode)) {
      addQueryForCategory(target,BlCoreConstants.IS_NEW, BlCoreConstants.TRUE);
    }
    addQueryForCategory(target,BlCoreConstants.FOR_SALE, BlCoreConstants.TRUE);
    addSaleAndRentQuery(target);
  }

  /*
   * this method is created for adding itemType to solr query
   */
  private void addSaleAndRentQuery(final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target) {
	  if (getSessionService().getAttribute("isApliCall") == null)
	  {
		  target.getSearchQuery().addFilterQuery(BlCoreConstants.ITEM_TYPE, BlCoreConstants.BLPRODUCT);
		  target.getSearchQuery().addFilterQuery("productTypeExclusion", BlCoreConstants.FALSE);
	  }
  }

  /*
   * This method is created for adding values dynamically
   */
  private void addQueryForCategory(final SolrSearchRequest<FACET_SEARCH_CONFIG_TYPE, IndexedType, IndexedProperty, SearchQuery, INDEXED_TYPE_SORT_TYPE> target ,final String key , final String value) {
    target.getSearchQuery().addFilterQuery(key,value);
    // Added for Rental Gear and Used Gear sorting field
          String sortName = BlCoreConstants.EMPTY_STRING;
          if(null != target.getPageableData() && null != target.getPageableData().getSort()) {
           sortName = target.getPageableData().getSort();
          }
          else if(null != target.getSearchQueryData() && null != target.getSearchQueryData().getSort()) {
             sortName = target.getSearchQueryData().getSort();
          }

        for (final IndexedTypeSort indexedTypeSorts : target.getSearchQuery().getIndexedType().getSorts()) {
          if(indexedTypeSorts.getCode().equalsIgnoreCase(sortName) && (BlCoreConstants.PRICE_ASC.equalsIgnoreCase(sortName)
          ||  BlCoreConstants.PRICE_DESC.equalsIgnoreCase(sortName))){
            indexedTypeSorts.getFields().clear();
            final List<IndexedTypeSortField> fields = new ArrayList<>();
            final IndexedTypeSortField indexedTypeSortField = new IndexedTypeSortField();
            indexedTypeSortField.setFieldName(BlCoreConstants.FOR_SALE.equalsIgnoreCase(key) ? BlCoreConstants.MIN_SERIAL_PRICE : BlCoreConstants.PRICE_VALUE);
            indexedTypeSortField.setAscending(BlCoreConstants.PRICE_ASC.equalsIgnoreCase(sortName));
            fields.add(indexedTypeSortField);
            indexedTypeSorts.setFields(fields);
          }
        }
  }


  public CommerceCategoryService getCommerceCategoryService() {
    return commerceCategoryService;
  }

  public void setCommerceCategoryService(
      final CommerceCategoryService commerceCategoryService) {
    this.commerceCategoryService = commerceCategoryService;
  }

  public UserService getUserService() {
    return userService;
  }

  public void setUserService(final UserService userService) {
    this.userService = userService;
  }

  public SessionService getSessionService()
  {
	  return sessionService;
  }

  public void setSessionService(final SessionService sessionService)
  {
	  this.sessionService = sessionService;
  }

}

