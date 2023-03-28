package com.bl.facades.populators;

import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.category.CommerceCategoryService;
import de.hybris.platform.commerceservices.search.pagedata.PageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.FilterQueryOperator;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SearchQueryPageableData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchFilterQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryTermData;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchRequest;
import de.hybris.platform.solrfacetsearch.config.FacetSearchConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.IndexedType;
import de.hybris.platform.solrfacetsearch.config.IndexedTypeSort;
import de.hybris.platform.solrfacetsearch.config.IndexedTypeSortField;
import de.hybris.platform.solrfacetsearch.search.QueryField;
import de.hybris.platform.solrfacetsearch.search.SearchQuery;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlSearchFiltersPopulatorTest {

  private static final String KEY1 = "key1";
  private static final String KEY2 = "key2";
  private static final String KEY3 = "key3";
  private static final String KEY4 = "key4";
  private static final String VALUE1 = "value1";
  private static final String VALUE2 = "value2";
  private static final String VALUE3 = "value3";
  private static final String VALUE4 = "value4";

  private static final String CATEGORY_CODE = "canon";
  private static final String USED_CATEGORY_CODE = "Used-NewArrivals";
  private static final String ALL_CATEGORIES = "allCategories";
  private static final String FOR_RENT = "forRent";
  private static final String FOR_SALE = "forSale";
  private static final String ITEM_TYPE = "itemtype";
  private static final String TRUE = "true";
  private static final String BL_PRODUCT = "BlProduct";
  private static final String IS_NEW = "isNew";
  public static final String USED_PAGE ="usedGear";
  public static final String RENTAL_PAGE ="rentalGear";
  private static final String SORT_CODE_PRICE_ASC = "price-asc";
  private static final String SORT_CODE_PRICE_DESC = "price-desc";
  private static final String SORT_CODE_NEWEST = "newest";
  public static final String MIN_SERIAL_PRICE = "minSerialfinalSalePrice";
  public static final String PRICE_VALUE = "priceValue";




  private BlSearchFiltersPopulator<FacetSearchConfig, IndexedTypeSort> blSearchFiltersPopulator;


  @Mock
  private CommerceCategoryService commerceCategoryService;

  @Mock
  private SolrSearchRequest target;

  @Before
  public void startUp() {
	  // MockitoAnnotations.initMocks(this);
    blSearchFiltersPopulator = new BlSearchFiltersPopulator<FacetSearchConfig, IndexedTypeSort>();
    blSearchFiltersPopulator.setCommerceCategoryService(commerceCategoryService);
  }

  // When category code is rental
  @Test
  public void testPopulate() {
    final Set<String> strings = Collections.singleton(CATEGORY_CODE);
    final SearchQueryPageableData<SolrSearchQueryData> source = null;
    final SolrSearchRequest<FacetSearchConfig, IndexedType, IndexedProperty, SearchQuery, IndexedTypeSort> target = populateTarget();
    final CategoryModel categoryModel = new CategoryModel();
    categoryModel.setCode(CATEGORY_CODE);
    categoryModel.setRentalCategory(true);
    when(commerceCategoryService.getCategoryForCode(CATEGORY_CODE)).thenReturn(categoryModel);
    blSearchFiltersPopulator.populate(source, target);
    Assert.assertEquals(CATEGORY_CODE,target.getSearchQueryData().getCategoryCode());

    for(final QueryField values : target.getSearchQuery().getFilterQueries()) {
      if(ALL_CATEGORIES.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(ALL_CATEGORIES,values.getField());
        Assert.assertEquals(strings,values.getValues());
      }
      if(FOR_RENT.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(FOR_RENT,values.getField());
        Assert.assertTrue(values.getValues().contains(TRUE));
      }
      if(ITEM_TYPE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(ITEM_TYPE,values.getField());
        Assert.assertTrue(values.getValues().contains(BL_PRODUCT));
      }

    }
  }


  // When category new arrivals
  @Test
  public void testPopulateWhenUsedGear() {
    final SearchQueryPageableData<SolrSearchQueryData> source = null;
    final SolrSearchRequest<FacetSearchConfig, IndexedType, IndexedProperty, SearchQuery, IndexedTypeSort> target = populateTarget();
    target.getSearchQueryData().setCategoryCode(USED_CATEGORY_CODE);
    final CategoryModel category = new CategoryModel();
    category.setCode(USED_CATEGORY_CODE);
    when(commerceCategoryService.getCategoryForCode(USED_CATEGORY_CODE)).thenReturn(category);
    blSearchFiltersPopulator.populate(source, target);
    Assert.assertEquals(USED_CATEGORY_CODE,target.getSearchQueryData().getCategoryCode());

    for(final QueryField values : target.getSearchQuery().getFilterQueries()) {
      if(IS_NEW.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(IS_NEW,values.getField());
        Assert.assertTrue(values.getValues().contains(TRUE));
      }
      if(FOR_SALE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(FOR_SALE,values.getField());
        Assert.assertTrue(values.getValues().contains(TRUE));
      }
      if(ITEM_TYPE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(ITEM_TYPE,values.getField());
        Assert.assertTrue(values.getValues().contains(BL_PRODUCT));
      }

    }
  }


  // When category code is null and have free text search
  @Test
  public void testPopulateWhenSearch() {
    final SearchQueryPageableData<SolrSearchQueryData> source = null;
    final SolrSearchRequest<FacetSearchConfig, IndexedType, IndexedProperty, SearchQuery, IndexedTypeSort> target = populateTarget();
    target.getSearchQueryData().setCategoryCode(null);
    target.getSearchQueryData().setBlPage(USED_PAGE);
    blSearchFiltersPopulator.populate(source, target);
    Assert.assertNull(target.getSearchQueryData().getCategoryCode());
    for(final QueryField values : target.getSearchQuery().getFilterQueries()) {
      if(FOR_SALE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(FOR_SALE,values.getField());
        Assert.assertTrue(values.getValues().contains(TRUE));
      }
      if(ITEM_TYPE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(ITEM_TYPE,values.getField());
        Assert.assertTrue(values.getValues().contains(BL_PRODUCT));
      }

    }
  }

  //If Search is rental and category code is null
  @Test
  public void testPopulateWhenSearchIsRental() {
    final SearchQueryPageableData<SolrSearchQueryData> source = null;
    final SolrSearchRequest<FacetSearchConfig, IndexedType, IndexedProperty, SearchQuery, IndexedTypeSort> target = populateTarget();
    target.getSearchQueryData().setCategoryCode(null);
    target.getSearchQueryData().setBlPage(RENTAL_PAGE);
    blSearchFiltersPopulator.populate(source, target);
    Assert.assertNull(target.getSearchQueryData().getCategoryCode());
    for(final QueryField values : target.getSearchQuery().getFilterQueries()) {
      if(FOR_RENT.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(FOR_RENT,values.getField());
        Assert.assertTrue(values.getValues().contains(TRUE));
      }
      if(ITEM_TYPE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(ITEM_TYPE,values.getField());
        Assert.assertTrue(values.getValues().contains(BL_PRODUCT));
      }

    }
  }


  // When category code is rental and have sort as price-asc
  @Test
  public void testPopulatewhenpriceasc() {
    final Set<String> strings = Collections.singleton(CATEGORY_CODE);
    final SearchQueryPageableData<SolrSearchQueryData> source = null;
    final SolrSearchRequest<FacetSearchConfig, IndexedType, IndexedProperty, SearchQuery, IndexedTypeSort> target = populateTarget();
    final CategoryModel categoryModel = new CategoryModel();
    categoryModel.setCode(CATEGORY_CODE);
    categoryModel.setRentalCategory(true);
    when(commerceCategoryService.getCategoryForCode(CATEGORY_CODE)).thenReturn(categoryModel);
    final PageableData pageableData = new PageableData();
    pageableData.setSort(SORT_CODE_PRICE_ASC);
    target.setPageableData(pageableData);
    final List<IndexedTypeSort> sorts = new ArrayList<>();
    final IndexedTypeSort indexedTypeSort = new IndexedTypeSort();
    indexedTypeSort.setCode(SORT_CODE_PRICE_ASC);
    indexedTypeSort.setName(SORT_CODE_PRICE_ASC);
    final List<IndexedTypeSortField> field = new ArrayList<>();
    final IndexedTypeSortField indexedTypeSortField = new IndexedTypeSortField();
    indexedTypeSortField.setFieldName(SORT_CODE_PRICE_ASC);
    indexedTypeSortField.setAscending(true);
    field.add(indexedTypeSortField);
    indexedTypeSort.setFields(field);
    sorts.add(indexedTypeSort);
    target.getSearchQuery().getIndexedType().setSorts(sorts);

    blSearchFiltersPopulator.populate(source, target);
    Assert.assertEquals(CATEGORY_CODE,target.getSearchQueryData().getCategoryCode());

    for(final QueryField values : target.getSearchQuery().getFilterQueries()) {
      if(ALL_CATEGORIES.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(ALL_CATEGORIES,values.getField());
        Assert.assertEquals(strings,values.getValues());
      }
      if(FOR_RENT.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(FOR_RENT,values.getField());
        Assert.assertTrue(values.getValues().contains(TRUE));
      }
      if(ITEM_TYPE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(ITEM_TYPE,values.getField());
        Assert.assertTrue(values.getValues().contains(BL_PRODUCT));
      }

    }

    for(final IndexedTypeSort sort :target.getSearchQuery().getIndexedType().getSorts()) {
      Assert.assertEquals(SORT_CODE_PRICE_ASC , sort.getCode());
      Assert.assertEquals(PRICE_VALUE , sort.getFields().get(0).getFieldName());
      Assert.assertTrue(sort.getFields().get(0).isAscending());
    }
  }


  // When category new arrivals and sort as price asc
  @Test
  public void testPopulateWhenUsedGearAndSortIsPriceDesc() {
    final SearchQueryPageableData<SolrSearchQueryData> source = null;
    final SolrSearchRequest<FacetSearchConfig, IndexedType, IndexedProperty, SearchQuery, IndexedTypeSort> target = populateTarget();
    target.getSearchQueryData().setCategoryCode(USED_CATEGORY_CODE);
    final CategoryModel category = new CategoryModel();
    category.setCode(USED_CATEGORY_CODE);
    when(commerceCategoryService.getCategoryForCode(USED_CATEGORY_CODE)).thenReturn(category);

    final PageableData pageableData = new PageableData();
    pageableData.setSort(SORT_CODE_PRICE_DESC);
    target.setPageableData(pageableData);
    final List<IndexedTypeSort> sorts = new ArrayList<>();
    final IndexedTypeSort indexedTypeSort = new IndexedTypeSort();
    indexedTypeSort.setCode(SORT_CODE_PRICE_DESC);
    indexedTypeSort.setName(SORT_CODE_PRICE_DESC);
    final List<IndexedTypeSortField> field = new ArrayList<>();
    final IndexedTypeSortField indexedTypeSortField = new IndexedTypeSortField();
    indexedTypeSortField.setFieldName(SORT_CODE_PRICE_DESC);
    indexedTypeSortField.setAscending(false);
    field.add(indexedTypeSortField);
    indexedTypeSort.setFields(field);
    sorts.add(indexedTypeSort);
    target.getSearchQuery().getIndexedType().setSorts(sorts);


    blSearchFiltersPopulator.populate(source, target);
    Assert.assertEquals(USED_CATEGORY_CODE,target.getSearchQueryData().getCategoryCode());

    for(final QueryField values : target.getSearchQuery().getFilterQueries()) {
      if(IS_NEW.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(IS_NEW,values.getField());
        Assert.assertTrue(values.getValues().contains(TRUE));
      }
      if(FOR_SALE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(FOR_SALE,values.getField());
        Assert.assertTrue(values.getValues().contains(TRUE));
      }
      if(ITEM_TYPE.equalsIgnoreCase(values.getField())) {
        Assert.assertEquals(ITEM_TYPE,values.getField());
        Assert.assertTrue(values.getValues().contains(BL_PRODUCT));
      }

    }

    for(final IndexedTypeSort sort :target.getSearchQuery().getIndexedType().getSorts()) {
      Assert.assertEquals(SORT_CODE_PRICE_DESC , sort.getCode());
      Assert.assertEquals(MIN_SERIAL_PRICE , sort.getFields().get(0).getFieldName());
      Assert.assertFalse(sort.getFields().get(0).isAscending());
    }

  }


  protected SolrSearchRequest<FacetSearchConfig, IndexedType, IndexedProperty, SearchQuery, IndexedTypeSort> populateTarget()
  {
    final SolrSearchRequest<FacetSearchConfig, IndexedType, IndexedProperty, SearchQuery, IndexedTypeSort> solrSearchRequest = new SolrSearchRequest<>();

    final SolrSearchQueryTermData solrSearchQueryTermData1 = new SolrSearchQueryTermData();
    solrSearchQueryTermData1.setKey(KEY1);
    solrSearchQueryTermData1.setValue(VALUE1);

    final SolrSearchQueryTermData solrSearchQueryTermData2 = new SolrSearchQueryTermData();
    solrSearchQueryTermData2.setKey(KEY2);
    solrSearchQueryTermData2.setValue(VALUE2);

    final SolrSearchQueryTermData solrSearchQueryTermData3 = new SolrSearchQueryTermData();
    solrSearchQueryTermData3.setKey(KEY3);
    solrSearchQueryTermData3.setValue(VALUE3);

    final SolrSearchQueryTermData solrSearchQueryTermData4 = new SolrSearchQueryTermData();
    solrSearchQueryTermData4.setKey(KEY4);
    solrSearchQueryTermData4.setValue(VALUE4);

    final List<SolrSearchQueryTermData> filterTerms = new ArrayList<>();
    filterTerms.add(solrSearchQueryTermData1);
    filterTerms.add(solrSearchQueryTermData2);
    filterTerms.add(solrSearchQueryTermData3);
    filterTerms.add(solrSearchQueryTermData4);

    final SolrSearchQueryData solrSearchQueryData = new SolrSearchQueryData();
    solrSearchQueryData.setFilterTerms(filterTerms);
    solrSearchQueryData.setCategoryCode(CATEGORY_CODE);

    final SolrSearchFilterQueryData solrSearchFilterQueryData3 = new SolrSearchFilterQueryData();
    solrSearchFilterQueryData3.setKey(KEY3);
    solrSearchFilterQueryData3.setOperator(FilterQueryOperator.AND);
    solrSearchFilterQueryData3.setValues(new HashSet<>());

    final SolrSearchFilterQueryData solrSearchFilterQueryData4 = new SolrSearchFilterQueryData();
    solrSearchFilterQueryData4.setKey(KEY4);
    solrSearchFilterQueryData4.setValues(new HashSet<>());

    final List<SolrSearchFilterQueryData> filterQueries = new ArrayList<>();
    filterQueries.add(solrSearchFilterQueryData3);
    filterQueries.add(solrSearchFilterQueryData4);

    solrSearchQueryData.setFilterQueries(filterQueries);

    solrSearchRequest.setSearchQueryData(solrSearchQueryData);

    final IndexedType indexedType = new IndexedType();
    indexedType.setIndexedProperties(populateIndexedProperties());

    final SearchQuery searchQuery = new SearchQuery(new FacetSearchConfig(), indexedType);

    solrSearchRequest.setSearchQuery(searchQuery);

    solrSearchRequest.setIndexedType(indexedType);
    final PageableData pageableData = new PageableData();
    pageableData.setSort(SORT_CODE_NEWEST);
    solrSearchRequest.setPageableData(pageableData);
    final List<IndexedTypeSort> sorts = new ArrayList<>();
    final IndexedTypeSort indexedTypeSort = new IndexedTypeSort();
    indexedTypeSort.setCode(SORT_CODE_NEWEST);
    indexedTypeSort.setName(SORT_CODE_NEWEST);
    sorts.add(indexedTypeSort);
    solrSearchRequest.getSearchQuery().getIndexedType().setSorts(sorts);

    return solrSearchRequest;
  }

  protected Map<String, IndexedProperty> populateIndexedProperties()
  {
    final HashMap<String, IndexedProperty> indexedProperties = new HashMap<>();

    final IndexedProperty indexedProperty1 = new IndexedProperty();
    indexedProperty1.setFacet(true);

    final IndexedProperty indexedProperty2 = new IndexedProperty();
    indexedProperty2.setFacet(true);

    final IndexedProperty indexedProperty3 = new IndexedProperty();
    indexedProperty3.setFacet(false);

    final IndexedProperty indexedProperty4 = new IndexedProperty();
    indexedProperty4.setFacet(false);

    indexedProperties.put(KEY1, indexedProperty1);
    indexedProperties.put(KEY2, indexedProperty2);
    indexedProperties.put(KEY3, indexedProperty3);
    indexedProperties.put(KEY4, indexedProperty4);

    return indexedProperties;
  }

}
