package com.bl.facades.populators;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.search.data.SearchQueryData;
import de.hybris.platform.commerceservices.enums.SearchQueryContext;
import de.hybris.platform.commerceservices.search.solrfacetsearch.data.SolrSearchQueryData;
import de.hybris.platform.commerceservices.util.ConverterFactory;
import de.hybris.platform.converters.impl.AbstractPopulatingConverter;

import java.util.Collections;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlSolrSearchQueryDecoderPopulatorTest {

  @InjectMocks
  private final AbstractPopulatingConverter<SearchQueryData, SolrSearchQueryData> blSolrSearchQueryDecoderPopulator = new ConverterFactory<SearchQueryData, SolrSearchQueryData, BlSolrSearchQueryDecoderPopulator>()
      .create(SolrSearchQueryData.class, new BlSolrSearchQueryDecoderPopulator());

  private static final String USED_GEAR ="usedgear";
  protected static final String FREE_TEXT_SEARCH = "free text search";
  protected static final String SORT = "sort";
  protected static final String KEY1 = "key1";
  protected static final String KEY2 = "key2";
  protected static final String VALUE1 = "value1";
  protected static final String VALUE2 = "value2";


  @Before
  public void startUp() {
	  // MockitoAnnotations.initMocks(this);
  }

  // If Source is Null
  @Test
  public void testPopulate() {
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(null);
    Assert.assertNull(result.getCategoryCode());
    Assert.assertNull(result.getFreeTextSearch());
    Assert.assertNull(result.getSort());
    Assert.assertNull(result.getFilterTerms());
    Assert.assertNull(result.getFilterQueries());
    Assert.assertNull(result.getSearchQueryContext());
    Assert.assertNull(result.getBlPage());
}

 // If source value is empty

  @Test
  public void testSourceEmpty() {
    final SearchQueryData searchQueryData = new SearchQueryData();
    searchQueryData.setValue("");
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(searchQueryData);
    Assert.assertNull(result.getCategoryCode());
    Assert.assertNull(result.getFreeTextSearch());
    Assert.assertNull(result.getSort());
    Assert.assertNull(result.getFilterTerms());
    Assert.assertNull(result.getBlPage());
  }

  // If source value is empty & Blpage as value
  @Test
  public void testSourceEmptyAndBlPageValue() {
    final SearchQueryData searchQueryData = new SearchQueryData();
    searchQueryData.setValue("");
    searchQueryData.setFilterQueries(Collections.emptyList());
    searchQueryData.setBlPage(USED_GEAR);
    searchQueryData.setSearchQueryContext(SearchQueryContext.DEFAULT);
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(searchQueryData);
    Assert.assertEquals(null,result.getFreeTextSearch());
    Assert.assertEquals(null,result.getSort());
    Assert.assertEquals(null,result.getFilterTerms());
    Assert.assertEquals(Collections.emptyList(),result.getFilterQueries());
    Assert.assertEquals(SearchQueryContext.DEFAULT,result.getSearchQueryContext());
    Assert.assertEquals(USED_GEAR,result.getBlPage());
  }


  // If Source as values search text without sorts
  @Test
  public void testSourceAsValue()
  {
    final SearchQueryData searchQueryData = new SearchQueryData();
    searchQueryData.setValue(FREE_TEXT_SEARCH);
    searchQueryData.setBlPage(USED_GEAR);
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(searchQueryData);
    Assert.assertNull(result.getCategoryCode());
    Assert.assertEquals(FREE_TEXT_SEARCH, result.getFreeTextSearch());
    Assert.assertNull(result.getSort());
    Assert.assertTrue(result.getFilterTerms().isEmpty());
    Assert.assertEquals(USED_GEAR,result.getBlPage());
  }

  // If Source as values as :
  @Test
  public void testSourceAsValueAsRation()
  {
    final SearchQueryData searchQueryData = new SearchQueryData();
    searchQueryData.setValue(":");
    searchQueryData.setBlPage(USED_GEAR);
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(searchQueryData);
    Assert.assertNull(result.getCategoryCode());
    Assert.assertEquals(null ,result.getFreeTextSearch());
    Assert.assertNull(result.getSort());
    Assert.assertTrue(result.getFilterTerms().isEmpty());
    Assert.assertEquals(Collections.emptyList(),result.getFilterQueries());
    Assert.assertEquals(USED_GEAR,result.getBlPage());
  }

  // If Source as values search text empty with sorts
  @Test
  public void testSourceAsEmptyValueWithSort()
  {
    final SearchQueryData searchQueryData = new SearchQueryData();
    searchQueryData.setValue(":" + SORT);
    searchQueryData.setBlPage(USED_GEAR);
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(searchQueryData);
    Assert.assertNull(result.getCategoryCode());
    Assert.assertEquals("",result.getFreeTextSearch());
    Assert.assertEquals(SORT,result.getSort());
    Assert.assertTrue(result.getFilterTerms().isEmpty());
    Assert.assertEquals(USED_GEAR,result.getBlPage());
  }

  // If Source as values search text empty with sorts
  @Test
  public void testSourceAsValueWithSort()
  {
    final SearchQueryData searchQueryData = new SearchQueryData();
    searchQueryData.setValue(FREE_TEXT_SEARCH + ":" + SORT);
    searchQueryData.setBlPage(USED_GEAR);
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(searchQueryData);
    Assert.assertNull(result.getCategoryCode());
    Assert.assertEquals(FREE_TEXT_SEARCH,result.getFreeTextSearch());
    Assert.assertEquals(SORT,result.getSort());
    Assert.assertTrue(result.getFilterTerms().isEmpty());
    Assert.assertEquals(USED_GEAR,result.getBlPage());
  }

  // If Source as values search text empty with sorts and More Value
  @Test
  public void testSourceAsValueWithSortAndMore()
  {
    final SearchQueryData searchQueryData = new SearchQueryData();
    searchQueryData.setValue(FREE_TEXT_SEARCH + ":" + SORT + ":" + KEY1 +":"+ VALUE1);
    searchQueryData.setBlPage(USED_GEAR);
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(searchQueryData);
    Assert.assertNull(result.getCategoryCode());
    Assert.assertEquals(FREE_TEXT_SEARCH,result.getFreeTextSearch());
    Assert.assertEquals(SORT,result.getSort());
    Assert.assertTrue(result.getFilterTerms().size() > 0);
    Assert.assertEquals(KEY1,result.getFilterTerms().get(0).getKey());
    Assert.assertEquals(VALUE1,result.getFilterTerms().get(0).getValue());
    Assert.assertEquals(USED_GEAR,result.getBlPage());
  }

  // If Source as values search text empty with sorts and More Value
  @Test
  public void testSourceAsValueWithSortAndMore2()
  {
    final SearchQueryData searchQueryData = new SearchQueryData();
    searchQueryData.setValue(FREE_TEXT_SEARCH + ":" + SORT + ":" + KEY1 +":"+ VALUE1 + ":" + KEY2+ ":" + VALUE2);
    searchQueryData.setBlPage(USED_GEAR);
    final SolrSearchQueryData result = blSolrSearchQueryDecoderPopulator.convert(searchQueryData);
    Assert.assertNull(result.getCategoryCode());
    Assert.assertEquals(FREE_TEXT_SEARCH,result.getFreeTextSearch());
    Assert.assertEquals(SORT,result.getSort());
    Assert.assertEquals(2,result.getFilterTerms().size());
    Assert.assertEquals(KEY1,result.getFilterTerms().get(0).getKey());
    Assert.assertEquals(VALUE1,result.getFilterTerms().get(0).getValue());
    Assert.assertEquals(KEY2,result.getFilterTerms().get(1).getKey());
    Assert.assertEquals(VALUE2,result.getFilterTerms().get(1).getValue());
    Assert.assertEquals(USED_GEAR,result.getBlPage());
  }

}
