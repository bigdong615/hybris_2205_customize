package com.bl.core.resolver;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.servicelayer.StubLocaleProvider;
import de.hybris.platform.servicelayer.internal.model.impl.LocaleProvider;
import de.hybris.platform.servicelayer.model.ItemModelContextImpl;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.QualifierProvider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.model.BlProductModel;
import com.bl.core.resolvers.BlGlobalFacetValueResolver;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlGlobalFacetValueResolverTest {

  private static final String INDEXED_PROPERTY_NAME = "store";
  private static final String CATEGORY_CODE = "Brands";
  private static final String CATEGORY_NAME = "Brands";
  private static final String CATEGORY_CODE1 = "Lenses";
  private static final String CATEGORY_NAME1 = "Lenses";
  private static final String CATEGORY_CODE2 = "Production";
  private static final String CATEGORY_NAME2 = "Production";
  private static final String CATEGORY_CODE3 = "Cameras";
  private static final String CATEGORY_NAME3 = "Cameras";
 private static final String CATEGORY_CODE4 = "nikon";
  private static final String CATEGORY_NAME4 = "nikon";
  private static final String CATEGORY_CODE5 = "panasonic";
  private static final String CATEGORY_NAME5 = "panasonic";
  private static final String CATEGORY_CODE6 = "kotak";
  private static final String CATEGORY_NAME6 = "kotak";

  @Mock
  private InputDocument inputDocument;
  @Mock
  private IndexerBatchContext batchContext;
  @Mock
  private BlProductModel blProductModel;
  @Mock
  private QualifierProvider qualifierProvider;
  @Mock
  private SessionService sessionService;
  @Mock
  private JaloSession jaloSession;
  @Mock
  private ModelService modelService;

  private IndexedProperty indexedProperty;

  @InjectMocks
  private BlGlobalFacetValueResolver blGlobalFacetValueResolver = Mockito.spy(
      BlGlobalFacetValueResolver.class);

  @Before
  public void startUp() {
	  // MockitoAnnotations.initMocks(this);
    indexedProperty = new IndexedProperty();
    indexedProperty.setName(INDEXED_PROPERTY_NAME);
    indexedProperty.setValueProviderParameters(new HashMap<>());

    when(qualifierProvider.canApply(any(IndexedProperty.class))).thenReturn(Boolean.FALSE);
    when(sessionService.getRawSession(null)).thenReturn(jaloSession);

    blGlobalFacetValueResolver = new BlGlobalFacetValueResolver();
    blGlobalFacetValueResolver.setSessionService(sessionService);
    blGlobalFacetValueResolver.setQualifierProvider(qualifierProvider);
    blGlobalFacetValueResolver.setModelService(modelService);

  }

// When categories is empty
  @Test
  public void resolveWhenCategoryIsEmpty() throws FieldValueProviderException {
    blProductModel.setSupercategories(new ArrayList<>());
    when(blProductModel.getSupercategories()).thenReturn(new ArrayList<>());
    blGlobalFacetValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
    Mockito.verify(inputDocument, Mockito.times(0)).addField(Mockito.any(IndexedProperty.class) ,  Mockito.any(CategoryModel.class));

  }

  // When products have multiple categories
  @Test
  public void resolveWhenCategoryIsNotEmpty() throws FieldValueProviderException {
    final Collection<CategoryModel> categories = new ArrayList<>();
    final List<CategoryModel> superCategories = new ArrayList<>();
    final CategoryModel categoryModel = new CategoryModel();
    final LocaleProvider localeProvider = new StubLocaleProvider(Locale.ENGLISH);
    final ItemModelContextImpl itemModelContext = (ItemModelContextImpl) categoryModel.getItemModelContext();
    itemModelContext.setLocaleProvider(localeProvider);
    categoryModel.setCode(CATEGORY_CODE);
    categoryModel.setName(CATEGORY_NAME);
    final CategoryModel superCategory = new CategoryModel();
    final ItemModelContextImpl itemModelContext1 = (ItemModelContextImpl) superCategory.getItemModelContext();
    itemModelContext1.setLocaleProvider(localeProvider);
    superCategory.setCode(CATEGORY_CODE3);
    superCategory.setName(CATEGORY_NAME3);
    superCategories.add(superCategory);
    categoryModel.setSupercategories(superCategories);
    categories.add(categoryModel);
    blProductModel.setSupercategories(categories);
    when(blProductModel.getSupercategories()).thenReturn(categories);
    blGlobalFacetValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
    Mockito.verify(inputDocument, Mockito.times(1)).addField(Mockito.any(IndexedProperty.class) ,  Mockito.any(CategoryModel.class));

  }

  // When products as different category
  @Test
  public void resolveWhenCategory() throws FieldValueProviderException {
    final Collection<CategoryModel> categories = new ArrayList<>();
    final List<CategoryModel> superCategories = new ArrayList<>();
    final CategoryModel categoryModel = new CategoryModel();
    final LocaleProvider localeProvider = new StubLocaleProvider(Locale.ENGLISH);
    final ItemModelContextImpl itemModelContext = (ItemModelContextImpl) categoryModel.getItemModelContext();
    itemModelContext.setLocaleProvider(localeProvider);
    categoryModel.setCode(CATEGORY_CODE1);
    categoryModel.setName(CATEGORY_NAME1);
    final CategoryModel superCategory = new CategoryModel();
    final ItemModelContextImpl itemModelContext1 = (ItemModelContextImpl) superCategory.getItemModelContext();
    itemModelContext1.setLocaleProvider(localeProvider);
    superCategory.setCode(CATEGORY_CODE);
    superCategory.setName(CATEGORY_NAME);
    superCategories.add(superCategory);
    categoryModel.setSupercategories(superCategories);
    categories.add(categoryModel);
    blProductModel.setSupercategories(categories);
    when(blProductModel.getSupercategories()).thenReturn(categories);
    blGlobalFacetValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
    Mockito.verify(inputDocument, Mockito.times(1)).addField(Mockito.any(IndexedProperty.class) ,  Mockito.any(CategoryModel.class));

  }


  // When Categories as multiple category
  @Test
  public void resolveWhenMultipleCaegory() throws FieldValueProviderException {
    final Collection<CategoryModel> categories = new ArrayList<>();
    final List<CategoryModel> superCategories = new ArrayList<>();
    final List<CategoryModel> superCategories1 = new ArrayList<>();
    final List<CategoryModel> superCategories2 = new ArrayList<>();
    final List<CategoryModel> superCategories3 = new ArrayList<>();
    final CategoryModel categoryModel = new CategoryModel();
    final CategoryModel categoryModel1 = new CategoryModel();
    final CategoryModel categoryModel2 = new CategoryModel();
    final CategoryModel categoryModel3 = new CategoryModel();
    final LocaleProvider localeProvider = new StubLocaleProvider(Locale.ENGLISH);
    final ItemModelContextImpl itemModelContext = (ItemModelContextImpl) categoryModel.getItemModelContext();
    final ItemModelContextImpl itemModelContext1 = (ItemModelContextImpl) categoryModel1.getItemModelContext();
    final ItemModelContextImpl itemModelContext2 = (ItemModelContextImpl) categoryModel2.getItemModelContext();
    final ItemModelContextImpl itemModelContext3 = (ItemModelContextImpl) categoryModel3.getItemModelContext();
    itemModelContext.setLocaleProvider(localeProvider);
    itemModelContext1.setLocaleProvider(localeProvider);
    itemModelContext2.setLocaleProvider(localeProvider);
    itemModelContext3.setLocaleProvider(localeProvider);
    categoryModel.setCode(CATEGORY_CODE);
    categoryModel.setName(CATEGORY_NAME);
    categoryModel1.setCode(CATEGORY_CODE1);
    categoryModel1.setName(CATEGORY_NAME1);
    categoryModel2.setCode(CATEGORY_CODE2);
    categoryModel2.setName(CATEGORY_NAME2);
    categoryModel3.setCode(CATEGORY_CODE3);
    categoryModel3.setName(CATEGORY_NAME3);
    final CategoryModel superCategory = new CategoryModel();
    final ItemModelContextImpl itemModelContext4 = (ItemModelContextImpl) superCategory.getItemModelContext();
    itemModelContext4.setLocaleProvider(localeProvider);
    superCategory.setCode(CATEGORY_CODE4);
    superCategory.setName(CATEGORY_NAME4);
    superCategories.add(superCategory);

    final CategoryModel superCategory1 = new CategoryModel();
    final ItemModelContextImpl itemModelContext5 = (ItemModelContextImpl) superCategory1.getItemModelContext();
    itemModelContext5.setLocaleProvider(localeProvider);
    superCategory1.setCode(CATEGORY_CODE4);
    superCategory1.setName(CATEGORY_NAME4);
    superCategories1.add(superCategory1);

    final CategoryModel superCategory2 = new CategoryModel();
    final ItemModelContextImpl itemModelContext6 = (ItemModelContextImpl) superCategory2.getItemModelContext();
    itemModelContext6.setLocaleProvider(localeProvider);
    superCategory2.setCode(CATEGORY_CODE5);
    superCategory2.setName(CATEGORY_NAME5);
    superCategories2.add(superCategory2);

    final CategoryModel superCategory3 = new CategoryModel();
    final ItemModelContextImpl itemModelContext7 = (ItemModelContextImpl) superCategory3.getItemModelContext();
    itemModelContext7.setLocaleProvider(localeProvider);
    superCategory3.setCode(CATEGORY_CODE6);
    superCategory3.setName(CATEGORY_NAME6);
    superCategories3.add(superCategory3);

    categoryModel.setSupercategories(superCategories);
    categoryModel1.setSupercategories(superCategories1);
    categoryModel2.setSupercategories(superCategories2);
    categoryModel3.setSupercategories(superCategories3);
    categories.add(categoryModel);
    categories.add(categoryModel1);
    categories.add(categoryModel2);
    categories.add(categoryModel3);
    blProductModel.setSupercategories(categories);
    when(blProductModel.getSupercategories()).thenReturn(categories);
    blGlobalFacetValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
   Mockito.verify(inputDocument, Mockito.times(3)).addField(Mockito.any(IndexedProperty.class) ,  Mockito.any(CategoryModel.class));

  }

}

