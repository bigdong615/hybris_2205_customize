package com.bl.core.resolver;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import com.bl.core.model.BlProductModel;
import com.bl.core.resolvers.BlGlobalFacetValueResolver;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.search.solrfacetsearch.provider.CategorySource;
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
import java.util.Locale;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@UnitTest
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
  private CategorySource categorySource;
  @Mock
  private ModelService modelService;

  private IndexedProperty indexedProperty;

  @InjectMocks
  private BlGlobalFacetValueResolver blGlobalFacetValueResolver = Mockito.spy(
      BlGlobalFacetValueResolver.class);

  @Before
  public void startUp() {
    MockitoAnnotations.initMocks(this);
    indexedProperty = new IndexedProperty();
    indexedProperty.setName(INDEXED_PROPERTY_NAME);
    indexedProperty.setValueProviderParameters(new HashMap<>());

    when(qualifierProvider.canApply(any(IndexedProperty.class))).thenReturn(Boolean.FALSE);
    when(sessionService.getRawSession(null)).thenReturn(jaloSession);

    blGlobalFacetValueResolver = new BlGlobalFacetValueResolver();
    blGlobalFacetValueResolver.setSessionService(sessionService);
    blGlobalFacetValueResolver.setQualifierProvider(qualifierProvider);
    blGlobalFacetValueResolver.setCategorySource(categorySource);
    blGlobalFacetValueResolver.setModelService(modelService);

  }

// When categories is empty
  @Test
  public void resolveWhenCategoryIsEmpty() throws FieldValueProviderException {
    final Collection<CategoryModel> categories = new ArrayList<>();
    when(categorySource.getCategoriesForConfigAndProperty(Matchers.any(), Matchers.any(), Matchers.any())).thenReturn(categories);
    blGlobalFacetValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
  }

  // When Categories have Brand as category
  @Test
  public void resolveWhenCategoryIsNotEmpty() throws FieldValueProviderException {
    final Collection<CategoryModel> categories = new ArrayList<>();
    final CategoryModel categoryModel = new CategoryModel();
    LocaleProvider localeProvider = new StubLocaleProvider(Locale.ENGLISH);
    ItemModelContextImpl itemModelContext = (ItemModelContextImpl) categoryModel.getItemModelContext();
    itemModelContext.setLocaleProvider(localeProvider);
    categoryModel.setCode(CATEGORY_CODE);
    categoryModel.setName(CATEGORY_NAME);
    categories.add(categoryModel);
    when(categorySource.getCategoriesForConfigAndProperty(Matchers.any(), Matchers.any(), Matchers.any())).thenReturn(categories);
    blGlobalFacetValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
  }

  // When Categories as different category
  @Test
  public void resolveWhenCategory() throws FieldValueProviderException {
    final Collection<CategoryModel> categories = new ArrayList<>();
    final CategoryModel categoryModel = new CategoryModel();
    LocaleProvider localeProvider = new StubLocaleProvider(Locale.ENGLISH);
    ItemModelContextImpl itemModelContext = (ItemModelContextImpl) categoryModel.getItemModelContext();
    itemModelContext.setLocaleProvider(localeProvider);
    categoryModel.setCode(CATEGORY_CODE1);
    categoryModel.setName(CATEGORY_NAME1);
    categories.add(categoryModel);
    when(categorySource.getCategoriesForConfigAndProperty(Matchers.any(), Matchers.any(), Matchers.any())).thenReturn(categories);
    blGlobalFacetValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
  }


  // When Categories as multiple category
  @Test
  public void resolveWhenMultipleCaegory() throws FieldValueProviderException {
    final Collection<CategoryModel> categories = new ArrayList<>();
    final CategoryModel categoryModel = new CategoryModel();
    final CategoryModel categoryModel1 = new CategoryModel();
    final CategoryModel categoryModel2 = new CategoryModel();
    final CategoryModel categoryModel3 = new CategoryModel();
    LocaleProvider localeProvider = new StubLocaleProvider(Locale.ENGLISH);
    ItemModelContextImpl itemModelContext = (ItemModelContextImpl) categoryModel.getItemModelContext();
    ItemModelContextImpl itemModelContext1 = (ItemModelContextImpl) categoryModel1.getItemModelContext();
    ItemModelContextImpl itemModelContext2 = (ItemModelContextImpl) categoryModel2.getItemModelContext();
    ItemModelContextImpl itemModelContext3 = (ItemModelContextImpl) categoryModel3.getItemModelContext();
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
    categories.add(categoryModel);
    categories.add(categoryModel1);
    categories.add(categoryModel2);
    categories.add(categoryModel3);
    when(categorySource.getCategoriesForConfigAndProperty(Matchers.any(), Matchers.any(), Matchers.any())).thenReturn(categories);
    blGlobalFacetValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
  }

}

