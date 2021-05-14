package com.bl.core.resolver;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import com.bl.core.model.BlProductModel;
import com.bl.core.resolvers.BlCategoryCodeValueResolver;
import de.hybris.platform.category.model.CategoryModel;
import de.hybris.platform.commerceservices.search.solrfacetsearch.provider.CategorySource;
import de.hybris.platform.commerceservices.search.solrfacetsearch.provider.impl.DefaultCategorySource;
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

public class BlCategoryCodeValueResolverTest {

  private static final String INDEXED_PROPERTY_NAME = "category";
  private static final String CATEGORY_CODE = "Brands";
  private static final String CATEGORY_NAME = "Brands";
  private static final String CATEGORY_CODE1 = "Lenses";
  private static final String CATEGORY_NAME1 = "Lenses";

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
  private DefaultCategorySource defaultCategorySource;
  @Mock
  private ModelService modelService;

  private IndexedProperty indexedProperty;

  @InjectMocks
  private BlCategoryCodeValueResolver blCategoryCodeValueResolver = Mockito.spy(
      BlCategoryCodeValueResolver.class);

  @Before
  public void startUp() {
    MockitoAnnotations.initMocks(this);
    indexedProperty = new IndexedProperty();
    indexedProperty.setName(INDEXED_PROPERTY_NAME);
    indexedProperty.setValueProviderParameters(new HashMap<>());

    when(qualifierProvider.canApply(any(IndexedProperty.class))).thenReturn(Boolean.FALSE);
    when(sessionService.getRawSession(null)).thenReturn(jaloSession);

    blCategoryCodeValueResolver = new BlCategoryCodeValueResolver();
    blCategoryCodeValueResolver.setSessionService(sessionService);
    blCategoryCodeValueResolver.setQualifierProvider(qualifierProvider);
    blCategoryCodeValueResolver.setCategorySource(categorySource);
  }

  //  When Categories is empty
  @Test
  public void resolveWhenCategoryIsEmpty() throws FieldValueProviderException {
    final Collection<CategoryModel> categories = new ArrayList<>();
    when(categorySource.getCategoriesForConfigAndProperty(Matchers.any(), Matchers.any(), Matchers.any())).thenReturn(categories);
    blCategoryCodeValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
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
    blCategoryCodeValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
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
    categoryModel.setName(CATEGORY_CODE1);
    categories.add(categoryModel);
    when(categorySource.getCategoriesForConfigAndProperty(Matchers.any(), Matchers.any(), Matchers.any())).thenReturn(categories);
    blCategoryCodeValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
  }

}
