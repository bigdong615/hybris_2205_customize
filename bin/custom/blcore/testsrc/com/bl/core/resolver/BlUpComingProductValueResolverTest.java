package com.bl.core.resolver;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.jalo.JaloSession;
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
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.resolvers.BlUpComingProductValueResolver;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlUpComingProductValueResolverTest {

  protected static final String INDEXED_PROPERTY_NAME = "upComing";
  protected static final String TEST_BL_PRODUCT_CODE = "Test12345";
  protected static final String TEST_SERIAL_PRODUCT_CODE = "Serial1234";
  protected static final String TEST_SERIAL_PRODUCT_NAME = "Test Serial Product";

  @InjectMocks
  BlUpComingProductValueResolver blUpComingProductValueResolver = Mockito.spy(BlUpComingProductValueResolver.class);

  @Mock
  private InputDocument inputDocument;
  @Mock
  private IndexerBatchContext batchContext;
  @Mock
  private BlProductModel blProductModel;
  @Mock
  private SessionService sessionService;
  @Mock
  private JaloSession jaloSession;
  @Mock
  private QualifierProvider qualifierProvider;


  private IndexedProperty indexedProperty;

  @Before
  public void startUp(){
	  // MockitoAnnotations.initMocks(this);
    indexedProperty = new IndexedProperty();
    indexedProperty.setName(INDEXED_PROPERTY_NAME);
    indexedProperty.setValueProviderParameters(new HashMap<>());

    when(qualifierProvider.canApply(any(IndexedProperty.class))).thenReturn(Boolean.FALSE);
    when(sessionService.getRawSession(null)).thenReturn(jaloSession);

    blUpComingProductValueResolver = new BlUpComingProductValueResolver();
    blUpComingProductValueResolver.setSessionService(sessionService);
    blUpComingProductValueResolver.setQualifierProvider(qualifierProvider);
  }

  // If BlProduct ForRent as True and have empty BlSerialProducts
  @Test
  public void addFieldValuesWhenProductIsRent() throws FieldValueProviderException {
    addBlProductModelAndBlSerialProductModel(true , false);
    callResolve(inputDocument,batchContext,Collections.singletonList(indexedProperty),blProductModel);
  }

  // If BlProduct ForRent as True and have as BlSerialProducts
  @Test
  public void addFieldValuesWhenProductIsRentAndBlSerialProducts() throws FieldValueProviderException {
    addBlProductModelAndBlSerialProductModel(true ,true);
    callResolve(inputDocument,batchContext,Collections.singletonList(indexedProperty),blProductModel);
  }

  // If BlProduct ForRent as False and have as BlSerialProducts
  @Test
  public void addFieldValuesWhenProductIsRentAsFalseAndBlSerialProducts() throws FieldValueProviderException {
    addBlProductModelAndBlSerialProductModel(false , true);
    callResolve(inputDocument,batchContext,Collections.singletonList(indexedProperty),blProductModel);
  }

  // If BlProduct ForRent as False and have as empty BlSerialProducts
  @Test
  public void addFieldValuesWhenProductIsRentAsFalseAndEMptyBlSerialProducts() throws FieldValueProviderException {
    addBlProductModelAndBlSerialProductModel(false ,false);
    callResolve(inputDocument,batchContext,Collections.singletonList(indexedProperty),blProductModel);
  }


  public void callResolve(final InputDocument inputDocument , final IndexerBatchContext batchContext ,final Collection<IndexedProperty> indexedProperties ,
      final BlProductModel blProductModel)
      throws FieldValueProviderException {
    blUpComingProductValueResolver.resolve(inputDocument,batchContext,indexedProperties,blProductModel);
  }

  public void addBlProductModelAndBlSerialProductModel(final boolean value , final boolean ifSerialhasProduct ){
    final Collection<BlSerialProductModel> blSerialProductModels = new ArrayList<>();
    if(ifSerialhasProduct) {
      final BlSerialProductModel blSerialProductModel = new BlSerialProductModel();
      blSerialProductModel.setCode(TEST_SERIAL_PRODUCT_CODE);
      blSerialProductModel.setName(TEST_SERIAL_PRODUCT_NAME, Locale.ENGLISH);
      blSerialProductModels.add(blSerialProductModel);
    }
    blProductModel.setCode(TEST_BL_PRODUCT_CODE);
    blProductModel.setForRent(value);
    blProductModel.setSerialProducts(blSerialProductModels);
    when(blProductModel.getForRent()).thenReturn(value);
    when(blProductModel.getSerialProducts()).thenReturn(blSerialProductModels);
  }

}

