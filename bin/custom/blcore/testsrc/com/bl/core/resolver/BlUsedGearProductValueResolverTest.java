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
import com.bl.core.resolvers.BlUsedGearProductValueResolver;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlUsedGearProductValueResolverTest {

  private static final String INDEXED_PROPERTY_NAME = "forSale";
  private static final String TEST_BL_PRODUCT_CODE = "Test12345";
  private static final String TEST_SERIAL_PRODUCT_CODE = "Serial1234";
  private static final String TEST_SERIAL_PRODUCT_NAME = "Test Serial Product";
  private static final String TEST_SERIAL_PRODUCT_CODE_1 = "SerialProduct123456";
  private static final String TEST_SERIAL_PRODUCT_NAME_1 = "Test Serial Product 1";
  private static final String TEST_SERIAL_PRODUCT_CODE_2 = "SerialProduct-2";
  private static final String TEST_SERIAL_PRODUCT_NAME_2 = "Test Serial Product 2";

  @InjectMocks
  private BlUsedGearProductValueResolver blUsedGearProductValueResolver = Mockito.spy(BlUsedGearProductValueResolver.class);

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
  @Mock
  private BlSerialProductModel blSerialProductModel;
  @Mock
  private BlSerialProductModel blSerialProductModel1;
  @Mock
  private BlSerialProductModel blSerialProductModel2;



  private IndexedProperty indexedProperty;


  @Before
  public void startUp(){
	  // MockitoAnnotations.initMocks(this);
    indexedProperty = new IndexedProperty();
    indexedProperty.setName(INDEXED_PROPERTY_NAME);
    indexedProperty.setValueProviderParameters(new HashMap<>());

    when(qualifierProvider.canApply(any(IndexedProperty.class))).thenReturn(Boolean.FALSE);
    when(sessionService.getRawSession(null)).thenReturn(jaloSession);

    blUsedGearProductValueResolver = new BlUsedGearProductValueResolver();
    blUsedGearProductValueResolver.setSessionService(sessionService);
    blUsedGearProductValueResolver.setQualifierProvider(qualifierProvider);
  }

  // When BlProduct and serialProducts as forSale true
  @Test
  public void addFieldValuesWhenProductIsSale() throws FieldValueProviderException {
    addBlProductModelAndBlSerialProductModel(true  , true ,false);
    callResolve(inputDocument,batchContext, Collections.singletonList(indexedProperty),blProductModel);
  }

  // When BlProduct and SerialProducts contains one as true and other as false
  @Test
  public void addFieldValuesWhenProductIsSaleAndBlSerialProductsIsSale() throws FieldValueProviderException {
    addBlProductModelAndBlSerialProductModel(true  , true ,true);
    callResolve(inputDocument,batchContext, Collections.singletonList(indexedProperty),blProductModel);
  }

  // When BlProduct as false  and SerialProducts contains are false
  @Test
  public void addFieldValuesWhenProductAsFalseAndBlSerialProductsFalse() throws FieldValueProviderException {
    addBlProductModelAndBlSerialProductModel(false  , true ,true);
    callResolve(inputDocument,batchContext, Collections.singletonList(indexedProperty),blProductModel);
  }

  public void addBlProductModelAndBlSerialProductModel(final boolean value , final boolean ifSerialhasProduct , final boolean multipleSerialProduct){
    final Collection<BlSerialProductModel> blSerialProductModels = new ArrayList<>();
    if(ifSerialhasProduct) {
      blSerialProductModel.setCode(TEST_SERIAL_PRODUCT_CODE);
      blSerialProductModel.setName(TEST_SERIAL_PRODUCT_NAME, Locale.ENGLISH);
      blSerialProductModel.setForSale(value);
      blSerialProductModels.add(blSerialProductModel);
      when(blSerialProductModel.getForSale()).thenReturn(value);
    }
    if(multipleSerialProduct) {
      blSerialProductModel1.setCode(TEST_SERIAL_PRODUCT_CODE_1);
      blSerialProductModel1.setName(TEST_SERIAL_PRODUCT_NAME_1, Locale.ENGLISH);
      blSerialProductModel1.setForSale(value);
      blSerialProductModel2.setCode(TEST_SERIAL_PRODUCT_CODE_2);
      blSerialProductModel2.setName(TEST_SERIAL_PRODUCT_NAME_2, Locale.ENGLISH);
      blSerialProductModel2.setForSale(false);
      when(blSerialProductModel1.getForSale()).thenReturn(value);
      when(blSerialProductModel2.getForSale()).thenReturn(false);
      blSerialProductModels.add(blSerialProductModel1);
      blSerialProductModels.add(blSerialProductModel2);
    }
    blProductModel.setCode(TEST_BL_PRODUCT_CODE);
    blProductModel.setForSale(value);
    blProductModel.setSerialProducts(blSerialProductModels);
    when(blProductModel.getForSale()).thenReturn(value);
    when(blProductModel.getSerialProducts()).thenReturn(blSerialProductModels);
  }

  public void callResolve(final InputDocument inputDocument , final IndexerBatchContext batchContext ,final Collection<IndexedProperty> indexedProperties ,
      final BlProductModel blProductModel)
      throws FieldValueProviderException {
    blUsedGearProductValueResolver.resolve(inputDocument,batchContext,indexedProperties,blProductModel);
  }



}

