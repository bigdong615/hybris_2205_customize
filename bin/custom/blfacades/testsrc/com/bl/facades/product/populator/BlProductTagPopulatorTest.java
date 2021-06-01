package com.bl.facades.product.populator;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.facades.populators.BlProductTagPopulator;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.product.ProductModel;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.runners.MockitoJUnitRunner;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlProductTagPopulatorTest {

  @InjectMocks
  private final BlProductTagPopulator populator = Mockito.spy(new BlProductTagPopulator());

  @Mock
  BlProductModel productModel;

  ProductData productData;

  @Before
  public void prepare(){
    MockitoAnnotations.initMocks(this);
    productData = new ProductData();
  }
  @Test
  public void populateProductTagNewDetails() {
    when(productModel.getIsNew()).thenReturn(Boolean.TRUE);
    when(productModel.getMostPopular()).thenReturn(Boolean.TRUE);
    when(productModel.getForRent()).thenReturn(Boolean.TRUE);
    when(productModel.getGreatValue()).thenReturn(Boolean.TRUE);
    when(productModel.getStaffPick()).thenReturn(Boolean.TRUE);
    populator.populate(productModel, productData);
    Assert.assertEquals( BlCoreConstants.NEW ,productData.getProductTagValues());
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.POPULAR);
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.GREAT_VALUE_STRING);
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.STAFF_PICK_STRING);
  }

  @Test
  public void populateProductTagPopularDetails() {
    when(productModel.getIsNew()).thenReturn(Boolean.FALSE);
    when(productModel.getMostPopular()).thenReturn(Boolean.TRUE);
    when(productModel.getForRent()).thenReturn(Boolean.TRUE);
    when(productModel.getGreatValue()).thenReturn(Boolean.TRUE);
    when(productModel.getStaffPick()).thenReturn(Boolean.TRUE);
    populator.populate(productModel, productData);
    assertNotEquals(productData.getProductTagValues(), BlCoreConstants.NEW);
    assertEquals(BlCoreConstants.POPULAR,productData.getProductTagValues());
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.GREAT_VALUE_STRING);
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.STAFF_PICK_STRING);
  }

  @Test
  public void populateProductTagRentDetails() {
    when(productModel.getIsNew()).thenReturn(Boolean.FALSE);
    when(productModel.getMostPopular()).thenReturn(Boolean.FALSE);
    when(productModel.getForRent()).thenReturn(Boolean.TRUE);
    when(productModel.getGreatValue()).thenReturn(Boolean.TRUE);
    when(productModel.getStaffPick()).thenReturn(Boolean.TRUE);
    populator.populate(productModel, productData);
    assertNotEquals(productData.getProductTagValues(), BlCoreConstants.NEW);
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.POPULAR);
    assertEquals(BlCoreConstants.GREAT_VALUE_STRING,productData.getProductTagValues());
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.STAFF_PICK_STRING);
  }

  @Test
  public void populateProductTagStaffDetails() {
    when(productModel.getIsNew()).thenReturn(Boolean.FALSE);
    when(productModel.getMostPopular()).thenReturn(Boolean.FALSE);
    when(productModel.getForRent()).thenReturn(Boolean.TRUE);
    when(productModel.getGreatValue()).thenReturn(Boolean.FALSE);
    when(productModel.getStaffPick()).thenReturn(Boolean.TRUE);
    populator.populate(productModel, productData);
    assertNotEquals(productData.getProductTagValues(), BlCoreConstants.NEW);
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.POPULAR);
    assertNotEquals(productData.getProductTagValues(),BlCoreConstants.GREAT_VALUE_STRING);
    assertEquals(BlCoreConstants.STAFF_PICK_STRING ,productData.getProductTagValues());
  }
}

