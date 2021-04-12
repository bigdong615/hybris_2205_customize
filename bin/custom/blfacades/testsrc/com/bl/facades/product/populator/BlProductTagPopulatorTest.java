package com.bl.facades.product.populator;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlProductModel;
import com.bl.facades.populators.BlProductTagPopulator;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.product.ProductModel;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.runners.MockitoJUnitRunner;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlProductTagPopulatorTest {

  @InjectMocks
  private final BlProductTagPopulator populator = Mockito.spy(new BlProductTagPopulator());


  public static String PRODUCT_NEW = "new";
  public static String PRODUCT_MOSTPOPULAR = "MOSTPOPULAR";
  public static String PRODUCT_GREATVALUE = "GREATVALUE";
  public static String PRODUCT_STAFFPICK = "STAFFPICK";
  public static String PRODUCT_FORRENT = "PRODUCT_FORRENT";

  @Mock
  BlProductModel productModel;
  @Mock
  ProductData productData;

  @Test
  public void populateProductTagNewDetails() {
    when(productModel.getIsNew()).thenReturn(Boolean.TRUE);
    populator.populate(productModel, productData);
    Assert.assertEquals(productData.getProductTagValues(), BlCoreConstants.NEW );
  }

  @Test
  public void populateProductTagPopularDetails() {
    when(productModel.getIsNew()).thenReturn(Boolean.TRUE);
    when(productModel.getMostPopular()).thenReturn(Boolean.TRUE);
    when(productModel.getForRent()).thenReturn(Boolean.TRUE);
    when(productModel.getGreatValue()).thenReturn(Boolean.TRUE);

    populator.populate(productModel, productData);
    Assert.assertEquals(productData.getProductTagValues(), BlCoreConstants.NEW );
    Assert.assertNotEquals(productData.getProductTagValues(),BlCoreConstants.POPULAR);
    Assert.assertNotEquals(productData.getProductTagValues(),BlCoreConstants.GREAT_VALUE);
  }

  @Test
  public void populateProductTag1PopularDetails() {
    when(productModel.getIsNew()).thenReturn(Boolean.FALSE);
    when(productModel.getMostPopular()).thenReturn(Boolean.TRUE);

    populator.populate(productModel, productData);
    Assert.assertNotEquals(productData.getProductTagValues(), BlCoreConstants.NEW );
    Assert.assertEquals(productData.getProductTagValues(),BlCoreConstants.POPULAR);
  }
}

