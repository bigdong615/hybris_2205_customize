package com.bl.facades.product.populator;

import static de.hybris.platform.testframework.Assert.assertEquals;
import static org.mockito.Mockito.when;

import com.bl.core.model.BlProductModel;
import com.bl.facades.populators.BlProductPopulator;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.converters.populator.ProductPopulator;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.servicelayer.model.ModelService;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@UnitTest
public class BlProductPopulatorTest {

  @InjectMocks
  private final BlProductPopulator populator = Mockito.spy(new BlProductPopulator());
  //private ProductPopulator productPopulator = new ProductPopulator();

  public static String PRODUCT_CODE = "testcode";
  public static String DISPLAY_NAME = "test product";
  public static String RENTAL_INCLUDE = "This is rental include related data";
  public static String SHORT_DESCRIPTION = "This is sort description";

  @Mock
  BlProductModel productModel;
  @Mock
  private ModelService modelService;

  ProductData productData;

  @Before
  public void prepare() {
    MockitoAnnotations.initMocks(this);
    productData = new ProductData();
  }

  @Test
  public void shouldPopulateData() {
    when(productModel.getCode()).thenReturn(PRODUCT_CODE);
    when(productModel.getDisplayName()).thenReturn(DISPLAY_NAME);
    when(productModel.getRentalIncludes()).thenReturn(RENTAL_INCLUDE);
    when(productModel.getForRent()).thenReturn(Boolean.TRUE);
    when(productModel.getShortDescription()).thenReturn(SHORT_DESCRIPTION);
    populator.populate(productModel, productData);
    assertEquals(productData.getDisplayName(), DISPLAY_NAME);
    assertEquals(productData.getRentalIncludes(), RENTAL_INCLUDE);
    assertEquals(productData.getForRent(), Boolean.TRUE);
    assertEquals(productData.getShortDescription(), SHORT_DESCRIPTION);
  }

}
