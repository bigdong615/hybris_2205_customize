package com.bl.facades.product.populator;

import static de.hybris.platform.testframework.Assert.assertEquals;
import static org.mockito.Mockito.when;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.ProductVideoModel;
import com.bl.facades.populators.BlProductPopulator;
import com.bl.facades.product.data.SerialProductData;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.List;
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

  public static String PRODUCT_CODE = "testcode";
  public static String DISPLAY_NAME = "test product";
  public static String RENTAL_INCLUDE = "This is rental include related data";
  public static String SHORT_DESCRIPTION = "This is sort description";
  public static String USED_INCLUDE = "This is used include related data";
  public static String DISPLAY_NOTE = "This is display note related data";
  public static String USED_DESCIPTION = "This is rental include related data";
  public static String VIDEO_TITLE = "Test video title";
  public static Long VIDEO_DURATION = 345l;
  public static String VIDEO_URL = "https://www.borrowlenses.com";
  public static String PRODUCT_ID = "39507";
  public static Double PRODUCT_RATING = 4.2;

  @Mock
  BlProductModel productModel;
  @Mock
  private ModelService modelService;
  @Mock
  List serialProductList;
  @Mock
  List productVideoList;
  @Mock
  private Converter<MediaModel, ImageData> imageConverter;

  private BlSerialProductModel serialProductModel;
  private ProductVideoModel productVideoModel;
  ProductData productData;

  @Before
  public void prepare() {
    MockitoAnnotations.initMocks(this);
    productData = new ProductData();
    productVideoModel = new ProductVideoModel();
    productVideoModel.setVideoTitle(VIDEO_TITLE);
    productVideoModel.setVideoLink(VIDEO_URL);
    productVideoModel.setVideoDuration(VIDEO_DURATION);
    productVideoList.add(productVideoModel);
    serialProductModel = new BlSerialProductModel();
    serialProductModel.setProductId(PRODUCT_ID);
    serialProductModel.setConditionRatingOverallScore(PRODUCT_RATING);
    serialProductList.add(serialProductModel);
  }

  @Test
  public void shouldPopulateProductModel() {
    when(productModel.getCode()).thenReturn(PRODUCT_CODE);
    when(productModel.getDisplayName()).thenReturn(DISPLAY_NAME);
    when(productModel.getRentalIncludes()).thenReturn(RENTAL_INCLUDE);
    when(productModel.getForRent()).thenReturn(Boolean.TRUE);
    when(productModel.getShortDescription()).thenReturn(SHORT_DESCRIPTION);
    when(productModel.getUsedIncludes()).thenReturn(USED_INCLUDE);
    when(productModel.getDisplayNotes()).thenReturn(DISPLAY_NOTE);
    when(productModel.getUsedDescription()).thenReturn(USED_DESCIPTION);
    when(productModel.getRentalVideosLink()).thenReturn(productVideoList);
    when(productModel.getUsedGearVideosLink()).thenReturn(productVideoList);
    when(productModel.getSerialProducts()).thenReturn(serialProductList);
    populator.populate(productModel, productData);
    assertEquals(productData.getDisplayName(), DISPLAY_NAME);
    assertEquals(productData.getRentalIncludes(), RENTAL_INCLUDE);
    assertEquals(productData.getForRent(), Boolean.TRUE);
    assertEquals(productData.getShortDescription(), SHORT_DESCRIPTION);
    assertEquals(productData.getUsedIncludes(), USED_INCLUDE);
    assertEquals(productData.getRentalNote(), DISPLAY_NOTE);
    assertEquals(productData.getUsedDescription(), USED_DESCIPTION);
    productData.getRentalVideosLink().forEach(videoData -> {
      assertEquals(videoData.getVideoName(), VIDEO_TITLE);
      assertEquals(videoData.getVideoUrl(), VIDEO_URL);
      assertEquals(videoData.getVideoDuration(), VIDEO_DURATION);
    });
    productData.getUsedGearVideosLink().forEach(videoData -> {
      assertEquals(videoData.getVideoName(), VIDEO_TITLE);
      assertEquals(videoData.getVideoUrl(), VIDEO_URL);
      assertEquals(videoData.getVideoDuration(), VIDEO_DURATION);
    });
    productData.getSerialproducts().forEach(serialproductData -> {
      SerialProductData serialProduct = (SerialProductData) serialproductData;
      assertEquals(serialProduct.getConditionRating(), PRODUCT_RATING);
      assertEquals(serialProduct.getSerialId(), PRODUCT_ID);
    });
  }
}
