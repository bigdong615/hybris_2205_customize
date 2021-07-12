package com.bl.facades.product.populator;

import static de.hybris.platform.testframework.Assert.assertEquals;
import static org.mockito.Mockito.when;

import com.bl.core.enums.ConditionRatingValueEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.ProductVideoModel;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.populators.BlProductPopulator;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.time.DurationFormatUtils;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import static org.mockito.Mockito.verify;

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
  public static String PRODUCT_ID1 = "39507";
  public static Double PRODUCT_RATING1 = 6.0d;
  public static String PRODUCT_ID2 = "39508";
  public static Double PRODUCT_RATING2 = 6.0d;
  public static String PRODUCT_ID3 = "39509";
  public static ConditionRatingValueEnum FUNCTIONAL_RATING = ConditionRatingValueEnum.valueOf("6");
  public static ConditionRatingValueEnum COSMETIC_RATING = ConditionRatingValueEnum.valueOf("6");

  @Mock
  BlProductModel productModel;
  @Mock
  private ModelService modelService;

  List serialProductList;

  List productVideoList;
  @Mock
  private Converter<MediaModel, ImageData> imageConverter;
  @Mock
  private Populator<BlProductModel, ProductData> blProductTagPopulator;

  private ProductVideoModel productVideoModel;
  ProductData productData;

  @Before
  public void prepare() {
    MockitoAnnotations.initMocks(this);
    productData = new ProductData();
    productVideoList= new ArrayList();
    productVideoModel = new ProductVideoModel();
    productVideoModel.setVideoTitle(VIDEO_TITLE);
    productVideoModel.setVideoLink(VIDEO_URL);
    productVideoModel.setVideoDuration(VIDEO_DURATION);
    productVideoList.add(productVideoModel);
    populator.setBlProductTagPopulator(blProductTagPopulator);
  }

  @Test
  public void populateProduct() {
    when(productModel.getCode()).thenReturn(PRODUCT_CODE);
    when(productModel.getDisplayName()).thenReturn(DISPLAY_NAME);
    when(productModel.getRentalIncludes()).thenReturn(RENTAL_INCLUDE);
    when(productModel.getForRent()).thenReturn(Boolean.TRUE);
    when(productModel.getShortDescription()).thenReturn(SHORT_DESCRIPTION);
    when(productModel.getDisplayNotes()).thenReturn(DISPLAY_NOTE);
    
    when(productModel.getRentalVideosLink()).thenReturn(productVideoList);
    
    when(productModel.getSerialProducts()).thenReturn(serialProductList);
    populator.populate(productModel, productData);
    assertEquals(productData.getDisplayName(), DISPLAY_NAME);
    assertEquals(productData.getRentalIncludes(), RENTAL_INCLUDE);
    assertEquals(productData.getForRent(), Boolean.TRUE);
    assertEquals(productData.getShortDescription(), SHORT_DESCRIPTION);
    assertEquals(productData.getRentalNote(), DISPLAY_NOTE);
    
    productData.getRentalVideosLink().forEach(videoData -> {
      assertEquals(videoData.getVideoName(), VIDEO_TITLE);
      assertEquals(videoData.getVideoUrl(), VIDEO_URL);
      assertEquals(videoData.getVideoDuration(), DurationFormatUtils.formatDuration(VIDEO_DURATION * 1000,
          BlFacadesConstants.TIME_FORMAT_STRING));

    });

    verify(blProductTagPopulator).populate(productModel, productData);
  }
}
