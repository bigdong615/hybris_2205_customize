package com.bl.facades.product.populator;

import static de.hybris.platform.testframework.Assert.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.time.DurationFormatUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.ProductVideoModel;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.populators.BlProductPopulator;

@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class BlProductPopulatorTest {

  @InjectMocks
  private final BlProductPopulator populator = Mockito.spy(new BlProductPopulator());

  private static final String PRODUCT_CODE = "testcode";
  private static final String DISPLAY_NAME = "test product";
  private static final String RENTAL_INCLUDE = "This is rental include related data";
  private static final String SHORT_DESCRIPTION = "This is sort description";
  private static final String DISPLAY_NOTE = "This is display note related data";
  private static final String VIDEO_TITLE = "Test video title";
  private static final Long VIDEO_DURATION = 345L;
  private static final String VIDEO_URL = "https://www.borrowlenses.com";

  @Mock
  BlProductModel productModel;

  private List<ProductVideoModel> productVideoList;

  @Mock
  private Populator<BlProductModel, ProductData> blProductTagPopulator;

  private final ProductData productData = new ProductData();

  @Before
  public void prepare() {
	  // MockitoAnnotations.initMocks(this);
    final ProductVideoModel productVideoModel;
    productVideoList= new ArrayList<>();
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
