package com.bl.core.resolver;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import com.bl.core.media.impl.DefaultBlMediaContainerService;
import com.bl.core.model.BlProductModel;
import com.bl.core.resolvers.BlImageValueResolver;
import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.jalo.JaloSession;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.QualifierProvider;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

@UnitTest
public class BlImageValueResolverTest {

  private static final String INDEXED_PROPERTY_NAME = "img-300Wx300H";
  private static final String MEDIA_FORMAT = "300Wx300H";
  private static final String MEDIA_QUALIFIER = "Canon-EOS-1DMark-II_Small";
  private static final String MEDIA_NAME = "Canon EOS 1DMark II Small";

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
  private MediaService mediaService;
  @Mock
  private DefaultBlMediaContainerService defaultBlMediaContainerService;


  private IndexedProperty indexedProperty;


  @InjectMocks
  private BlImageValueResolver blImageValueResolver = Mockito.spy(BlImageValueResolver.class);

  @Before
  public void startUp() {
    MockitoAnnotations.initMocks(this);
    indexedProperty = new IndexedProperty();
    indexedProperty.setName(INDEXED_PROPERTY_NAME);
    indexedProperty.setValueProviderParameters(new HashMap<>());

    when(qualifierProvider.canApply(any(IndexedProperty.class))).thenReturn(Boolean.FALSE);
    when(sessionService.getRawSession(null)).thenReturn(jaloSession);

    blImageValueResolver = new BlImageValueResolver();
    blImageValueResolver.setSessionService(sessionService);
    blImageValueResolver.setQualifierProvider(qualifierProvider);
    blImageValueResolver.setMediaFormat(MEDIA_FORMAT);
    blImageValueResolver.setMediaService(mediaService);
    blImageValueResolver.setDefaultBlMediaContainerService(defaultBlMediaContainerService);
  }

  // When media is empty
  @Test
  public void resolveWhenMediaIsEmpty() throws FieldValueProviderException {
    final MediaFormatModel mediaFormatModel = new MediaFormatModel();
    when(mediaService.getFormat(Matchers.any())).thenReturn(mediaFormatModel);
    blImageValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
  }

  // When media is not empty
  @Test
  public void resolveWhenMediaIsNotEmpty() throws FieldValueProviderException {
    final MediaFormatModel mediaFormatModel = new MediaFormatModel();
    final List<MediaContainerModel> mediaContainerModels = new ArrayList<>();
    final List<MediaModel> mediaModelList = new ArrayList<>();
    final MediaModel mediaModel = new MediaModel();
    final MediaModel mediaModel1 = new MediaModel();
    final MediaContainerModel mediaContainerModel = new MediaContainerModel();
    mediaContainerModel.setQualifier("Test123");
    mediaContainerModels.add(mediaContainerModel);
    mediaFormatModel.setQualifier(MEDIA_QUALIFIER);
    mediaFormatModel.setName(MEDIA_NAME, Locale.ENGLISH);
    when(mediaService.getFormat(Matchers.any())).thenReturn(mediaFormatModel);
    blProductModel.setGalleryImages(mediaContainerModels);
    mediaModel.setAltText("image coming soon");
    mediaModel.setCode("testimg");
    mediaModel.setURL("https://cdn.static-bl.com/images/store/Canon_EOS_1DS_MKII_small.jpg");
    mediaModel1.setAltText("image coming soon");
    mediaModel1.setCode("testimg2");
    mediaModel1.setURL("https://cdn.static-bl.com/images/store/nikon_d70_sm.jpg");
    mediaModelList.add(mediaModel);
    mediaModelList.add(mediaModel1);
    when(blProductModel.getGalleryImages()).thenReturn(mediaContainerModels);
    when(defaultBlMediaContainerService.getMediaForFormatList(mediaContainerModel , mediaFormatModel)).thenReturn(mediaModelList);
    blImageValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
  }


  // When media is not empty
  @Test
  public void resolveWhenMediaModelListIsEmpty() throws FieldValueProviderException {
    final MediaFormatModel mediaFormatModel = new MediaFormatModel();
    final List<MediaContainerModel> mediaContainerModels = new ArrayList<>();
    final List<MediaModel> mediaModelList = new ArrayList<>();
    final MediaContainerModel mediaContainerModel = new MediaContainerModel();
    mediaContainerModel.setQualifier("Test123");
    mediaContainerModels.add(mediaContainerModel);
    mediaFormatModel.setQualifier(MEDIA_QUALIFIER);
    mediaFormatModel.setName(MEDIA_NAME, Locale.ENGLISH);
    when(mediaService.getFormat(Matchers.any())).thenReturn(mediaFormatModel);
    blProductModel.setGalleryImages(mediaContainerModels);
    when(blProductModel.getGalleryImages()).thenReturn(mediaContainerModels);
    when(defaultBlMediaContainerService.getMediaForFormatList(mediaContainerModel , mediaFormatModel)).thenReturn(mediaModelList);
    blImageValueResolver.resolve(inputDocument , batchContext ,  Collections.singletonList(indexedProperty) , blProductModel);
  }


}
