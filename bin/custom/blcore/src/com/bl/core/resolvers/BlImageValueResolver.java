package com.bl.core.resolvers;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.media.impl.DefaultBlMediaContainerService;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.indexer.IndexerBatchContext;
import de.hybris.platform.solrfacetsearch.indexer.spi.InputDocument;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractValueResolver;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

public class BlImageValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {
  private String mediaFormat;
  private MediaService mediaService;
  private DefaultBlMediaContainerService defaultBlMediaContainerService;

  @Override
  protected void addFieldValues(InputDocument inputDocument,
      IndexerBatchContext indexerBatchContext, IndexedProperty indexedProperty,
      BlProductModel blProductModel, ValueResolverContext<Object, Object> valueResolverContext)
      throws FieldValueProviderException {
    final MediaFormatModel mediaFormatModel = getMediaService().getFormat(getMediaFormat());
    if (null != mediaFormatModel)
    {
      // To get the list of media model to be index to solr
      final List<MediaModel> mediaModelList = findMediaList(blProductModel,mediaFormatModel);
      if (CollectionUtils.isNotEmpty(mediaModelList)) {
        inputDocument.addField(indexedProperty,createFieldValuesForList(indexedProperty,mediaModelList));
      }
    }

  }



  private List<MediaModel> findMediaList(final BlProductModel productModel, final MediaFormatModel mediaFormatModel) {
    final List<MediaContainerModel> galleryImages = productModel.getGalleryImages();
    if (CollectionUtils.isNotEmpty(galleryImages))
    {
      return getMediaList(galleryImages,mediaFormatModel);
    }
    return Collections.emptyList();
  }

  private List<MediaModel> getMediaList(final List<MediaContainerModel> galleryImages ,final MediaFormatModel mediaFormatModel) {
    for (final MediaContainerModel container : galleryImages)
    {
      final List<MediaModel> mediaModelList = getDefaultBlMediaContainerService().getMediaForFormatList(container,mediaFormatModel);
      if(CollectionUtils.isNotEmpty(mediaModelList)) {
        return mediaModelList;
      }
    }
    return Collections.emptyList();
  }

  private String createFieldValuesForList(final IndexedProperty indexedProperty,
      final List<MediaModel> mediaModelList)
  {
    String mediaString;
    // Used to split the images in a form of string
    mediaString = mediaModelList.stream().map(mediaListModel -> mediaListModel.getURL() + BlCoreConstants.BL_IMAGE)
        .collect(Collectors.joining());
    String value = mediaString;
    // removing the last spliiter from string
    if(mediaString.endsWith(BlCoreConstants.BL_IMAGE)) {
      value = StringUtils.removeEnd(mediaString, BlCoreConstants.BL_IMAGE);
    }
   return value;
  }

  public String getMediaFormat() {
    return mediaFormat;
  }

  public void setMediaFormat(String mediaFormat) {
    this.mediaFormat = mediaFormat;
  }

  public MediaService getMediaService() {
    return mediaService;
  }

  public void setMediaService(MediaService mediaService) {
    this.mediaService = mediaService;
  }


  public DefaultBlMediaContainerService getDefaultBlMediaContainerService() {
    return defaultBlMediaContainerService;
  }

  public void setDefaultBlMediaContainerService(
      DefaultBlMediaContainerService defaultBlMediaContainerService) {
    this.defaultBlMediaContainerService = defaultBlMediaContainerService;
  }

}
