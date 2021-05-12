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

/**
 * This Value Provider Created for Indexing Image to solr
 * @author Manikandan
 */
public class BlImageValueResolver extends
    AbstractValueResolver<BlProductModel, Object, Object> {
  private String mediaFormat;
  private MediaService mediaService;
  private DefaultBlMediaContainerService defaultBlMediaContainerService;

  /**
   * This method created for adding field values to solr for images indexed property
   * @param inputDocument  inputDocument adds field values to solr
   * @param indexerBatchContext indexerBatchContext
   * @param indexedProperty indexedProperty to be index
   * @param blProductModel blProductModel
   * @param valueResolverContext valueResolverContext
   * @throws FieldValueProviderException throws exception
   */
  @Override
  protected void addFieldValues(final InputDocument inputDocument,
      final IndexerBatchContext indexerBatchContext, final IndexedProperty indexedProperty,
      final BlProductModel blProductModel, final ValueResolverContext<Object, Object> valueResolverContext)
      throws FieldValueProviderException {
    final MediaFormatModel mediaFormatModel = getMediaService().getFormat(getMediaFormat());
    if (null != mediaFormatModel)
    {
      // To get the list of media model to be index to solr
      final List<MediaModel> mediaModelList = findMediaList(blProductModel,mediaFormatModel);
      if (CollectionUtils.isNotEmpty(mediaModelList)) {
        inputDocument.addField(indexedProperty,createFieldValuesForList(mediaModelList));
      }
    }

  }


  /**
   * This method created for finding media list
   * @param productModel productmodel
   * @param mediaFormatModel mediaFormatModel
   * @return List<MediaModel>
   */
  private List<MediaModel> findMediaList(final BlProductModel productModel, final MediaFormatModel mediaFormatModel) {
    final List<MediaContainerModel> galleryImages = productModel.getGalleryImages();
    if (CollectionUtils.isNotEmpty(galleryImages))
    {
      return getMediaList(galleryImages,mediaFormatModel);
    }
    return Collections.emptyList();
  }

  /**
   * This method created to get Media list from Data Base for solr
   * @param galleryImages galleryImages
   * @param mediaFormatModel mediaFormatModel
   * @return List<MediaModel>
   */
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

  /**
   * This method created to append the list of medias
   * @param mediaModelList mediaModelList
   * @return String
   */
  private String createFieldValuesForList(final List<MediaModel> mediaModelList)
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
