package com.bl.core.search.solrfacetsearch.provider.impl;

import com.bl.core.media.impl.DefaultBlMediaContainerService;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.solrfacetsearch.config.IndexConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
import de.hybris.platform.solrfacetsearch.config.exceptions.FieldValueProviderException;
import de.hybris.platform.solrfacetsearch.provider.FieldNameProvider;
import de.hybris.platform.solrfacetsearch.provider.FieldValue;
import de.hybris.platform.solrfacetsearch.provider.FieldValueProvider;
import de.hybris.platform.solrfacetsearch.provider.impl.AbstractPropertyFieldValueProvider;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Manikandan
 * This value provider created for indexing image to solr
 */
public class BlImageValueProvider extends AbstractPropertyFieldValueProvider implements
    FieldValueProvider {

  private static final String BL_IMAGE = "blimage";

  private String mediaFormat;
  private MediaService mediaService;
  private FieldNameProvider fieldNameProvider;


  private DefaultBlMediaContainerService defaultBlMediaContainerService;

  @Override
  public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig, final IndexedProperty indexedProperty,
      final Object model) throws FieldValueProviderException
  {
    if (model instanceof ProductModel)
    {
      final MediaFormatModel mediaFormatModel = getMediaService().getFormat(getMediaFormat());
      if (mediaFormatModel != null)
      {
        // To get the list of media model to be index to solr
        final List<MediaModel> mediaModelList = findMediaList((ProductModel) model,mediaFormatModel);
        if (CollectionUtils.isNotEmpty(mediaModelList)) {
          return createFieldValues(indexedProperty, mediaModelList);
        }
      }
    }
    return Collections.emptyList();
  }

  /*
   * To get the list of media model
   */
  private List<MediaModel> findMediaList(final ProductModel productModel,
      final MediaFormatModel mediaFormatModel) {

    if(null != productModel && null != mediaFormatModel) {
      final List<MediaContainerModel> galleryImages = productModel.getGalleryImages();
      if (null != galleryImages && !galleryImages.isEmpty())
      {
        for (final MediaContainerModel container : galleryImages)
        {
           return getDefaultBlMediaContainerService().getMediaForFormatList(container,mediaFormatModel); //NOSONAR
          }
      }
    }
    return Collections.emptyList();
  }



  private Collection<FieldValue> createFieldValues(final IndexedProperty indexedProperty,
      final List<MediaModel> mediaList)
  {
    return createFieldValuesForList(indexedProperty, mediaList);
  }

  private Collection<FieldValue> createFieldValuesForList(final IndexedProperty indexedProperty,
      List<MediaModel> mediaListModels)
  {
    final List<FieldValue> fieldValues = new ArrayList<>();

    final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
    String mediaString;

    String splitter = BL_IMAGE;

    mediaString = mediaListModels.stream().map(mediaListModel -> mediaListModel.getURL() + splitter)
        .collect(Collectors.joining());

    String value = mediaString;

   if(mediaString.endsWith(splitter)) {
     value = StringUtils.removeEnd(mediaString, splitter);
    }
    for (final String fieldName : fieldNames)
    {
      fieldValues.add(new FieldValue(fieldName, value));
    }

    return fieldValues;
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


  public FieldNameProvider getFieldNameProvider() {
    return fieldNameProvider;
  }

  public void setFieldNameProvider(
      FieldNameProvider fieldNameProvider) {
    this.fieldNameProvider = fieldNameProvider;
  }


  public DefaultBlMediaContainerService getDefaultBlMediaContainerService() {
    return defaultBlMediaContainerService;
  }

  public void setDefaultBlMediaContainerService(
      DefaultBlMediaContainerService defaultBlMediaContainerService) {
    this.defaultBlMediaContainerService = defaultBlMediaContainerService;
  }

}
