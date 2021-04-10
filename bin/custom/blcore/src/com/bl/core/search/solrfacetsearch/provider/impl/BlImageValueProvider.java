package com.bl.core.search.solrfacetsearch.provider.impl;

import com.bl.core.media.impl.DefaultBlMediaContainerService;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.core.model.media.MediaContainerModel;
import de.hybris.platform.core.model.media.MediaFormatModel;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.servicelayer.media.MediaService;
import de.hybris.platform.solrfacetsearch.config.IndexConfig;
import de.hybris.platform.solrfacetsearch.config.IndexedProperty;
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
 * This value provider created for indexing image to solr
 * @author Manikandan
 */
public class BlImageValueProvider extends AbstractPropertyFieldValueProvider implements
    FieldValueProvider {

  private static final String BL_IMAGE = "blimage";

  private String mediaFormat;
  private MediaService mediaService;
  private FieldNameProvider fieldNameProvider;
  private DefaultBlMediaContainerService defaultBlMediaContainerService;

  /**
   * this methd created for getting filed values for solr
   * @param indexConfig indexconfig of solr config property
   * @param indexedProperty indexedPropert for solr
   * @param model defines the product model
   * @return  Collection<FieldValue> to solr
   */
  @Override
  public Collection<FieldValue> getFieldValues(final IndexConfig indexConfig, final IndexedProperty indexedProperty,
      final Object model) {
    if (model instanceof BlProductModel)
    {
      final MediaFormatModel mediaFormatModel = getMediaService().getFormat(getMediaFormat());
      if (null != mediaFormatModel)
      {
        // To get the list of media model to be index to solr
        final List<MediaModel> mediaModelList = findMediaList((BlProductModel) model,mediaFormatModel);
        if (CollectionUtils.isNotEmpty(mediaModelList)) {
          return createFieldValuesForList(indexedProperty,mediaModelList);
        }
      }
    }
    return Collections.emptyList();
  }

  /**
   * This method gets the list of media model from media container to index to solr
   * @param productModel product
   * @param mediaFormatModel meida format for product
   * @return List<MediaModel> to solr
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
   * This method created for getting list of media model
   * @param galleryImages list of galley images
   * @param mediaFormatModel media format type
   * @return  List<MediaModel> for product
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
   * this method created for creating field values for solr
   * @param indexedProperty indexed property for solr
   * @param mediaModelList media model for product
   * @return Collection<FieldValue> to be indexed to solr
   */
  private Collection<FieldValue> createFieldValuesForList(final IndexedProperty indexedProperty,
      final List<MediaModel> mediaModelList)
  {
    final List<FieldValue> fieldValues = new ArrayList<>();

    final Collection<String> fieldNames = getFieldNameProvider().getFieldNames(indexedProperty, null);
    String mediaString;

    final String splitter = BL_IMAGE;
    // Used to split the images in a form of string
    mediaString = mediaModelList.stream().map(mediaListModel -> mediaListModel.getURL() + splitter)
        .collect(Collectors.joining());

    String value = mediaString;

    // removing the last spliiter from string
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
