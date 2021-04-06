package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.ProductVideoModel;
import com.bl.facades.product.data.ProductVideoData;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;

/*
 * This populator is used for populating bl related specific product attribute.
 *  @author  Vijay Vishwakarma
 */
public class BlProductPopulator implements Populator<ProductModel, ProductData> {

  private Converter<MediaModel, ImageData> imageConverter;

  private ModelService modelService;

  @Override
  public void populate(ProductModel source, ProductData target)
      throws ConversionException {
    if(PredicateUtils.instanceofPredicate(BlProductModel.class).evaluate(source)){
      BlProductModel blProductModel = (BlProductModel)source;
      target.setPurchaseNotes(blProductModel.getPurchaseNotes());
      target.setDisplayName(blProductModel.getDisplayName());
      target.setRentalIncludes(blProductModel.getRentalIncludes());
      target.setForRent(BooleanUtils.toBoolean(blProductModel.getForRent()));
      target.setShortDescription(blProductModel.getShortDescription());
      target.setRentalVideosLink(populateVideo(CollectionUtils.emptyIfNull(blProductModel.getRentalVideosLink())));
      target.setUsedIncludes(blProductModel.getUsedIncludes());
      target.setForSale(BooleanUtils.toBoolean(blProductModel.getForSale()));
      target.setUsedGearVideosLink(populateVideo(CollectionUtils.emptyIfNull(blProductModel.getUsedGearVideosLink())));
      target.setRentalNote(blProductModel.getDisplayNotes());
      final Collection<MediaModel> dataSheets = (Collection<MediaModel>) getProductCollectionAttribute(blProductModel,
          ProductModel.DATA_SHEET);
      if (CollectionUtils.isNotEmpty(dataSheets))
      {
        populateResourceData(dataSheets, target);
      }

    }

  }

  /*
   * This method is used for populating video related data.
   */
  List<ProductVideoData> populateVideo( Collection<ProductVideoModel> populateVideos ){
    List<ProductVideoData> videoDataList =  new ArrayList<>();
    populateVideos.forEach(productVideoModel -> {
          ProductVideoData productVideoData=new ProductVideoData();
          productVideoData.setVideoName(productVideoModel.getVideoTitle());
          productVideoData.setVideoUrl(productVideoModel.getVideoLink());
          final long duretion =  productVideoModel.getVideoDuration();
          final int hour = (int)duretion/3600;
          final int minuts = (int)duretion%3600;
          final int remainingMinuts= minuts/60;
          final int remainingSecond= minuts%60;
          String formattedTime = hour>1 ? hour+(remainingMinuts>1 ? ":":""):"";
          formattedTime+=(remainingMinuts>1? remainingMinuts+(remainingSecond>1? ":":"") :"");
          formattedTime+=remainingSecond;
          productVideoData.setVideoDuration(formattedTime);
          videoDataList.add(productVideoData);
        }
        );
    return videoDataList;
  }

  /*
   * This method provide media resource.
   */
  private Object getProductCollectionAttribute(final ProductModel productModel, final String attribute)
  {
    final Object value = getModelService().getAttributeValue(productModel, attribute);
    if (value instanceof Collection && CollectionUtils.isEmpty((Collection) value))
    {
      return getProductAttribute(productModel, attribute);

    }
    return value;
  }

 /*
  * This method is used for populating resource related pdf data
  */
  private void populateResourceData(final Collection<MediaModel> data_sheet, final ProductData target)
  {
    final Collection<ImageData> imageList = new ArrayList<ImageData>();
    for (final MediaModel mediaModel : data_sheet)
    {
      final ImageData imagedata = getImageConverter().convert(mediaModel);
      imageList.add(imagedata);
    }
    target.setData_Sheet(imageList);
  }
/*
 * This method provide product media
 */
  private Object getProductAttribute(final ProductModel productModel, final String attribute)
  {
    return  getModelService().getAttributeValue(productModel, attribute);
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public Converter<MediaModel, ImageData> getImageConverter() {
    return imageConverter;
  }

  public void setImageConverter(
      Converter<MediaModel, ImageData> imageConverter) {
    this.imageConverter = imageConverter;
  }


}
