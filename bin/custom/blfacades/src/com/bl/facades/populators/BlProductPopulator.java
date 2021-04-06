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
       {
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
      final Collection<MediaModel> dataSheets = (Collection<MediaModel>)  getModelService().getAttributeValue(blProductModel,
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
          productVideoData.setVideoDuration(formattedTime(productVideoModel.getVideoDuration()));
          videoDataList.add(productVideoData);
        }
        );
    return videoDataList;
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
   * This method provide time in hh:mm:ss format.
   */
  private String formattedTime(long duration){
    final int hour = (int)duration/3600;
    final int minutes = (int)duration%3600;
    final int remainingMinutes= minutes/60;
    final int remainingSecond= minutes%60;
    String formattedTime = hour>1 ? (hour+(remainingMinutes>1 ? ":":"")):"";
    formattedTime+=(remainingMinutes>1? (remainingMinutes+(remainingSecond>1? ":":"") ):"");
    formattedTime+=remainingSecond;
    return formattedTime;
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
