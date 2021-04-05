package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.ProductVideoModel;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.ProductVideoData;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.variants.model.VariantProductModel;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import javax.annotation.Resource;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;

/*
 * This populator is used for populating bl related specific product attribute.
 *  @author  Vijay Vishwakarma
 */
public class BlProductPopulator implements Populator<ProductModel, ProductData> {

  @Resource(name = "imageConverter")
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
          Date duration= productVideoModel.getVideoDuration();
          DateFormat dateFormat = new SimpleDateFormat(BlFacadesConstants.TIME_FORMAT_STRING);
          String formattedTime= dateFormat.format(duration);
          productVideoData.setVideoDuration(formattedTime);
          videoDataList.add(productVideoData);
        }
        );
    return videoDataList;
  }

  protected Object getProductCollectionAttribute(final ProductModel productModel, final String attribute)
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
      final ImageData imagedata = imageConverter.convert(mediaModel);
      imageList.add(imagedata);
    }
    target.setData_Sheet(imageList);
  }

  protected Object getProductAttribute(final ProductModel productModel, final String attribute)
  {
    final Object value = getModelService().getAttributeValue(productModel, attribute);
    if (value == null && productModel instanceof VariantProductModel)
    {
      final ProductModel baseProduct = ((VariantProductModel) productModel).getBaseProduct();
      if (baseProduct != null)
      {
        return getProductAttribute(baseProduct, attribute);
      }
    }
    return value;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

}
