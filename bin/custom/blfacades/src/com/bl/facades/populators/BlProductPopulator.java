package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.ProductVideoModel;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.data.ProductVideoData;
import com.bl.facades.product.data.SerialProductData;

import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.PredicateUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.time.DurationFormatUtils;

/*
 * This populator is used for populating bl related specific product attribute.
 *  @author  Vijay Vishwakarma
 */
public class BlProductPopulator implements Populator<BlProductModel, ProductData> {

    private Converter<MediaModel, ImageData> imageConverter;

    private ModelService modelService;
    private Populator<BlProductModel, ProductData> blProductTagPopulator;
    private PriceDataFactory priceDataFactory;
    private CommonI18NService commonI18NService;

    @Override
    public void populate(final BlProductModel source, final ProductData target) {
        target.setDisplayName(source.getDisplayName());
        target.setRentalIncludes(source.getRentalIncludes());
        target.setForRent(BooleanUtils.toBoolean(source.getForRent()));
        target.setShortDescription(source.getShortDescription());
        target.setRentalVideosLink(
                populateVideo(CollectionUtils.emptyIfNull(source.getRentalVideosLink())));
        target.setUsedIncludes(source.getUsedIncludes());
        target.setForSale(BooleanUtils.toBoolean(source.getForSale()));
        target.setUsedGearVideosLink(
                populateVideo(CollectionUtils.emptyIfNull(source.getUsedGearVideosLink())));
        target.setRentalNote(source.getDisplayNotes());
        final Collection<MediaModel> dataSheets = (Collection<MediaModel>) getModelService()
                .getAttributeValue(source,
                        ProductModel.DATA_SHEET);
        if (CollectionUtils.isNotEmpty(dataSheets)) {
            populateResourceData(dataSheets, target);
        }
        target.setIsDiscontinued(BooleanUtils.toBoolean(source.getDiscontinued()));
        target.setIsNew(BooleanUtils.toBoolean(source.getIsNew()));
        target.setIsUpcoming(CollectionUtils.isEmpty(source.getSerialProducts()));
        target.setUsedDescription(source.getUsedDescription());  
        //populates list of serial product data assigned to SKU
        populateSerialProducts(source,target);
        getBlProductTagPopulator().populate(source, target);
    }

    /*
     * This method used to populate video related information.
     */
    private List<ProductVideoData> populateVideo(final Collection<ProductVideoModel> populateVideos) {
        final List<ProductVideoData> videoDataList = new ArrayList<>();
        populateVideos.forEach(productVideoModel -> {
                    ProductVideoData productVideoData = new ProductVideoData();
                    productVideoData.setVideoName(productVideoModel.getVideoTitle());
                    productVideoData.setVideoUrl(productVideoModel.getVideoLink());
                    productVideoData.setVideoDuration(
                            DurationFormatUtils.formatDuration(productVideoModel.getVideoDuration() * 1000,
                                    BlFacadesConstants.TIME_FORMAT_STRING));
                    videoDataList.add(productVideoData);
                }
        );
        return videoDataList;
    }

    /*
     * This method is used for populating resource related pdf data
     */
    private void populateResourceData(final Collection<MediaModel> dataSheet,
                                      final ProductData target) {
        final Collection<ImageData> imageList = new ArrayList<>();
        for (final MediaModel mediaModel : dataSheet) {
            final ImageData imagedata = getImageConverter().convert(mediaModel);
            imageList.add(imagedata);
        }
        target.setDataSheet(imageList);
    }

    /*
     * This method is used for populating serial product.
     */
    private void populateSerialProducts(final BlProductModel source, final ProductData target) {
   	 final List<SerialProductData> serialProductDataList = new ArrayList<>();
   	 final List<BlSerialProductModel> blSerialProductModels = new ArrayList<>(CollectionUtils.emptyIfNull(source.getSerialProducts()));
        blSerialProductModels.forEach(serialProductModel -> {
                    SerialProductData serialProductData = new SerialProductData();
            if (serialProductModel.getConditionRatingOverallScore() != null) {
                serialProductData
                    .setConditionRating(
                        serialProductModel.getConditionRatingOverallScore() + BlFacadesConstants.DEFAULT_CONDITIONAL_RATING);
            } else {
                serialProductData.setConditionRating(BlFacadesConstants.DEFAULT_CONDITIONAL_RATING);
            }
                    serialProductData.setSerialId(serialProductModel.getProductId());
                    if(PredicateUtils.notNullPredicate().evaluate(serialProductModel.getFinalSalePrice())) {
                  	  serialProductData.setFinalSalePrice(getProductPriceData(serialProductModel.getFinalSalePrice()));
                    }
                    if(PredicateUtils.notNullPredicate().evaluate(serialProductModel.getIncentivizedPrice())) {
                  	  serialProductData.setFinalIncentivizedPrice(getProductPriceData(serialProductModel.getIncentivizedPrice()));
                  	  target.setHasIncentivizedPrice(Boolean.TRUE);
                    }
                    serialProductDataList.add(serialProductData);
                }
        );
        sortSerialBasedOnConditionRating(serialProductDataList);
        target.setSerialproducts(serialProductDataList);
    }
    
    /**
     * Sorting serial products in Ascending Order based on condition rating.
     *
     * @param serialProductDataList the serial product data list
     * @return the list
     */
    private List<SerialProductData> sortSerialBasedOnConditionRating(final List<SerialProductData> serialProductDataList)
    {
   	 if (CollectionUtils.isNotEmpty(serialProductDataList))
       {
   		 Collections.sort(serialProductDataList, new Comparator<SerialProductData>()
   		 {
   			 @Override
   			 public int compare(final SerialProductData serialProductData1, final SerialProductData serialProductData2)
   			 {
   				 return compareSerialProductData(serialProductData1, serialProductData2);
   			 }
   		 });
       }
       return serialProductDataList;
   }

   /**
    * Compare serial product data.
    *
    * @param serialProductData1 the serial product data 1
    * @param serialProductData2 the serial product data 2
    * @return the int
    */
   private int compareSerialProductData(final SerialProductData serialProductData1, final SerialProductData serialProductData2)
   {
   	if (serialProductData1 != null )
   	{
   		if (serialProductData2 != null )
   		{
   			return Double.valueOf(serialProductData1.getConditionRating())
   					.compareTo(serialProductData2.getConditionRating());
   		}
   		else
   		{
   			return 1;
   		}
   	}
   	else
   	{
   		if (serialProductData2 != null )
   		{
   			return -1;
   		}
   	}
   	return 0;
   }
    
    /**
     * Gets the product price data.
     *
     * @param priceValue the price value
     * @return the product price data
     */
    private PriceData getProductPriceData(final BigDecimal priceValue){
   		return getPriceDataFactory().create(PriceDataType.BUY, priceValue, getCommonI18NService().getCurrentCurrency());
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

    public Populator<BlProductModel, ProductData> getBlProductTagPopulator() {
        return blProductTagPopulator;
    }

    public void setBlProductTagPopulator(Populator<BlProductModel, ProductData> blProductTagPopulator) {
        this.blProductTagPopulator = blProductTagPopulator;
    }

	/**
	 * @return the priceDataFactory
	 */
	public PriceDataFactory getPriceDataFactory()
	{
		return priceDataFactory;
	}

	/**
	 * @param priceDataFactory the priceDataFactory to set
	 */
	public void setPriceDataFactory(PriceDataFactory priceDataFactory)
	{
		this.priceDataFactory = priceDataFactory;
	}

	/**
	 * @return the commonI18NService
	 */
	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	/**
	 * @param commonI18NService the commonI18NService to set
	 */
	public void setCommonI18NService(CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
	}


}

