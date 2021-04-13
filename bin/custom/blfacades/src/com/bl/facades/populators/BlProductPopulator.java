package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.model.ProductVideoModel;
import com.bl.facades.constants.BlFacadesConstants;
import com.bl.facades.product.SerialProductData;
import com.bl.facades.product.data.ProductVideoData;
import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections4.CollectionUtils;
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
        target.setSerialproducts(populatedSerialProduct(CollectionUtils.emptyIfNull(source.getSerialProducts())));
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
    private List<SerialProductData> populatedSerialProduct(final Collection<BlSerialProductModel> blSerialProductModels) {
        final List<SerialProductData> serialProductDataList = new ArrayList<>();
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
                    serialProductDataList.add(serialProductData);
                }
        );
        Collections.sort(serialProductDataList);
        return serialProductDataList;
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


}

