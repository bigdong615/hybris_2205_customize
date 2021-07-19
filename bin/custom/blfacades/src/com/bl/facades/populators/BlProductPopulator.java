package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;

import de.hybris.platform.commercefacades.product.data.ImageData;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.media.MediaModel;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;

/*
 * This populator is used for populating bl Rental Product related specific product attribute.
 *  @author  Vijay Vishwakarma
 */
public class BlProductPopulator extends AbstractBlProductPopulator implements Populator<BlProductModel, ProductData> {

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
        target.setAlternativeProduct(source.getAlternativeProduct());
        getBlProductTagPopulator().populate(source, target);
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

