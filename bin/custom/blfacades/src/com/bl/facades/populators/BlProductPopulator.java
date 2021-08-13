package com.bl.facades.populators;

import com.bl.core.model.BlProductModel;
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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;

/*
 * This populator is used for populating bl Rental Product related specific product attribute.
 *  @author  Vijay Vishwakarma
 */
public class BlProductPopulator extends AbstractBlProductPopulator implements Populator<BlProductModel, ProductData> {

    private Converter<MediaModel, ImageData> imageConverter;

    private ModelService modelService;
    private Populator<BlProductModel, ProductData> blProductTagPopulator;
    private PriceDataFactory priceDataFactory;
    private CommonI18NService commonI18NService;

    @Override
    public void populate(final BlProductModel source, final ProductData target) {
        target.setProductId(source.getProductId());
        target.setIsVideo(source.getIsVideo());
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
     
        target.setProductType(source.getProductType().getCode());
      
        target.setIsDiscontinued(BooleanUtils.toBoolean(source.getDiscontinued()));
        target.setIsNew(BooleanUtils.toBoolean(source.getIsNew()));
        target.setIsUpcoming(CollectionUtils.isEmpty(source.getSerialProducts()));
        target.setAlternativeProduct(source.getAlternativeProduct());
        target.setOnSale(source.getOnSale() != null && source.getOnSale());
        target.setUpc(StringUtils.isNotEmpty(source.getUpc()) ? source.getUpc() : StringUtils.EMPTY );
        target.setBrandName(StringUtils.isNotEmpty(source.getManufacturerName()) ? source.getManufacturerName() : StringUtils.EMPTY);
        target.setRetailGear(BooleanUtils.toBoolean(source.getRetailGear()));
        target.setRetailGearPrice(getProductPriceData(source.getRetailGearPrice()));
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

    /**
     * Gets the product price data.
     *
     * @param priceValue
     *           the price value
     * @return the product price data
     */
    private PriceData getProductPriceData(final BigDecimal priceValue)
    {
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

    public PriceDataFactory getPriceDataFactory() {
        return priceDataFactory;
    }

    public void setPriceDataFactory(
        PriceDataFactory priceDataFactory) {
        this.priceDataFactory = priceDataFactory;
    }

    public CommonI18NService getCommonI18NService() {
        return commonI18NService;
    }

    public void setCommonI18NService(
        CommonI18NService commonI18NService) {
        this.commonI18NService = commonI18NService;
    }
}

