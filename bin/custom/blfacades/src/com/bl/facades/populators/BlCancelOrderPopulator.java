package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.log4j.Logger;

/**
 * This class created to populate cancelled orderEntryData
 * @author Manikandan
 */
public class BlCancelOrderPopulator implements Populator<OrderEntryData , OrderEntryModel> {

    private ModelService modelService;

    @Override
    public void populate(final OrderEntryData orderEntryData, final OrderEntryModel orderEntryModel) throws ConversionException {
        orderEntryModel.setCancelledQuantity(orderEntryData.getCancellableQty());
        orderEntryModel.setQuantity(0L);
        modelService.save(orderEntryModel);
        modelService.refresh(orderEntryModel);
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

}
