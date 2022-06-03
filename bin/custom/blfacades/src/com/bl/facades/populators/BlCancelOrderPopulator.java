package com.bl.facades.populators;

import com.bl.core.model.handler.BlOrderEntryCancelledQuantityHandler;
import de.hybris.platform.basecommerce.enums.CancelReason;
import de.hybris.platform.basecommerce.enums.OrderModificationEntryStatus;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.ordercancel.model.OrderCancelRecordEntryModel;
import de.hybris.platform.ordercancel.model.OrderCancelRecordModel;
import de.hybris.platform.ordercancel.model.OrderEntryCancelRecordEntryModel;
import de.hybris.platform.orderhistory.model.OrderHistoryEntryModel;
import de.hybris.platform.ordermodify.model.OrderModificationRecordEntryModel;
import de.hybris.platform.ordermodify.model.OrderModificationRecordModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.log4j.Logger;

import java.util.Date;

public class BlCancelOrderPopulator implements Populator<OrderEntryData , OrderEntryModel> {


    private static final Logger LOG = Logger.getLogger(BlCancelOrderPopulator.class);

    private ModelService modelService;

    @Override
    public void populate(OrderEntryData orderEntryData , OrderEntryModel orderEntryModel) throws ConversionException {
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
