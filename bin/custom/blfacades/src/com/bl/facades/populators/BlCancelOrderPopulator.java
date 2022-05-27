package com.bl.facades.populators;

import com.bl.core.model.handler.BlOrderEntryCancelledQuantityHandler;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.warehousing.cancellation.impl.OrderEntryCancelledQuantityHandler;

public class BlCancelOrderPopulator implements Populator<OrderEntryData , OrderEntryModel> {

   // private BlOrderEntryCancelledQuantityHandler blOrderEntryCancelledQuantityHandler;
    @Override
    public void populate(OrderEntryData orderEntryData , OrderEntryModel orderEntryModel) throws ConversionException {
    orderEntryModel.setQuantity(0L);
      //  blOrderEntryCancelledQuantityHandler.set(orderEntryModel , orderEntryData.getCancellableQty());
    }

}
