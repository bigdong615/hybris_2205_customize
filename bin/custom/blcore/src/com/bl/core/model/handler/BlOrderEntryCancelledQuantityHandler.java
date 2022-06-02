package com.bl.core.model.handler;

//import de.hybris.platform.core.model.order.BlOrderEntryModel;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;
import de.hybris.platform.warehousing.cancellation.impl.OrderEntryCancelledQuantityHandler;
import de.hybris.platform.warehousing.orderentry.service.OrderEntryQuantityService;
import org.springframework.beans.factory.annotation.Required;

public class BlOrderEntryCancelledQuantityHandler extends OrderEntryCancelledQuantityHandler {

    @Override
    public Long get(OrderEntryModel orderEntry) {
       return  orderEntry.getCancelledQuantity();

    }





}
