package com.bl.core.model.handler;

import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.warehousing.cancellation.impl.OrderEntryCancelledQuantityHandler;

public class BlOrderEntryCancelledQuantityHandler extends OrderEntryCancelledQuantityHandler {

    @Override
    public void set(OrderEntryModel orderEntry, Long value) {
        System.out.println(orderEntry);
    }

}
