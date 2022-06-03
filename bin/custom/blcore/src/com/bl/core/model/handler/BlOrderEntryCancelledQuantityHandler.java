package com.bl.core.model.handler;

import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.warehousing.cancellation.impl.OrderEntryCancelledQuantityHandler;

/**
 * This class used for overriding the existing OrderEntryCancelledQuantityHandler's get method
 * @author Jyoti Packirisamy
 */
public class BlOrderEntryCancelledQuantityHandler extends OrderEntryCancelledQuantityHandler {

    @Override
    public Long get(final OrderEntryModel orderEntry) {
       return  orderEntry.getCancelledQuantity();
    }
}
