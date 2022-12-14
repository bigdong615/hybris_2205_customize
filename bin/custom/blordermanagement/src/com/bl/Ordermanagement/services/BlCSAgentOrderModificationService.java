package com.bl.Ordermanagement.services;

import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;

import java.util.List;

public interface BlCSAgentOrderModificationService {
    /**
     * Add new entry to existing order by CS Agent in CS Cockpit
     *
     * @param orderEntryModel
     * @param interceptorContext
     * @param originalSerialProducts
     * @param isOrderModified
     */
    void addNewOrderEntry(final OrderEntryModel orderEntryModel, final InterceptorContext interceptorContext, List originalSerialProducts, boolean isOrderModified) throws InterceptorException;

    /**
     *  Modify existing entry when quantity is modified
     * @param orderEntryModel
     * @param interceptorContext
     * @param originalSerialProducts
     * @param isOrderModified
     */
    void modifyExistingEntryForQuantity(final OrderEntryModel orderEntryModel, final InterceptorContext interceptorContext, List originalSerialProducts, boolean isOrderModified) throws InterceptorException;

    /**
     * Allowed statuses for modification
     * @param orderStatus
     * @return
     */
    boolean allowedOrderStatusforModification(final OrderStatus orderStatus);
}
