package com.braintree.customersupportbackoffice.services;

import de.hybris.platform.core.model.order.OrderModel;

@Deprecated
public interface BrainTreeOrderManagementActionsService {
    boolean isMultipleCapturePossible(OrderModel order);
    
    boolean isPartialRefundPossible(OrderModel order);

    boolean isMultiCaptureEnabled();

//    boolean isIntentOrder(OrderModel order);

    boolean isAvailableOrderAuthorization(OrderModel order);

    boolean isRefundPossible(OrderModel order);

    boolean isReplacePossible(OrderModel order);
}
