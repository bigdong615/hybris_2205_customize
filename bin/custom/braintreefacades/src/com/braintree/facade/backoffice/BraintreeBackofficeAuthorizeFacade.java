package com.braintree.facade.backoffice;

import de.hybris.platform.core.model.order.OrderModel;

public interface BraintreeBackofficeAuthorizeFacade {

    boolean isAuthorizePossible(final OrderModel order);

}
