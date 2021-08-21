package com.bl.facades.order;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.returns.model.ReturnRequestModel;


public interface BlReturnOrderFacade {

  ReturnRequestModel createReturnRequest(OrderModel orderModel, String productReturnInfo);
}
