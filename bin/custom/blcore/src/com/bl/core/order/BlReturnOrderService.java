package com.bl.core.order;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.returns.ReturnService;
import de.hybris.platform.returns.model.ReturnRequestModel;


public interface BlReturnOrderService extends ReturnService
{
 ReturnRequestModel createReturnRequest(final OrderModel orderModel, final String productList);
}
