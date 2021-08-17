package com.bl.core.order;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.returns.ReturnService;
import de.hybris.platform.returns.model.ReturnRequestModel;

import java.util.List;


public interface BlReturnOrderService extends ReturnService
{

 ReturnRequestModel createReturnRequest(OrderModel orderModel, List<String> productReturnInfo);
}
