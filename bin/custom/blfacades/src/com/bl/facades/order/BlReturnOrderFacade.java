package com.bl.facades.order;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.returns.model.ReturnRequestModel;

import java.util.List;


public interface BlReturnOrderFacade {

  ReturnRequestModel createReturnRequest(OrderModel orderModel, List<String> productReturnInfo);

}
