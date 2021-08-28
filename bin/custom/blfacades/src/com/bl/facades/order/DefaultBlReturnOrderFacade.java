package com.bl.facades.order;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.returns.model.ReturnRequestModel;

import javax.annotation.Resource;

import com.bl.core.order.BlReturnOrderService;

public class DefaultBlReturnOrderFacade implements  BlReturnOrderFacade {

  @Resource(name = "blReturnOrderService")
  private BlReturnOrderService blReturnOrderService;

  @Override
  public ReturnRequestModel createReturnRequest(final OrderModel orderModel, final String productReturnInfo) {
    return blReturnOrderService.createReturnRequest(orderModel, productReturnInfo);
  }
}
