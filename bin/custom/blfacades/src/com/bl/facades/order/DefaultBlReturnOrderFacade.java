package com.bl.facades.order;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.order.BlReturnOrderService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.returns.ReturnService;
import de.hybris.platform.returns.model.ReturnRequestModel;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.util.List;

public class DefaultBlReturnOrderFacade implements  BlReturnOrderFacade {

  private static final Logger LOG = Logger.getLogger(DefaultBlReturnOrderFacade.class);

  @Resource(name = "blReturnOrderService")
  private BlReturnOrderService blReturnOrderService;

  @Override
  public ReturnRequestModel createReturnRequest(OrderModel orderModel, List<String> productReturnInfo) {
    return blReturnOrderService.createReturnRequest(orderModel, productReturnInfo);
  }
}
