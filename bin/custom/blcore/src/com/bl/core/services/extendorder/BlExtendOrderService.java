package com.bl.core.services.extendorder;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;

public interface BlExtendOrderService {

  OrderModel cloneOrderModelForExtendRental(final OrderModel originalOrder , final long defaultAddedTimeForExtendRental);

  void updateExtendOrder(final AbstractOrderModel orderModel);
}
