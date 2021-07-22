package com.bl.core.services.extendorder;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;

/**
 * This interface created to add customize logic for extend order
 * @author Manikandan
 */
public interface BlExtendOrderService {

  /**
   * This method created to clone the extend order from existing order
   */
  OrderModel cloneOrderModelForExtendRental(final OrderModel originalOrder , final long defaultAddedTimeForExtendRental);

  /**
   * this method created to update the extend order
   */
  void updateExtendOrder(final AbstractOrderModel orderModel);
}
