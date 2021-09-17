package com.bl.core.services.order;

import de.hybris.platform.core.model.order.AbstractOrderModel;

/**
 * Order service for various order related functionalities.
 *
 * @author Sunil Sahu
 */
public interface BlOrderService {

  /**
   * It checks whether the order contains atleast one aquatech product
   * @param orderModel
   * @return true if the given order contains aquatech product
   */
  boolean isAquatechProductsPresentInOrder(final AbstractOrderModel orderModel);

  /**
   * It checks whether the order contains only aquatech products
   * @param orderModel
   * @return true if the given order contains only aquatech product
   */
  boolean isAquatechProductOrder(final AbstractOrderModel orderModel);
  
  /**
   * Check and update order status.
   *
   * @param order the order
   */
  public void checkAndUpdateOrderStatus(final AbstractOrderModel order);
  
  /**
   * Sets the resolved status on repair log for order code.
   *
   * @param orderCode the new resolved status on repair log
   */
  public void setResolvedStatusOnRepairLog(final String orderCode);
  
}
