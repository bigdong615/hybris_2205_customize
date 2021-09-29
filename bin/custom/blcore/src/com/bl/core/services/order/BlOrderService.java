package com.bl.core.services.order;

import de.hybris.platform.catalog.model.ProductReferenceModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Order service for various order related functionalities.
 *
 * @author Sunil Sahu
 */
public interface BlOrderService {

  /**
   * It checks whether the order contains atleast one aquatech product
   * @param orderModel order
   * @return true if the given order contains aquatech product
   */
  boolean isAquatechProductsPresentInOrder(final AbstractOrderModel orderModel);

  /**
   * It checks whether the order contains only aquatech products
   * @param orderModel order
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
   * This method used to create entry for given bundle product.
   * @param productReferenceModel reference product from bundle
   * @param orderModel order
   * @param existingEntry main bundle etry
   * @param entryNumber entryNumber
   * @return returning created entry.
   */
  AbstractOrderEntryModel createBundleOrderEntry(final ProductReferenceModel productReferenceModel,
      final OrderModel orderModel,
      final AbstractOrderEntryModel existingEntry,final AtomicInteger entryNumber);
  
  /**
   * Sets the resolved status on repair log for order code.
   *
   * @param orderCode the new resolved status on repair log
   */
  public void setResolvedStatusOnRepairLog(final String orderCode);
  /**
   * This method is used to create a separate entry for every product present in the bundle.
   * @param orderModel order
   */
  public void createAndSetBundleOrderEntriesInOrder(final OrderModel orderModel);
}
