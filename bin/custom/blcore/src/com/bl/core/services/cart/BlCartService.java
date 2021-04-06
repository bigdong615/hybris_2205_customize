package com.bl.core.services.cart;

import de.hybris.platform.order.CartService;

/**
 * It provides cart related functionality.
 *
 * @author Neeraj Singh
 */
public interface BlCartService extends CartService {

  /**
   * This method will remove all the entries from current cart.
   */
  void clearCartEntries();
}
