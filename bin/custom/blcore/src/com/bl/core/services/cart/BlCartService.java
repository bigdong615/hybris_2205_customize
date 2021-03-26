package com.bl.core.services.cart;

import de.hybris.platform.core.model.order.CartModel;

/**
 * @author Neeraj Singh
 */
public interface BlCartService {

    /**
     * THis method will remove all the entries from current cart.
     */
    void clearCartEntries();
}
