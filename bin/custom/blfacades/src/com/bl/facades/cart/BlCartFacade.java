package com.bl.facades.cart;

import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.core.model.order.CartModel;

/**
 * It is responsible for getting all necessary information for cart.
 * @author Neeraj Singh
 */
public interface BlCartFacade extends CartFacade {

    /**
     * Remove cart entries.
     * @param cartModel
     */
    void removeCartEntries(final CartModel cartModel);
}
