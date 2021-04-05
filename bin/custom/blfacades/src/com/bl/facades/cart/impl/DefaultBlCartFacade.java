package com.bl.facades.cart.impl;

import com.bl.core.services.cart.BlCartService;
import com.bl.facades.cart.BlCartFacade;
import de.hybris.platform.commercefacades.order.impl.DefaultCartFacade;
import de.hybris.platform.core.model.order.CartModel;

/**
 * Default implementation of the {@link BlCartFacade}.Delivers functionality for cart.
 * @author Neeraj Singh
 */
public class DefaultBlCartFacade extends DefaultCartFacade implements BlCartFacade {

    private BlCartService blCartService;

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeCartEntries(final CartModel cartModel) {

        getBlCartService().clearCartEntries(cartModel);
    }

    public BlCartService getBlCartService() {
        return blCartService;
    }

    public void setBlCartService(BlCartService blCartService) {
        this.blCartService = blCartService;
    }
}