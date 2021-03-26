package com.bl.facades.cart.impl;

import com.bl.core.services.cart.BlCartService;
import com.bl.facades.cart.BlCartFacade;
import org.apache.log4j.Logger;
import javax.annotation.Resource;

/**
 * @author Neeraj Singh
 */
public class DefaultBlCartFacade implements BlCartFacade {

    private static final Logger LOGGER = Logger.getLogger(DefaultBlCartFacade.class);

    @Resource(name = "blCartService")
    private BlCartService blCartService;

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeCartEntries() {

        getBlCartService().clearCartEntries();
    }

    public BlCartService getBlCartService() {
        return blCartService;
    }

    public void setBlCartService(BlCartService blCartService) {
        this.blCartService = blCartService;
    }
}