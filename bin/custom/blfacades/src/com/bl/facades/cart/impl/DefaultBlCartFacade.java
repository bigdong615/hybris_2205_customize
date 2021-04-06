package com.bl.facades.cart.impl;

import com.bl.core.services.cart.BlCartService;
import com.bl.facades.cart.BlCartFacade;
import de.hybris.platform.commercefacades.order.impl.DefaultCartFacade;

/**
 * Default implementation of the {@link BlCartFacade}.Delivers functionality for cart.
 *
 * @author Neeraj Singh
 */
public class DefaultBlCartFacade extends DefaultCartFacade implements BlCartFacade {

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