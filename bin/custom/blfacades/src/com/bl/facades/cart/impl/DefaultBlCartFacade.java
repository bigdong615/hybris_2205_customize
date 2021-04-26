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
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void resetCartCalculationFlag()
  {
	  getBlCartService().resetCartCalculationFlag();
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public void recalculateCartIfRequired()
  {
	  getBlCartService().recalculateCartIfRequired();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updateCartEntryDamageWavier(final long entryNumber, final String damageWavierType)
  {
	  getBlCartService().updateCartEntryDamageWavier(entryNumber, damageWavierType);
  }

  /**
   * Gets the bl cart service.
   *
   * @return the bl cart service
   */
  public BlCartService getBlCartService() 
  {
    return blCartService;
  }

  /**
   * Sets the bl cart service.
   *
   * @param blCartService the new bl cart service
   */
  public void setBlCartService(final BlCartService blCartService) 
  {
    this.blCartService = blCartService;
  }

}