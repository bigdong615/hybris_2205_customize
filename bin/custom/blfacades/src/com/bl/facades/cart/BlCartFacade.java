package com.bl.facades.cart;

import de.hybris.platform.commercefacades.order.CartFacade;

/**
 * It is responsible for getting all necessary information for cart.
 *
 * @author Neeraj Singh
 */
public interface BlCartFacade extends CartFacade {

  /**
   * Remove all cart entries.
   */
  void removeCartEntries();
  
  /**
   * Resets the cart calculation flag to FALSE at the time of Rental date change.
   */
  void resetCartCalculationFlag();
  
  /**
   * Recalculate cart if required.
   * Set calculation flag of CartModel to FALSE before using this method. 
   */
  void recalculateCartIfRequired();
  
  /**
   * Update cart entry with the selected damage wavier type.
   *
   * @param entryNumber the entry number
   * @param damageWavierType the damage wavier type
   */
  void updateCartEntryDamageWavier(final long entryNumber, final String damageWavierType);
}
