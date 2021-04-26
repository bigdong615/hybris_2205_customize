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
  
  /**
   * Resets the cart calculation flag to FALSE at the time of Rental date change.
   */
  void resetCartCalculationFlag();
  
  /**
   * Recalculate cart if required.
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
