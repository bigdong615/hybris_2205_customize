package com.bl.core.services.cart;

import de.hybris.platform.order.CartService;

import java.util.Date;

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
   * Update cart entry with the selected damage Waiver type.
   *
   * @param entryNumber the entry number
   * @param damageWaiverType the damage Waiver type
   */
  void updateCartEntryDamageWaiver(final long entryNumber, final String damageWaiverType);
  
  /**
   * Sets the rental dates on cart.
   *
   * @param rentalStartDate the rental start date
   * @param rentalEndDate the rental end date
   */
  void setRentalDatesOnCart(final Date rentalStartDate, final Date rentalEndDate);
}
