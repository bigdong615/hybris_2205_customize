package com.bl.facades.cart;

import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;

import java.util.Date;

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

  /**
   * This method is used to add product to cart.
   * @param productCode
   * @param quantity
   * @param serialCode
   * @return CartModificationData
   * @throws CommerceCartModificationException
   */
  CartModificationData addToCart(final String productCode,final long quantity,final String serialCode) throws CommerceCartModificationException;

  /**
   * Prevent add to cart when user try to add rental product in used gear cart and vice-versa.
   * @param productCode
   * @param serialCode
   * @return
   */
  boolean isRentalAndUsedProduct(final String productCode, final String serialCode);
}
