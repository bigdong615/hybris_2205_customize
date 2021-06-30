package com.bl.facades.cart;

import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;

import de.hybris.platform.core.model.order.CartModel;
import java.util.Date;

import com.bl.facades.product.data.RentalDateDto;

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
   *
   * @param productCode the product code
   * @param quantity the quantity
   * @param serialCode the serial code
   * @return the cart modification data
   * @throws CommerceCartModificationException the commerce cart modification exception
   */
  CartModificationData addToCart(final String productCode,final long quantity,final String serialCode) throws CommerceCartModificationException;

  /**
   * It prevents the product to be added to cart if rental products is added in used gear cart and vice-versa
   *
   * @param productCode the product code
   * @param serialCode the serial code
   * @return true, if is rental product added to cart in used gear cart
   */
  boolean isRentalProductAddedToCartInUsedGearCart(final String productCode, final String serialCode);
  
  /**
	 * Check availability for rental cart entries. If available quantity is less then the entry quantity then it will set
	 * message on cart line items.
	 *
	 * @param cartData
	 *           the cart data
	 * @return true, if successful
	 */
	void checkAvailabilityForRentalCart(final CartData cartData);

	/**
	 * Update the product quantity cart entry from add to cart popup.
	 *
	 * @param entryNumber
	 *           the entry number
	 * @param quantity
	 *           the quantity
	 * @return the cart modification data
	 * @throws CommerceCartModificationException
	 *            the commerce cart modification exception
	 */
	CartModificationData updateCartEntryFromPopup(long entryNumber, long quantity) throws CommerceCartModificationException;
	
	/**
	 * Check availability in cart page on continue button to go to next checkout step
	 *
	 * @param sessionRentalDate
	 *           the session rental date
	 * @return true, if stock is available for cart
	 */
	boolean checkAvailabilityOnCartContinue(final RentalDateDto sessionRentalDate);

	/**
	 * It identifies type of cart, whether it is rental or used gear cart.
	 * @return String
	 */
	String identifyCartType();

	/**
	 * This method is used for remove discontinue product from cart.
	 */
	 void removeDiscontinueProductFromCart(final CartModel cartModel, boolean isCartPage);

	/**
	 * This method used for pre-populating card data before remove discontinue entry.
	 * @param entryNumber
	 * @param quantity
	 * @param cartModel
	 * @return
	 * @throws CommerceCartModificationException
	 */
	public CartModificationData updateCartEntry(final long entryNumber, final long quantity,
			final CartModel cartModel)
			throws CommerceCartModificationException;
}
