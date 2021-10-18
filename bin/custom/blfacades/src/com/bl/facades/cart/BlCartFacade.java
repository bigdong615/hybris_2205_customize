package com.bl.facades.cart;

import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.CartModificationData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.CartModel;
import java.util.Date;

import com.bl.facades.product.data.RentalDateDto;
import com.bl.storefront.forms.GiftCardPurchaseForm;

import java.util.List;

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
	 * Update cart entry with the selected option
	 *
	 * @param entryNumber the entry number
	 * @param optionCode the optionCode
	 */
	void updateCartEntrySelectedOption(final long entryNumber, final String optionCode);


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
   * This method is used to add product to cart.
   *
   * @param productCode the product code
   * @param quantity the quantity
   * @param serialCode the serial code
   * @return the cart modification data
   * @throws CommerceCartModificationException the commerce cart modification exception
   */
  CartModificationData addToCart(final String productCode,final long quantity,final String serialCode,final GiftCardPurchaseForm giftCardForm) throws CommerceCartModificationException;

  /**
   * It prevents the product to be added to cart if rental products is added in used gear cart and vice-versa
   *
   * @param productCode the product code
   * @param serialCode the serial code
   * @return true, if is rental product added to cart in used gear cart
   */
  boolean isRentalProductAddedToCartInUsedGearCart(final String productCode, final String serialCode);
  /**
   * This method is used to check if cart has rental products or used gear products.
   * 
   * @return true, if cart has rental products or used gear products.
   */
  boolean cartHasRentalOrUsedGearProducts();
  
  /**
   * This method is used to check if cart has gift card or not.
   * 
   * @return true, if cart has gift card.
   */
  boolean cartHasGiftCard(final String productCode);
   
  /**
   * This method is used to check gift card product or not.
   * 
   * @return true, if cart has gift card.
   */
  boolean isGiftCardProduct(final String code);
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
	String removeDiscontinueProductFromCart(final CartModel cartModel,final boolean isCartPage); // NOSONAR

	/**
	 * This method used for pre-populating saved card data before removing its discontinue entry.
	 * @param entryNumber
	 * @param quantity
	 * @param cartModel
	 * @return
	 * @throws CommerceCartModificationException
	 */
	public CartModificationData updateCartEntry(final long entryNumber, final long quantity, final CartModel cartModel) throws CommerceCartModificationException; // NOSONAR

	/**
	 * This method used for collecting discontinue entries number form cart.
	 * @return List<Integer>
	 * @param cartModel
	 * @param removedEntry
	 */
	public List<Integer> getDiscontinueEntryList(final CartModel cartModel, StringBuilder removedEntry); // NOSONAR

	/**
	 * This method saves PO payment details.
	 * @param poNumber
	 * @param poNotes
	 */
	void savePoPaymentDetails(final String poNumber, final String poNotes);

	/**
	 * If credit card or paypal payment method selected then remove po number from cartModel.
	 */
	void removePoNumber();

	/**
	 * To check new gear product is allowed to add to cart.
	 * @param productCode
	 * @param serialCode
	 * @return
	 */
	public boolean isNewGearProductAllowToAdd(final String productCode, final String serialCode);

	/**
	 * Check rental dates should be 2 business days minimum for shipping for aquatech products
	 *
	 * @param cartData
	 *           the cart data
	 */
	void checkAquatechRentalDates(final CartData cartData);

	/**
	 * Remove restricted entries from cart
	 * @param restrictedEntries
	 * @return
	 */
  String removeRestrictedEntries(final List<AbstractOrderEntryModel> restrictedEntries, final CartModel cartModel, final boolean isCartPage);
}
