package com.bl.core.services.cart;

import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import java.util.Date;
import java.util.List;
import java.util.Map;

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
  
  /**
	 * Gets the list of available quantity for rental cart entries.
	 *
	 * @param cartData
	 *           the cart data
	 * @param warehouses
	 *           the warehouses
	 * @param rentalDatesFromSession
	 *           the rental dates from session
	 * @return the map
	 */
	Map<String, Long> getAvailabilityForRentalCart(final CartData cartData, final List<WarehouseModel> warehouses,
			final RentalDateDto rentalDatesFromSession);

	/**
	 * Is Free Shipping Promo Applied on the cart
	 * @return
	 */
	public boolean isFreeShippingPromoApplied(final AbstractOrderModel abstractOrderModel);
}
