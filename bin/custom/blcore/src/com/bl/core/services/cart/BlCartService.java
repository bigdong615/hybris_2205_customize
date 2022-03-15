package com.bl.core.services.cart;

import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
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
     * @param cartModel the cart model
     */
    void clearCartEntries(final CartModel cartModel);

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
     * @param entryNumber      the entry number
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
     * @param rentalEndDate   the rental end date
     */
    void setRentalDatesOnCart(final Date rentalStartDate, final Date rentalEndDate);

    /**
     * Gets the list of available quantity for rental cart entries.
     *
     * @param cartData               the cart data
     * @param warehouses             the warehouses
     * @param rentalDatesFromSession the rental dates from session
     * @return the map
     */
    Map<String, Long> getAvailabilityForRentalCart(final CartData cartData, final List<WarehouseModel> warehouses,
                                                   final RentalDateDto rentalDatesFromSession);


    /**
     * Change usedGear serial product status when cart session Timeout
     *
     * @param cartModel
     * @param cartEntry
     */
    void setUsedGearSerialProductStatus(final CartModel cartModel, final AbstractOrderEntryModel cartEntry);
    /**
     * Change gift card purchase status when remove from cart
     *
     * @param cartModel
     */
    void updateGiftCardPurchaseStatus(final CartModel cartModel);


    /**
     * This method saves PO payment details.
     * @param poNumber
     * @param poNotes
     */
    void savePoPaymentDetails(final String poNumber, final String poNotes);

    /**
     * Update promotional End date for the promotion with extended
     * rental days
     * @param updatedRentalToDate
     */
    void updatePromotionalEndDate(final Date updatedRentalToDate);

    /**
     * Check if the Promotion with extended days is applied to cart
     * @return
     */
    boolean isFreeRentalDayPromoApplied();

    /**
     * This method saves PO payment details.
     * @param poNumber
     * @param poNotes
    * @return boolean
     */
    boolean savePoPaymentDetailsForPayBill(final String poNumber, final String poNotes, final OrderModel orderModel);

    /**
     * This method will update the order types like VIP order, FD/SHIPPING order
     */
    void updateOrderTypes();

    /**
     * Change new gear purchase status when remove from cart.
     * @param cartModel
     */
     void updateNewGearPurchaseStatus(final CartModel cartModel);

    /**
     * This method used for remove cart from DB.
     * @param cartModel
     */
    public void removeEmptyCart(final CartModel cartModel);
    
    /**
     * Checks if is selected date is blackout date.
     *
     * @param dateToCheck the date to check
     * @param blackoutDateType the blackout date type
     * @return true, if is selected date is blackout date
     */
    boolean isSelectedDateIsBlackoutDate(final Date dateToCheck, final BlackoutDateTypeEnum blackoutDateType);

    /**
     * Checks if the given product is aquatech in session cart
     *
     * @param productModel the date to check
     * @return true, if the given product is aquatech
     */
    boolean isAquatechProductsPresentInCart(final ProductModel productModel);

    /**
     * Checks if aquatech product present in session cart
     *
     * @return true, if the session cart have aquatech products
     */
    boolean isAquatechProductsPresentInCart();
    
    /**
     * Checks if is rental cart only.
     *
     * @return true, if is rental cart only
     */
    boolean isRentalCartOnly();
    
    /**
     * Checks if is used order only.
     *
     * @param order the order
     * @return true, if is used order only
     */
    boolean isUsedOrderOnly(final AbstractOrderModel order);
}
