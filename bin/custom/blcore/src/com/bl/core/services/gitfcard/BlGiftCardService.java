package com.bl.core.services.gitfcard;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import com.bl.core.model.GiftCardModel;

/**
 * It provides services for gift card.
 * @author Neeraj Singh
 */
public interface BlGiftCardService {

    /**
     * It performs operation to apply gift card.
     * @param giftCardCode
     * @return
     */
    public boolean applyGiftCard(final String giftCardCode);

    /**
     * It calculates gift card balance.
     * @param giftCardModel
     * @return
     */
    public double calculateGiftCardBalance(final GiftCardModel giftCardModel);

    /**
     * It clears uncommitted gift card movements.
     * @param giftCardModel
     */
    public void clearUncommittedMovements(final GiftCardModel giftCardModel);

    /**
     * It removes applied gift card.
     * @param giftCardCode
     */
    public void removeGiftCard(final String giftCardCode, final CartModel cartModel);

    /**
     * It performs calculation for a gift card.
     * @param order
     * @param totalplustax
     */
    void calculateGiftCard(final AbstractOrderModel order, final double totalplustax);

    /**
     * It validates gift card before placing an order
     * @param order
     * @return
     */
    boolean validateGiftCardBeforePlaceOrder(final OrderModel order);

    /**
     * Commented this method, it might be useful while canceling order, If not required remove later on.
     * It will issue a new gift card in case of return
     *
     * @param item
     * @param amount
     */
    //public void giftCardRefund(ItemModel item, double amount);

    /**
     * It fetches the giftCardModel
     * @param giftCardCode
     * @return if gift card model found then returns GiftCardModel else null.
     */
    GiftCardModel getGiftCard(final String giftCardCode);
    /**
     * Generate code for gift card purchase.
     * @return String value.
     */
    String getUniqueGiftCodeGenertaor();
    
    
    /**
     * It performs operation to apply gift card.
     * @param giftCardCode current gift card code
     * @return boolean true/false
     */
    public boolean applyGiftCardForModifyOrder(final String giftCardCode, final AbstractOrderModel orderModel);
    
    
    /**
     * It removes applied gift card.
     * @param giftCardCode current gift card code
     */
    public void removeGiftCardForModifyOrder(final String giftCardCode, final AbstractOrderModel orderModel);


}
