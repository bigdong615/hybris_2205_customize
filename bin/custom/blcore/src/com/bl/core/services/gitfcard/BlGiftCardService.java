package com.bl.core.services.gitfcard;

import de.hybris.platform.core.model.order.AbstractOrderModel;
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
    public boolean applyGiftCard(String giftCardCode);

    /**
     * It calculates gift card balance.
     * @param giftCardModel
     * @return
     */
    public double calculateGiftCardBalance(GiftCardModel giftCardModel);

    /**
     * It clears uncommitted gift card movements.
     * @param giftCardModel
     */
    public void clearUncommitedMovements(GiftCardModel giftCardModel);

    /**
     * It removes applied gift card.
     * @param giftCardCode
     */
    public void removeGiftCard(String giftCardCode);

    /**
     * It performs calculation for a gift card.
     * @param order
     * @param totalplustax
     */
    void calculateGiftCard(AbstractOrderModel order, double totalplustax);

    /**
     * It validates gift card before placing an order
     * @param order
     * @return
     */
    boolean validateGiftPlaceOrder(OrderModel order);

    /**
     * Commented this method, it might be useful while canceling order, If not required remove later on.
     * It will issue a new gift card in case of return
     *
     * @param item
     * @param amount
     */
    //public void giftCardRefund(ItemModel item, double amount);

    /**
     * It calls gift card dao to check whether applied gift card code present or not.
     * @param giftCardCode
     * @return if gift card found then returns GiftCardModel else null.
     */
    GiftCardModel getGiftCard(final String giftCardCode);
}
