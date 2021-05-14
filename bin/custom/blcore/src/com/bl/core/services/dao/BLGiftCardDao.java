package com.bl.core.services.dao;

import de.hybris.platform.core.model.order.CartModel;

import java.util.List;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;

public interface BLGiftCardDao {

    GiftCardModel getGiftCard(String giftId);

    CartModel getCart(String cartId);

    List<GiftCardMovementModel> getCardMovement(String giftId);
}
