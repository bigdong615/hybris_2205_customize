package com.bl.core.services.dao;

import com.bl.core.model.GiftCardModel;

/**
 * Dao to fetch gift card details.
 * @author Neeraj Singh
 */
public interface BlGiftCardDao {

  /**
   * It checks applied gift card in DB and returns corresponding gift card details.
   * @param giftCardCode
   * @return
   */
  GiftCardModel getGiftCard(String giftCardCode);
}
