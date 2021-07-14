package com.bl.core.services.dao;

import com.bl.core.model.GiftCardModel;
import de.hybris.platform.servicelayer.internal.dao.GenericDao;

/**
 * Dao to fetch GiftCardModel
 * @author Neeraj Singh
 */
public interface BlGiftCardDao extends GenericDao<GiftCardModel> {

  /**
   * It checks applied gift card in DB and returns corresponding gift card details.
   * @param giftCardCode
   * @return
   */
  GiftCardModel getGiftCard(final String giftCardCode);
}
