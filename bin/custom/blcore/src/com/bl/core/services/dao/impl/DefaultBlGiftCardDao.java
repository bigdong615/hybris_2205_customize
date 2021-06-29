package com.bl.core.services.dao.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.GiftCardModel;
import com.bl.core.services.dao.BlGiftCardDao;
import de.hybris.platform.servicelayer.internal.dao.DefaultGenericDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.util.Config;
import java.util.Collections;
import java.util.Map;


/**
 * Default implementation of {@link BlGiftCardDao}.
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardDao extends DefaultGenericDao<GiftCardModel> implements BlGiftCardDao {

  private static final String CASE_SENISTIVE_CHECK = " COLLATE SQL_Latin1_General_CP1_CS_AS";

  public DefaultBlGiftCardDao()
  {
    super(GiftCardModel._TYPECODE);
  }

  /**
   * {@inheritDoc}
   */
  public GiftCardModel getGiftCard(final String giftCardCode) {
    String giftCardQuery = "SELECT {mp:pk} FROM {GiftCard AS mp} WHERE {mp:code} = ?giftCardCode";
    Map<String, String> params = Collections
        .singletonMap(BlCoreConstants.GIFT_CARD_CODE, giftCardCode);
    if (Config.isSQLServerUsed()) {
      giftCardQuery = giftCardQuery.concat(CASE_SENISTIVE_CHECK);
    }
    return getFlexibleSearchService().searchUnique(
        new FlexibleSearchQuery(giftCardQuery, params));
  }
}

