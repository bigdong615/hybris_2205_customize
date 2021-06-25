package com.bl.core.services.dao.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.services.dao.BlGiftCardDao;
import de.hybris.platform.servicelayer.internal.dao.DefaultGenericDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.util.Config;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;


/**
 * Default implementation of {@link BlGiftCardDao}.
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardDao extends DefaultGenericDao<GiftCardModel> implements BlGiftCardDao {

  private static final String CASE_SENISTIVE_CHECK = " COLLATE SQL_Latin1_General_CP1_CS_AS";
  private String giftCard = "SELECT {mp:pk} FROM {GiftCard AS mp} WHERE {mp:code} = ?giftId ";


  public DefaultBlGiftCardDao()
  {
    super(GiftCardModel._TYPECODE);
  }

  /**
   * {@inheritDoc}
   */
  public GiftCardModel getGiftCard(final String giftCardCode) {
    if(Config.isSQLServerUsed()){
      giftCard = giftCard.concat(CASE_SENISTIVE_CHECK);
    }
    final FlexibleSearchQuery query = new FlexibleSearchQuery(giftCard);
    query.addQueryParameter("giftId", giftCardCode);
    final List<GiftCardModel> result = getFlexibleSearchService().<GiftCardModel> search(query).getResult();
    return CollectionUtils.isNotEmpty(result) ? result.get(0) : null;
  }
}

