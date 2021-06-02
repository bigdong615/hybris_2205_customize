package com.bl.core.services.dao.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.services.dao.BlGiftCardDao;
import de.hybris.platform.servicelayer.internal.dao.DefaultGenericDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;


/**
 * Default implementation of {@link BlGiftCardDao}.
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardDao extends DefaultGenericDao<GiftCardModel> implements BlGiftCardDao {

  private static final String GIFTCARD = "SELECT {mp:pk} FROM {GiftCard AS mp} WHERE {mp:code} = ?giftId ";

  public DefaultBlGiftCardDao()
  {
    super(GiftCardModel._TYPECODE);
  }

  /**
   * {@inheritDoc}
   */
  public GiftCardModel getGiftCard(final String giftCardCode) {
    /*final StringBuilder stringBuilder = new StringBuilder();
    stringBuilder.append(GIFTCARD);
    final FlexibleSearchQuery query = new FlexibleSearchQuery(stringBuilder.toString());
    query.addQueryParameter("giftId", giftCardCode);
    final List<GiftCardModel> result = flexibleSearchService.<GiftCardModel>search(query).getResult();*/

    final FlexibleSearchQuery query = new FlexibleSearchQuery(GIFTCARD);
    query.addQueryParameter("giftId", giftCardCode);
    final List<GiftCardModel> result = getFlexibleSearchService().<GiftCardModel> search(query).getResult();
    return CollectionUtils.isNotEmpty(result) ? result.get(0) : null;
  }
}

