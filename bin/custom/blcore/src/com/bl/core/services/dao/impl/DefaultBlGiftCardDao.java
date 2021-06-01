package com.bl.core.services.dao.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.services.dao.BlGiftCardDao;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import java.util.List;
import org.springframework.util.CollectionUtils;


/**
 * Default implementation of {@link BlGiftCardDao}.
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardDao implements BlGiftCardDao {

  private FlexibleSearchService flexibleSearchService;

  private static final String GIFTCARD = "SELECT {mp:pk} FROM {GiftCard AS mp} WHERE {mp:code} = ?giftId ";

  /**
   * {@inheritDoc}
   */
  public GiftCardModel getGiftCard(final String giftCardCode) {
    final StringBuilder stringBuilder = new StringBuilder();
    stringBuilder.append(GIFTCARD);
    final FlexibleSearchQuery query = new FlexibleSearchQuery(stringBuilder.toString());
    query.addQueryParameter("giftId", giftCardCode);
    final List<GiftCardModel> result = flexibleSearchService.<GiftCardModel>search(query)
        .getResult();
    return !CollectionUtils.isEmpty(result) ? result.get(0) : null;
  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }
}

