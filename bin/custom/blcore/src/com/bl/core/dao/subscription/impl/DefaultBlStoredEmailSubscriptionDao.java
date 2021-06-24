package com.bl.core.dao.subscription.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.subscription.BlStoredEmailSubscriptionDao;
import com.bl.core.model.BlStoredEmailSubscriptionModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;
import org.apache.commons.collections.CollectionUtils;

/**
 * This interface is
 * created for getting State BlStoredEmailSubscription object.
 *
 * @author Sunil
 */
public class DefaultBlStoredEmailSubscriptionDao implements BlStoredEmailSubscriptionDao {

  private FlexibleSearchService flexibleSearchService;
  private static final String FIND_BL_STORED_EMAIL_SUBSCRIPTION_BY_CONTACT_KEY =
      "SELECT {pk} FROM {" + BlStoredEmailSubscriptionModel._TYPECODE + "} WHERE {"
          + BlStoredEmailSubscriptionModel.CONTACTKEY + "} =  ?contactKey ";


  /**
   * {@inheritDoc}
   */
  @Override
  public BlStoredEmailSubscriptionModel getStoredEmailSubscriptionForContactKey(
      final String contactKey) {

    final FlexibleSearchQuery query = new FlexibleSearchQuery(
        FIND_BL_STORED_EMAIL_SUBSCRIPTION_BY_CONTACT_KEY);
    query.addQueryParameter(BlCoreConstants.SUBSCRIPTION_CONTACT_KEY, contactKey);
    final SearchResult<BlStoredEmailSubscriptionModel> result = getFlexibleSearchService()
        .search(query);

    if (null != result && CollectionUtils.isNotEmpty(result.getResult())) {
      return result.getResult().get(0);
    }

    return null;
  }

  public FlexibleSearchService getFlexibleSearchService() {
    return flexibleSearchService;
  }

  public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService) {
    this.flexibleSearchService = flexibleSearchService;
  }

}
