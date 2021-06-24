package com.bl.core.dao.subscription;

import com.bl.core.model.BlStoredEmailSubscriptionModel;

/**
 * This interface is
 * created for getting State BlStoredEmailSubscription object.
 *
 * @author Sunil
 */
public interface BlStoredEmailSubscriptionDao {

  /**
   * Get BlStoredEmailSubscription for given contact key.
   *
   * @param contactKey - contact key.
   * @return BlStoredEmailSubscription object.
   */
  BlStoredEmailSubscriptionModel getStoredEmailSubscriptionForContactKey(final String contactKey);
}
