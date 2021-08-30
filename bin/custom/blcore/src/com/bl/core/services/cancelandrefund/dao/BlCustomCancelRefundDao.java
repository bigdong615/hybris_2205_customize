package com.bl.core.services.cancelandrefund.dao;

import de.hybris.platform.returns.model.RefundEntryModel;
import java.util.Collection;

/**
 * Custom cancel and refund dao interface
 *
 * @author Namrata Lohar
 */
public interface BlCustomCancelRefundDao {

    /**
     * This method will fetch all refund records associated to order entry
     *
     * @param abstractOrderEntryCode entry code
     * @param originalInstance versioned/extended order entry or not
     * @param orderNumber order code
     * @return list of refund entries transacted for particular order entry
     */
    Collection<RefundEntryModel> getAllRefundEntriesForOrderEntry(final String abstractOrderEntryCode, final String orderNumber,
                                                                  final boolean originalInstance);
}
