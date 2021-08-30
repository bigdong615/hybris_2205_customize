package com.bl.core.services.cancelandrefund.dao.impl;

import com.bl.constants.BlCancelRefundLoggingConstants;
import com.bl.core.services.cancelandrefund.dao.BlCustomCancelRefundDao;
import com.bl.logging.BlLogger;
import de.hybris.platform.returns.model.RefundEntryModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Custom cancel and refund dao implementer
 *
 * @author Namrata Lohar
 */
public class DefaultBlCustomCancelRefundDao implements BlCustomCancelRefundDao {

    private static final Logger LOG = Logger.getLogger(DefaultBlCustomCancelRefundDao.class);

    private FlexibleSearchService flexibleSearchService;

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<RefundEntryModel> getAllRefundEntriesForOrderEntry(final String abstractOrderEntryCode, final String orderNumber,
                                                                         final boolean originalInstance) {
        final StringBuilder barcodeList = new StringBuilder("select {refund.pk} from {RefundEntry as refund}, {AbstractOrderEntry as entry}, " +
                "{Order as o} where {refund.orderEntry} = {entry.pk} and {entry.order} = {o.pk} and {o.code} = ?orderNumber and " +
                "{entry.entryNumber} = ?abstractOrderEntryCode");
        final FlexibleSearchQuery query = new FlexibleSearchQuery(String.valueOf(barcodeList));
        query.addQueryParameter("orderNumber", orderNumber);
        query.addQueryParameter("abstractOrderEntryCode", abstractOrderEntryCode);
        if(originalInstance) {
            barcodeList.append(" and {o.versionId} is null");
        } else {
            barcodeList.append(" and {o.versionId} is not null");
        }
        final List<RefundEntryModel> results = getFlexibleSearchService().<RefundEntryModel> search(query).getResult();
        BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlCancelRefundLoggingConstants.ENTRY_LEVEL_REFUND_ENTRIES_FETCH +
                abstractOrderEntryCode + BlCancelRefundLoggingConstants.ORDER_NUMBER + orderNumber);
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    public FlexibleSearchService getFlexibleSearchService() {
        return flexibleSearchService;
    }

    public void setFlexibleSearchService(FlexibleSearchService flexibleSearchService) {
        this.flexibleSearchService = flexibleSearchService;
    }
}
