package com.bl.core.inventory.cycle.count.dao.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.cycle.count.dao.BlInventoryCycleCountDao;
import com.bl.core.model.BlInventoryCycleCountModel;
import com.bl.core.model.BlProductModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collection;
import java.util.List;

/**
 * ICC Default DAO
 *
 * @author Namrata Lohar
 */
public class DefaultBlInventoryCycleCountDao implements BlInventoryCycleCountDao {
    private static final Logger LOG = Logger.getLogger(DefaultBlInventoryCycleCountDao.class);

    @Autowired
    private FlexibleSearchService flexibleSearchService;

    /**
     * {@inheritDoc}
     */
    @Override
    public BlInventoryCycleCountModel getActiveInventoryCycleCount() {
        final FlexibleSearchQuery query = new FlexibleSearchQuery("SELECT {pk} FROM {BlInventoryCycleCount!} WHERE " +
                "{inventoryCycleCountActive} = 1");
        final List<BlInventoryCycleCountModel> results = getFlexibleSearchService().<BlInventoryCycleCountModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FETCH_ACTIVE_INVENTORY_CYCLE_COUNT);
        return CollectionUtils.isEmpty(results) ? null : results.get(BlInventoryScanLoggingConstants.ZERO);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlProductModel> getAllActiveSKUsWithSerialStatus() {
        final String queryString = "select {prod.pk}, {version.version}, {cat.id} from {BlProduct as prod}, {ArticleApprovalStatus as status}, " +
                "{Catalog as cat}, {CatalogVersion as version}, {ProductTypeEnum as pte} where {prod.approvalStatus} = {status.pk} and {status.code} = 'approved' " +
                "and {prod.catalogVersion} = {version.pk} and {version.catalog} = {cat.pk} and {cat.id} = 'blProductCatalog' and " +
                "{version.version} = 'Online' and {prod.productType} = {pte.pk} and {pte.code} != 'SUBPARTS'";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(queryString);
        final List<BlProductModel> results = getFlexibleSearchService().<BlProductModel>search(query).getResult();
        BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FETCH_ALL_SKUs);
        return CollectionUtils.isEmpty(results) ? CollectionUtils.EMPTY_COLLECTION : results;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<ConsignmentModel> getAllShippedConsignmentForSerial(final String serial)
    {
        BlLogger.logMessage(LOG, Level.INFO, "DefaultBlInventoryCycleCountDao : getAllShippedConsignmentForSerial");
        final String barcodeList = "select distinct({c:pk}) from {Consignment as c}, {ConsignmentEntry as ce}, {BlSerialProduct as serial}, {SerialStatusEnum as ss},"
                + "{ConsignmentStatus as cs} where {c:status} = {cs:pk} and {ce:consignment} = {c:pk} and {serial.serialStatus} = {ss.pk} and {ce:serialProducts} LIKE CONCAT('%',CONCAT({serial.pk},'%')) "
                + "and {serial.code} = ?serial and {ss.code}='SHIPPED' and {cs.code} ='BL_SHIPPED'";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("serial", serial);
        final List<ConsignmentModel> results = getFlexibleSearchService().<ConsignmentModel> search(query).getResult();
        return CollectionUtils.isNotEmpty(results) ? results : null;
    }

    public FlexibleSearchService getFlexibleSearchService() {
        return flexibleSearchService;
    }

    public void setFlexibleSearchService(FlexibleSearchService flexibleSearchService) {
        this.flexibleSearchService = flexibleSearchService;
    }
}
