package com.bl.core.inventory.scan.dao.impl;

import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 */
public class DefaultBlInventoryScanToolDao implements BlInventoryScanToolDao {

    @Autowired
    private FlexibleSearchService flexibleSearchService;

    /**
     * {@inheritDoc}
     */
    @Override
    public BlInventoryLocationModel getInventoryLocationById(final String locationId) {
        final String barcodeList = "SELECT {pk} FROM {BlInventoryLocation!} WHERE {code} = ?locationId";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("locationId", locationId);
        final List<BlInventoryLocationModel> results = getFlexibleSearchService().<BlInventoryLocationModel>search(query).getResult();
        return CollectionUtils.isNotEmpty(results) ? results.get(0) : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcodes) {
        final String barcodeList = "SELECT {bsp.pk} FROM {BlSerialProduct! as bsp}, {CatalogVersion as cv}, {Catalog as c}, " +
                "{ArticleApprovalStatus as aas} WHERE {cv.catalog} = {c.pk} and {bsp.catalogVersion} = {cv.pk} and {c.id} = 'blProductCatalog' and {cv.version} = 'Online'" +
                "and {aas.code} = 'approved' and {barcode} in (?barcodeList)";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("barcodeList", barcodes);
        final List<BlSerialProductModel> results = getFlexibleSearchService().<BlSerialProductModel>search(query).getResult();
        return CollectionUtils.isNotEmpty(results) ? results : Collections.emptyList();
    }

    @Override
    public BlInventoryScanConfigurationModel getConfigKeyFromScanConfiguration(final String key) {
        final String barcodeList = "SELECT {pk} FROM {BlInventoryScanConfiguration!} WHERE {blScanConfigKey} = ?key";
        final FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("key", key);
        final List<BlInventoryScanConfigurationModel> results = getFlexibleSearchService().<BlInventoryScanConfigurationModel>search(query).getResult();
        return CollectionUtils.isNotEmpty(results) ? results.get(0) : null;
    }

    public FlexibleSearchService getFlexibleSearchService() {
        return flexibleSearchService;
    }

    public void setFlexibleSearchService(final FlexibleSearchService flexibleSearchService) {
        this.flexibleSearchService = flexibleSearchService;
    }
}
