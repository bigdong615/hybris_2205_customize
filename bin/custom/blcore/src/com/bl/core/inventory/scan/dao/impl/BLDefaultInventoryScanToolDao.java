package com.bl.core.inventory.scan.dao.impl;

import com.bl.core.inventory.scan.dao.BLInventoryScanToolDao;
import com.bl.core.jalo.BLInventoryLocation;
import com.bl.core.jalo.BlSerialProduct;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import org.apache.commons.collections.CollectionUtils;

import java.util.*;

/**
 * {javadoc}
 * @author Namrata Lohar
 */
public class BLDefaultInventoryScanToolDao implements BLInventoryScanToolDao {

    private FlexibleSearchService flexibleSearchService;

    /**
     * {@inheritDoc}
     */
    @Override
    public BLInventoryLocation getInventoryLocationById(String locationId) {
        Map<String, Object> params = new TreeMap<>();
        params.put("locationId", locationId);

        FlexibleSearchQuery query = new FlexibleSearchQuery("SELECT {pk} FROM {BlInventoryLocation!} WHERE {inventoryLocationID} = ?locationId", params);
        final List<BLInventoryLocation> results = flexibleSearchService.<BLInventoryLocation> search(query).getResult();

        if (CollectionUtils.isNotEmpty(results))
        {
            return results.get(0);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BlSerialProduct getSerialProductByBarcode(String barcode) {
        Map<String, Object> params = new TreeMap<>();
        params.put("barcode", barcode);

        FlexibleSearchQuery query = new FlexibleSearchQuery("SELECT {pk} FROM {BlSerialProduct!} WHERE {barcode} = ?barcode", params);
        final List<BlSerialProduct> results = flexibleSearchService.<BlSerialProduct> search(query).getResult();

        if (CollectionUtils.isNotEmpty(results))
        {
            return results.get(0);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BlSerialProduct getSerialProductBySerialId(String serialId) {
        Map<String, Object> params = new TreeMap<>();
        params.put("serialId", serialId);

        FlexibleSearchQuery query = new FlexibleSearchQuery("SELECT {pk} FROM {BlSerialProduct!} WHERE {serialId} = ?barcode", params);
        final List<BlSerialProduct> results = flexibleSearchService.<BlSerialProduct> search(query).getResult();

        if (CollectionUtils.isNotEmpty(results))
        {
            return results.get(0);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlSerialProduct> getSerialProductsByBarcode(Collection<String> barcodes) {
        FlexibleSearchQuery query = new FlexibleSearchQuery("SELECT {pk} FROM {BlSerialProduct!} WHERE {serialId} in (?" + barcodes + ")");
        final List<BlSerialProduct> results = flexibleSearchService.<BlSerialProduct> search(query).getResult();

        if (CollectionUtils.isNotEmpty(results))
        {
            return results;
        }
        return Collections.emptyList();
    }
}
