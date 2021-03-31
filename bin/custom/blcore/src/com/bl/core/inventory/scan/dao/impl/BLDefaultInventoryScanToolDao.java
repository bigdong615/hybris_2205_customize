package com.bl.core.inventory.scan.dao.impl;

import com.bl.core.inventory.scan.dao.BLInventoryScanToolDao;
import com.bl.core.model.BLInventoryLocationModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

/**
 * {javadoc}
 * @author Namrata Lohar
 */
public class BLDefaultInventoryScanToolDao implements BLInventoryScanToolDao {

    @Autowired
    private FlexibleSearchService flexibleSearchService;

    /**
     * {@inheritDoc}
     * @return
     */
    @Override
    public BLInventoryLocationModel getInventoryLocationById(String locationId) {
        Map<String, Object> params = new TreeMap<>();
        params.put("locationId", locationId);

        FlexibleSearchQuery query = new FlexibleSearchQuery("SELECT {pk} FROM {BlInventoryLocation!} WHERE {code} = ?locationId", params);
        final List<BLInventoryLocationModel> results = flexibleSearchService.<BLInventoryLocationModel> search(query).getResult();

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
    public BlSerialProductModel getSerialProductByBarcode(String barcode) {
        Map<String, Object> params = new TreeMap<>();
        params.put("barcode", barcode);

        FlexibleSearchQuery query = new FlexibleSearchQuery("SELECT {pk} FROM {BlSerialProduct!} WHERE {barcode} = ?barcode", params);
        final List<BlSerialProductModel> results = flexibleSearchService.<BlSerialProductModel> search(query).getResult();

        if (CollectionUtils.isNotEmpty(results))
        {
            return results.get(0);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     * @return
     */
    @Override
    public Collection<BlSerialProductModel> getSerialProductsByBarcode(Collection<String> barcodes) {
        String barcodeList = "SELECT {bsp.pk} FROM {BlSerialProduct! as bsp}, {CatalogVersion as cv}, {Catalog as c}, " +
                "{ArticleApprovalStatus as aas} WHERE {cv.catalog} = {c.pk} and {bsp.catalogVersion} = {cv.pk} and {c.id} = 'blProductCatalog' and {cv.version} = 'Online'" +
                "and {aas.code} = 'approved' and {barcode} in (?barcodeList)";

        FlexibleSearchQuery query = new FlexibleSearchQuery(barcodeList);
        query.addQueryParameter("barcodeList", barcodes);

        final List<BlSerialProductModel> results = flexibleSearchService.<BlSerialProductModel> search(query).getResult();

        if (CollectionUtils.isNotEmpty(results))
        {
            return results;
        }
        return Collections.emptyList();
    }
}
