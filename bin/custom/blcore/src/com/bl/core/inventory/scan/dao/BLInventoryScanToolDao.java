package com.bl.core.inventory.scan.dao;

import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlSerialProductModel;

import java.util.Collection;

/**
 * {javadoc}
 * @author Namrata Lohar
 */
public interface BlInventoryScanToolDao {

    /**
     * @param locationId for InventoryLocation
     * @return InventoryLocation
     */
    BlInventoryLocationModel getInventoryLocationById(final String locationId);

    /**
     * @param barcode for Serial
     * @return List of Serials
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode);

}
