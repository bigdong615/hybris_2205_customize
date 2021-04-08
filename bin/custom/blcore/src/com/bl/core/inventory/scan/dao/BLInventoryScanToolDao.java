package com.bl.core.inventory.scan.dao;

import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlSerialProductModel;

import java.util.Collection;

/**
 * {javadoc}
 * @author Namrata Lohar
 */
public interface BlInventoryScanToolDao {

    /**
     * javadoc
     * @param locationId for InventoryLocation
     * @return InventoryLocation
     */
    BlInventoryLocationModel getInventoryLocationById(final String locationId);

    /**
     * javadoc
     * @param barcode for Serial
     * @return List of Serials
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode);

    /**
     * javadoc
     * @param key for config
     * @return BlInventoryScanConfigurationModel
     */
    BlInventoryScanConfigurationModel getConfigKeyFromScanConfiguration(final String key);

}
