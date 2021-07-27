package com.bl.core.inventory.scan.dao;

import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlSerialProductModel;

import de.hybris.platform.warehousing.model.PackagingInfoModel;

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
     * method will fetch InventoryLocation by its Id
     */
    BlInventoryLocationModel getInventoryLocationById(final String locationId);

    /**
     * javadoc
     * @param barcode for Serial
     * @return List of Serials
     * method will fetch SerialProducts by its barcode
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode);

    /**
     * javadoc
     * @param key for config
     * @return BlInventoryScanConfigurationModel
     * method will fetch ConfigurationKey by its key
     */
    BlInventoryScanConfigurationModel getConfigKeyFromScanConfiguration(final String key);
    
    /**
    * @param scannedItem
    * @return
    */
   PackagingInfoModel getPackageInfoByCode(String scannedItem);

}
