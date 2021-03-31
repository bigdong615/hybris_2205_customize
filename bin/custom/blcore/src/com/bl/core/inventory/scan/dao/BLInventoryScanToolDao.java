package com.bl.core.inventory.scan.dao;

import com.bl.core.model.BLInventoryLocationModel;
import com.bl.core.model.BlSerialProductModel;

import java.util.Collection;

/**
 * {javadoc}
 * @author Namrata Lohar
 */
public interface BLInventoryScanToolDao {

    /**
     * @param locationId for InventoryLocation
     * @return InventoryLocation
     */
    BLInventoryLocationModel getInventoryLocationById(String locationId);

    /**
     * @param barcode for Serial
     * @return Serial Product
     */
    BlSerialProductModel getSerialProductByBarcode(String barcode);

    /**
     * @param barcode for Serial
     * @return List of Serials
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(Collection<String> barcode);

}
