package com.bl.core.inventory.scan.service;

import com.bl.core.jalo.BLInventoryLocation;
import com.bl.core.jalo.BlSerialProduct;
import com.bl.facades.inventoryScan.model.BlInventoryScanResultData;

import java.util.Collection;
import java.util.List;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 */
public interface BLInventoryScanToolService {

    /**
     * @param username for Employee
     * @param password for Employee
     * @return boolean
     */
    boolean validateEmployeeByUsernameAndPassword(String username, String password);

    /**
     * @param locationId for BLInventoryLocation
     * @return BLInventoryLocation
     */
    BLInventoryLocation getInventoryLocationById(String locationId);

    /**
     * @param barcode for BLSerialProduct
     * @return BLSerial Product
     */
    BlSerialProduct getSerialProductByBarcode(String barcode);

    /**
     * @param barcode for BLSerialProduct
     * @return List of BLSerial Product
     */
    Collection<BlSerialProduct> getSerialProductsByBarcode(Collection<String> barcode);

    /**
     * @param serialId for BLSerialProduct
     * @return BLSerial Product
     */
    BlSerialProduct getSerialProductBySerialId(String serialId);

    /**
     * @param barcodes from frontend
     * @return Scan Response Data
     */
    BlInventoryScanResultData webScanToolUpdateInventoryLocation(List<String> barcodes);

}
