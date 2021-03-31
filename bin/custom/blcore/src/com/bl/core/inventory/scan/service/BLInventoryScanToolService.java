package com.bl.core.inventory.scan.service;

import com.bl.core.model.BLInventoryLocationModel;
import com.bl.core.model.BlSerialProductModel;

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
     * @param barcodes from input list
     * @return int for success/error message
     */
    int checkValidLocationInBarcodeList(List<String> barcodes);

    /**
     * @param locationId for BLInventoryLocation
     * @return BLInventoryLocation
     */
    BLInventoryLocationModel getInventoryLocationById(String locationId);

    /**
     * @param barcode for BLSerialProduct
     * @return BLSerial Product
     */
    BlSerialProductModel getSerialProductByBarcode(String barcode);

    /**
     * @param barcode for BLSerialProduct
     * @return List of BLSerial Product
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(Collection<String> barcode);

    /**
     * @param barcodes for BLSerialProduct
     * @return List of BLSerial Product
     */
    List<String> getFailedBarcodeList(List<String> barcodes);

}
