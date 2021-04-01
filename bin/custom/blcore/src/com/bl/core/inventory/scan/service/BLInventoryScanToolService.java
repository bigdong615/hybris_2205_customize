package com.bl.core.inventory.scan.service;

import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlSerialProductModel;

import java.util.Collection;
import java.util.List;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 */
public interface BlInventoryScanToolService {

    /**
     * @param barcodes from input list
     * @return int for success/error message
     */
    int checkValidLocationInBarcodeList(final List<String> barcodes);

    /**
     * @param locationId for BlInventoryLocation
     * @return BlInventoryLocation
     */
    BlInventoryLocationModel getInventoryLocationById(final String locationId);

    /**
     * @param barcode for BlSerialProduct
     * @return List of BLSerial Product
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode);

    /**
     * @param barcodes for BlSerialProduct
     * @return List of BlSerial Product
     */
    List<String> getFailedBarcodeList(final List<String> barcodes);
}
