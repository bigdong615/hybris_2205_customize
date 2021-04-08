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
     * javadoc
     * @param barcodes from input list
     * @return int
     */
    int checkValidLocationInBarcodeList(final List<String> barcodes);

    /**
     * javadoc
     * @param locationId for BlInventoryLocation
     * @return BlInventoryLocation
     */
    BlInventoryLocationModel getInventoryLocationById(final String locationId);

    /**
     * javadoc
     * @param barcode for BlSerialProduct
     * @return Collection<BlSerialProductModel>
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode);

    /**
     * javadoc
     * @param barcodes for BlSerialProduct
     * @return List<String>
     */
    List<String> getFailedBarcodeList(final List<String> barcodes);

    /**
     * javadoc
     * @param key for config
     * @return String
     */
    String getConfigKeyFromScanConfiguration(final String key);
}
