package com.bl.core.inventory.scan.dao;

import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlSerialProductModel;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
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
     * @param key for config
     * @return BlInventoryScanConfigurationModel
     * method will fetch ConfigurationKey by its key
     */
    BlInventoryScanConfigurationModel getConfigKeyFromScanConfiguration(final String key);

    /**
 	 * @param barcodes
 	 *           list of scanned barcode serials
 	 * @return PackagingInfoModel
 	 */
 	Collection<PackagingInfoModel> getPackageForSerials(final Collection<String> barcodes);

 	/**
 	 * This method will return all orders that needs to be shipped out today and will return today to mark Serials with
 	 * DirtyPriorityStatus
 	 *
 	 * @return Abstract Order list
 	 */
 	Collection<AbstractOrderModel> getAllOutTodayOrders();

 	/**
 	 * This method will give list orders that has particular serial associated
 	 *
 	 * @param serial
 	 *           product
 	 * @return list Consignments
 	 */
 	Collection<ConsignmentModel> getAllConsignmentForSerial(final String serial);
}
