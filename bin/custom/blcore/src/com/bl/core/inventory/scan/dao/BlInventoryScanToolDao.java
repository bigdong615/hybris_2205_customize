package com.bl.core.inventory.scan.dao;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.Collection;

import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlSerialProductModel;

/**
 * 
 * @author Namrata Lohar
 */
public interface BlInventoryScanToolDao {

    /**
     * method will fetch InventoryLocation by its Id
     * 
     * @param locationId for InventoryLocation
     * @return InventoryLocation
     * 
     */
    BlInventoryLocationModel getInventoryLocationById(final String locationId);

    /**
     * method will fetch SerialProducts by its barcode
     * 
     * @param barcode for Serial
     * @return List of Serials
     * 
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode);

    /**
     * method will fetch ConfigurationKey by its key
     * 
     * @param key for config
     * @return BlInventoryScanConfigurationModel
     * 
     */
    BlInventoryScanConfigurationModel getConfigKeyFromScanConfiguration(final String key);
    
    /**
     * This method will get the packageInfo by tracking id
     * @param scannedItem
     * @return PackagingInfoModel
     * method will fetch PackagInfo by tracking id
     */
    PackagingInfoModel getPackageInfoByCode(final String scannedItem);

 	/**
	  * Gets the list of packages for serials.
	  *
	  * @param barcodes the barcodes
	  * @return the package for serials
	  */
	 Collection<PackagingInfoModel> getPackageForSerials(final Collection<String> barcodes);

 	/**
 	 * This method will return all orders that needs to be shipped out today and will return today to mark Serials with
 	 * DirtyPriorityStatus
 	 *
 	 * @return Abstract Order list
 	 */
 	Collection<ConsignmentModel> getTodaysShippingOrders();

 	/**
 	 * This method will give list orders that has particular serial associated
 	 *
 	 * @param serial
 	 *           product
 	 * @return list Consignments
 	 */
 	Collection<ConsignmentModel> getAllConsignmentForSerial(final String serial);
 	
 	/**
	  * Gets the all consignment out today.
	  *
	  * @param serial the serial
	  * @return the all consignment out today
	  */
	 Collection<ConsignmentModel> getTodaysShippingConsignments(final String serial);

	/**
	 * Get All Serials by Bin Location
	 * @param binLocationId
	 * @return
	 */
	 Collection<BlSerialProductModel> getAllSerialsByBinLocation(final String binLocationId);


}
