package com.bl.core.inventory.scan.service;

import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlSerialProductModel;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 */
public interface BlInventoryScanToolService {

    /**
     * method will check the valid Location provided in barcode list and return int with appropriate notification
     * number to notify employee
     *
     * @param barcodes the barcodes
     * @param memberAllowedLocationList the member allowed location list
     * @return the int
     */
    int checkValidLocationInBarcodeList(final List<String> barcodes, final List<String> memberAllowedLocationList);

    /**
     * javadoc
     * @param locationId for BlInventoryLocation
     * @return BlInventoryLocation
     * method will fetch InventoryLocation by its Id from dao
     */
    BlInventoryLocationModel getInventoryLocationById(final String locationId);

    /**
     * javadoc
     * @param barcode for BlSerialProduct
     * @return Collection<BlSerialProductModel>
     * method will fetch SerialProducts by its barcode from dao
     */
    Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode);

    /**
     * javadoc
     * @param barcodes for BlSerialProduct
     * @return List<String>
     * method will verify the list of barcodes and result into list of failed barcodes that has been failed to update
     * its location in db
     */
    List<String> getFailedBarcodeList(final List<String> barcodes);

    /**
     * javadoc
     * @param key for config
     * @return String
     * method will fetch ConfigurationKey by its key from dao
     */
    String getConfigKeyFromScanConfiguration(final String key);
    
    /**
     * Checks if is valid tech eng location barcode.
     *
     * @param barcodes the barcodes
     * @param memberAllowedLocationList the member allowed location list
     * @return the int
     */
    int isValidTechEngLocationBarcode(final List<String> barcodes, final List<String> memberAllowedLocationList);
    
    /**
     * Do tech eng serial location update.
     *
     * @param barcodes the barcodes
     * @return the map
     */
    Map<String,List<String>> doTechEngSerialLocationUpdate(final List<String> barcodes);
    
 	/**
 	 * method will verify the list of bin barcodes and result into list of failed barcodes that has
 	 *         been failed to update its location in db
 	 * @param barcodes
 	 *           for BlSerialProduct
 	 * @return List<String> 
 	 */
 	public List<String> getFailedBinBarcodeList(final List<String> barcodes);

 	/**
 	 * method will verify the list of barcodes and result into list of failed barcodes that are not
 	 *         valid as per the order
 	 * @param selectedConsignment
 	 * @return List<String> 
 	 */
 	public List<String> verifyShippingScan(final List<String> barcodes, final ConsignmentModel selectedConsignment);
 	
	/**
	 * This method will check valid tracking Id scanned or not and return int with appropriate notification
	 * @param barcodes
	 * @return the int
	 */
	public int checkValidTrackingId(final String barcodes);
	
	/**
	 * method will verify the list of bin barcodes and result into list of failed barcodes that has
	 *         been failed to update its location in db
	 * @param barcodes
	 *           for BlSerialProduct
	 * @return List<String>
	 */
	public Map<Integer, List<String>> getFailedPackageBarcodeList(final List<String> barcodes);

	/**
	 * This method will verify loctiona and update items to new working desk location
	 * @param lastScannedItem
	 */
	public void updateToUpsBound();

	/**
	 * This method will verify valid location and return int with appropriate notification 
	 * @param barcodes
	 * @param defaultLocations
	 * @param memberAllowedLocationList
	 * @return int
	 */
	public int checkLocationWithType(final List<String> barcodes, final List<String> defaultLocations,
			final List<String> memberAllowedLocationList);
    
 	/**
	  * This method will check the valid Location provided in barcode list and return int with appropriate
	  * notification number to notify employee for DirtyCart and DirtyPriorityCart
	  *
	  * @param barcodes the barcodes
	  * @return the int
	  */
	 int checkValidLocationInBarcodeListOfDPC(final List<String> barcodes);

 	/**
	  * This Method will return list of packages for serial barcodes
	  *
	  * @param barcodes the barcodes list
	  * @return the list of packages for serial barcodes
	  */
	 Collection<PackagingInfoModel> getPackageForSerials(final Collection<String> barcodes);

 	/**
	  * This Method will perform job for unboxing scanning process
	  *
	  * @param barcodes the barcodes
	  * @return the map
	  */
	 Map<Integer, Collection<String>> doUnboxing(final List<String> barcodes);

 	/**
 	 * This method will check current serials DirtyPriorityStatus along with today's IN and OUT orders for
 	 * current serials
 	 *
 	 * @param serialProductModel
 	 *           serial
 	 * @return true if need to flag with DirtyPriority
 	 */
 	boolean doCheckDirtyPriorityStatus(final BlSerialProductModel serialProductModel);

 	/**
 	 * This method will return location is of what type DC or DPC
 	 *
 	 * @return true if location is of dirtyPriority
 	 */
 	boolean getStatusOfLocationDP();

 	/**
 	 * This method will return all orders that needs to be shipped out today and will return today to mark
 	 * Serials with DirtyPriorityStatus
 	 *
 	 * @return Abstract Order list
 	 */
 	Collection<ConsignmentModel> getTodaysShippingOrders();

 	/**
 	 * This method will give list of consignments that has particular serial associated
 	 *
 	 * @param serial
 	 *           product
 	 * @return list Consignments
 	 */
 	Collection<ConsignmentModel> getAllConsignmentForSerial(final String serial);

 	/**
 	 * This method will mark serial's dirtyPriorityFlag true according to order IN/OUT status
 	 */
 	void flagAllDirtyPrioritySerialsOfConsignment();

 	/**
 	 * This method will mark serial's dirtyPriorityFlag true according to order IN/OUT status on new order
 	 *
 	 * @param order
 	 *           newly placed order
 	 */
 	void flagAllDirtyPrioritySerialsOfNewOrder(final ConsignmentModel order);
}
