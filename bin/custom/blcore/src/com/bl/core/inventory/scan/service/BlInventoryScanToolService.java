package com.bl.core.inventory.scan.service;

import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlSerialProductModel;

import de.hybris.platform.ordersplitting.model.ConsignmentModel;

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
 	 * method will check the valid Bin Location provided in barcode list and return int with appropriate notification
 	 * number to notify employee
 	 *
 	 * @param barcodes
 	 *           the barcodes
 	 * @param memberAllowedLocationList
 	 *           the member allowed location list
 	 * @return the int
 	 */
 	public int checkValidBinLocationInBarcodeList(final String barcodes, final List<String> memberAllowedLocationList);

 	/**
 	 * javadoc
 	 *
 	 * @param barcodes
 	 *           for BlSerialProduct
 	 * @return List<String> method will verify the list of bin barcodes and result into list of failed barcodes that has
 	 *         been failed to update its location in db
 	 */
 	public List<String> getFailedBinBarcodeList(final List<String> barcodes);

 	/**
 	 * @param barcodes
 	 * @param selectedConsignment
 	 * @return List<String> method will verify the list of barcodes and result into list of failed barcodes that are not
 	 *         valid as per the order
 	 */
 	public List<String> verifyShippingScan(final List<String> barcodes, final ConsignmentModel selectedConsignment);
 	
	/**
	 * @param barcodes
	 * @return the int
	 */
	public int checkValidTrackingId(final String barcodes);
	
	/**
	 * javadoc
	 *
	 * @param barcodes
	 *           for BlSerialProduct
	 * @return List<String> method will verify the list of bin barcodes and result into list of failed barcodes that has
	 *         been failed to update its location in db
	 */
	public List<String> getFailedPackageBarcodeList(final List<String> barcodes);

	/**
	 * @param lastScannedItem
	 */
	public void updateToUpsBound();

	/**
	 * @param barcodes
	 * @param defaultLocations
	 * @param memberAllowedLocationList
	 * @return int
	 */
	public int checkLocationWithType(final List<String> barcodes, final List<String> defaultLocations,
			final List<String> memberAllowedLocationList);
    
}
