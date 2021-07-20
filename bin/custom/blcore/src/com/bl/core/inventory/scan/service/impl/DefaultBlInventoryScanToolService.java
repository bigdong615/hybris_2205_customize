package com.bl.core.inventory.scan.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryLocationScanHistoryModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.logging.BlLogger;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;

import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class DefaultBlInventoryScanToolService implements BlInventoryScanToolService {

    private static final Logger LOG = Logger.getLogger(DefaultBlInventoryScanToolService.class);

    @Autowired
    UserService userService;

    @Autowired
    ModelService modelService;

    @Resource(name = "blInventoryScanToolDao")
    BlInventoryScanToolDao blInventoryScanToolDao;

    private BlInventoryLocationModel blInventoryLocation;

    /**
     * {@inheritDoc}
     */
    @Override
    public BlInventoryLocationModel getInventoryLocationById(final String locationId) {
        return getBlInventoryScanToolDao().getInventoryLocationById(locationId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode) {
        return getBlInventoryScanToolDao().getSerialProductsByBarcode(barcode);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int checkValidLocationInBarcodeList(final List<String> barcodes, final List<String> memberAllowedLocationList) {
        final List<String> defaultLocations = BlInventoryScanLoggingConstants.getDefaultInventoryLocation();
        final List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream()
                .anyMatch(b::startsWith)).collect(Collectors.toList());
        return checkValidInventoryLocation(barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE), 
      		  filteredLocationList, memberAllowedLocationList);
    }

    /**
     * method will check size of input barcode list and validate against its size. If doesnt satisfy AC then will send
     * number to notify employee
     *
     * @param inventoryLocation the inventory location
     * @param filteredLocationList the filtered location list
     * @param memberAllowedLocationList the member allowed location list
     * @return the int
     */
    public int checkValidInventoryLocation(final String inventoryLocation, final List<String> filteredLocationList,
   		 final List<String> memberAllowedLocationList) {
        if (CollectionUtils.isNotEmpty(filteredLocationList)) {
            if (filteredLocationList.size() == BlInventoryScanLoggingConstants.ONE) {
                return validateLocation(inventoryLocation, filteredLocationList, memberAllowedLocationList);
            }
            BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.MANY_LOCATION_ERROR_FAILURE_MSG);
            return BlInventoryScanLoggingConstants.FOUR;
        }
        BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG);
        return BlInventoryScanLoggingConstants.THREE;
    }

    /**
     * method will validate last location in list and filteredLocationList to satisfy scenario of "last barcode
     * should be a valid location" and if not then will send number to notify employee
     *
     * @param inventoryLocation the inventory location
     * @param filteredLocationList the filtered location list
     * @param memberAllowedLocationList the member allowed location list
     * @return the int
     */
    public int validateLocation(final String inventoryLocation, final List<String> filteredLocationList,
   		 final List<String> memberAllowedLocationList) {
        if (filteredLocationList.get(BlInventoryScanLoggingConstants.ZERO).equals(inventoryLocation)) {
            final BlInventoryLocationModel blLocalInventoryLocation = getBlInventoryScanToolDao().getInventoryLocationById(inventoryLocation);
            if (isLocationValidForMember(memberAllowedLocationList, blLocalInventoryLocation)) {
                setBlInventoryLocation(blLocalInventoryLocation);
                return BlInventoryScanLoggingConstants.ONE;
            }
            BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG);
            return BlInventoryScanLoggingConstants.TWO;
        }
        BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.LAST_SCAN_ERROR_FAILURE_MSG);
        return BlInventoryScanLoggingConstants.THREE;
    }

	/**
	 * Checks if location is valid for member.
	 *
	 * @param memberAllowedLocationList the member allowed location list
	 * @param blLocalInventoryLocation the bl local inventory location
	 * @return true, if is location valid for member
	 */
	private boolean isLocationValidForMember(final List<String> memberAllowedLocationList,
			final BlInventoryLocationModel blLocalInventoryLocation) {
		return Objects.nonNull(blLocalInventoryLocation) && Objects.nonNull(blLocalInventoryLocation.getLocationCategory()) 
				&& memberAllowedLocationList.contains(blLocalInventoryLocation.getLocationCategory().getCode());
	}

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getFailedBarcodeList(final List<String> barcodes) {
        final List<String> failedBarcodeList = new ArrayList<>();
        final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
        final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
        subList.forEach(barcode -> setInventoryLocationOnSerial(failedBarcodeList, blSerialProducts, barcode));
        BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FAILED_BARCODE_LIST + failedBarcodeList);
        return failedBarcodeList;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getConfigKeyFromScanConfiguration(final String key) {
        BlInventoryScanConfigurationModel blInventoryScanConfigurationModel = getBlInventoryScanToolDao().getConfigKeyFromScanConfiguration(key);
        return blInventoryScanConfigurationModel != null ? blInventoryScanConfigurationModel.getBlScanConfigValue() :
                String.valueOf(BlInventoryScanLoggingConstants.TEN);
    }

    /**
     * javadoc
     * @param failedBarcodeList from scanned barcode list
     * @param blSerialProducts  from barcodes
     * @param iteratorBarcode   current iterator
     * method will update location on serial and save it. Also, it will create a history for scan and will associate with
     * the InventoryLocation if barcode is valid serial product and if not then will fill it into failedBarcodeList to
     * check status of scan that success or failure
     */
    public void setInventoryLocationOnSerial(final List<String> failedBarcodeList, final Collection<BlSerialProductModel> blSerialProducts,
                                              final String iteratorBarcode) {
        final BlSerialProductModel blSerialProduct = blSerialProducts.stream()
                .filter(p -> p.getBarcode().equals(iteratorBarcode)).findFirst().orElse(null);
        if (blSerialProduct != null) {
      	  final BlInventoryLocationModel blInventoryLocationLocal = getBlInventoryLocation();
            updateLocationOnItem(blSerialProduct, blInventoryLocationLocal);     
        } else {
            failedBarcodeList.add(iteratorBarcode);
        }
    }

    /**
 	 * Update location on item.
 	 *
 	 * @param blSerialProduct
 	 *           the bl serial product
 	 * @param blInventoryLocationLocal
 	 *           the bl inventory location local
 	 */
 	private void updateLocationOnItem(final BlSerialProductModel blSerialProduct,
 			final BlInventoryLocationModel blInventoryLocationLocal)
 	{
 		blSerialProduct.setOcLocation(blInventoryLocationLocal.getCode());
 		blSerialProduct.setLastLocationScanParent(blInventoryLocationLocal.getParentInventoryLocation() != null
 				? blInventoryLocationLocal.getParentInventoryLocation().getCode()
 				: null);
 		blSerialProduct.setOcLocationDetails(blInventoryLocationLocal);
 		modelService.save(blSerialProduct);
 		modelService.refresh(blSerialProduct);
 		/* Scan History Entry */
 		setBlLocationScanHistory(blSerialProduct);
 	}

 	/**
 	 * Sets the Location scan history.
 	 *
 	 * @param blSerialProduct
 	 *           the new bl location scan history
 	 */
 	private void setBlLocationScanHistory(final BlSerialProductModel blSerialProduct)
 	{
 		final BlInventoryLocationScanHistoryModel blInventoryLocationScanHistory = modelService
 				.create(BlInventoryLocationScanHistoryModel.class);
 		blInventoryLocationScanHistory.setSerialProduct(blSerialProduct);
 		blInventoryLocationScanHistory.setScanUser(userService.getCurrentUser());
 		blInventoryLocationScanHistory.setBlInventoryLocation(blInventoryLocation);
 		blInventoryLocationScanHistory.setScanTime(new Date());
 		modelService.save(blInventoryLocationScanHistory);
 		modelService.refresh(blInventoryLocationScanHistory);
 	}
    
    /**
     * {@inheritDoc}
     */
    @Override
    public int isValidTechEngLocationBarcode(final List<String> barcodes, final List<String> memberAllowedLocationList) {
        return checkLocationWithType(barcodes, BlInventoryScanLoggingConstants.getDefaultTechEngLocation(), memberAllowedLocationList);
    }
    
    private int checkLocationWithType(final List<String> barcodes, final List<String> defaultLocations, final List<String> memberAllowedLocationList) {
   	 final List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream()
   			 .anyMatch(b::startsWith)).collect(Collectors.toList());
   	 return checkValidInventoryLocation(barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE),
   			 filteredLocationList, memberAllowedLocationList);
    }
    
    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String,List<String>> doTechEngSerialLocationUpdate(final List<String> barcodes) {   	 
   	 final BlInventoryLocationModel techEngLocation = getBlInventoryLocation();
   	 if(Objects.nonNull(techEngLocation) && Objects.nonNull(techEngLocation.getLocationCategory())) {
   		 final String locationCategoryCode = techEngLocation.getLocationCategory().getCode();
   		 if(BlInventoryScanLoggingConstants.getTechEngWorkStationLocations().contains(locationCategoryCode)) {
   			 return doUpdateLocation(barcodes);
      	 }
   		 else if(BlInventoryScanLoggingConstants.getTechEngCleanCartLocations().contains(locationCategoryCode)) {
   			 return updateCleanCartLocation(barcodes, techEngLocation);
   		 }
   		 else if(BlInventoryScanLoggingConstants.getTechEngCleanPriorityCartLocations().contains(locationCategoryCode)) {
   			 return updateCleanPriorityCartLocation(barcodes, techEngLocation);
   		 }
   		 else if(BlInventoryScanLoggingConstants.getTechEngRepairLocations().contains(locationCategoryCode)) {
   			 return doUpdateLocation(barcodes);
   		 }
   	 }
   	 return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.SOMETHING_WENT_WRONG,barcodes));
    }

	/**
	 * Do update location on items.
	 *
	 * @param barcodes the barcodes
	 * @return the map
	 */
	private Map<String, List<String>> doUpdateLocation(final List<String> barcodes)
	{
		final List<String> failedBarcodeList = getFailedBarcodeList(barcodes);
		 return Maps.newHashMap(
				 CollectionUtils.isNotEmpty(failedBarcodeList) 
				 	? ImmutableMap.of(BlInventoryScanLoggingConstants.MISSING_BARCODE_ITEMS,failedBarcodeList)
				 			: ImmutableMap.of(BlInventoryScanLoggingConstants.SUCCESS,Collections.emptyList()));
	}
    
    /**
     * Update clean cart location of item.
     *
     * @param barcodes the barcodes
     * @param blCleanCartLocation the bl clean cart location
     * @return the map
     */
    private Map<String,List<String>> updateCleanCartLocation(final List<String> barcodes, final BlInventoryLocationModel blCleanCartLocation){
   	 final List<String> failedBarcodeList = new ArrayList<>();
       final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
       final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
       if(blSerialProducts.size() == subList.size()) {
      	 blSerialProducts.forEach(serial -> {
      		 if(BooleanUtils.isFalse(serial.isDirtyPriorityStatus())) { // check for dirtycart flag on serial 
      			 updateLocationOnItem(serial, blCleanCartLocation);
             }
             else {
            	 failedBarcodeList.add(serial.getBarcode());
             }
      	 });
      	 return Maps.newHashMap(
					 CollectionUtils.isNotEmpty(failedBarcodeList) 
					 	? ImmutableMap.of(BlInventoryScanLoggingConstants.WRONG_ITEM_CLEAN_CART,failedBarcodeList)
					 			: ImmutableMap.of(BlInventoryScanLoggingConstants.SUCCESS,Collections.emptyList()));
       }   	 
   	 return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.MISSING_BARCODE_ITEMS,barcodes));
    }
    
    /**
     * Update clean priority cart location of item.
     *
     * @param barcodes the barcodes
     * @param blCleanCartLocation the bl clean cart location
     * @return the map
     */
    private Map<String,List<String>> updateCleanPriorityCartLocation(final List<String> barcodes, final BlInventoryLocationModel blCleanCartLocation){
   	 final List<String> failedBarcodeList = new ArrayList<>();
       final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
       final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
       if(blSerialProducts.size() == subList.size()) {
      	 blSerialProducts.forEach(serial -> {
      		 if(BooleanUtils.isTrue(serial.isDirtyPriorityStatus())) { // check for dirtycart flag on serial 
      			 updateLocationOnItem(serial, blCleanCartLocation);
             }
             else {
            	 failedBarcodeList.add(serial.getBarcode());
             }
      	 });
      	 return Maps.newHashMap(
					 CollectionUtils.isNotEmpty(failedBarcodeList) 
					 	? ImmutableMap.of(BlInventoryScanLoggingConstants.WRONG_ITEM_CLEAN_PRIORITY_CART,failedBarcodeList)
					 			: ImmutableMap.of(BlInventoryScanLoggingConstants.SUCCESS,Collections.emptyList()));
       }   	 
   	 return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.MISSING_BARCODE_ITEMS,barcodes));
    }

    public BlInventoryLocationModel getBlInventoryLocation() {
        return blInventoryLocation;
    }

    public void setBlInventoryLocation(final BlInventoryLocationModel blInventoryLocation) {
        this.blInventoryLocation = blInventoryLocation;
    }

    public BlInventoryScanToolDao getBlInventoryScanToolDao() {
        return blInventoryScanToolDao;
    }

    public void setBlInventoryScanToolDao(final BlInventoryScanToolDao blInventoryScanToolDao) {
        this.blInventoryScanToolDao = blInventoryScanToolDao;
    }
}
