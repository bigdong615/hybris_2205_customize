package com.bl.core.inventory.scan.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryLocationScanHistoryModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.utils.BlInventoryScanUtility;
import com.bl.logging.BlLogger;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;

import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

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
    
    private PackagingInfoModel packagingInfoModel;

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
 	public int isValidTechEngLocationBarcode(final List<String> barcodes, final List<String> memberAllowedLocationList)
 	{
 		return checkLocationWithType(barcodes, BlInventoryScanUtility.getDefaultTechEngLocation(),
 				memberAllowedLocationList);
 	}

 	@Override
	public int checkLocationWithType(final List<String> barcodes, final List<String> defaultLocations,
 			final List<String> memberAllowedLocationList)
 	{
 		final List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream().anyMatch(b::startsWith))
 				.collect(Collectors.toList());
 		return checkValidInventoryLocation(barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE),
 				filteredLocationList, memberAllowedLocationList);
 	}

 	/**
	  	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, List<String>> doTechEngSerialLocationUpdate(final List<String> barcodes)
	{
		final BlInventoryLocationModel techEngLocation = getBlInventoryLocation();
		if (Objects.nonNull(techEngLocation) && Objects.nonNull(techEngLocation.getLocationCategory()))
		{
			final String locationCategoryCode = techEngLocation.getLocationCategory().getCode();
			if (BlInventoryScanUtility.getTechEngWorkStationLocations().contains(locationCategoryCode))
			{
				return doUpdateLocation(barcodes);
			}
			else if (BlInventoryScanUtility.getTechEngCleanCartLocations().contains(locationCategoryCode))
			{
				return updateCartLocation(barcodes, techEngLocation, Boolean.FALSE);
			}
			else if (BlInventoryScanUtility.getTechEngCleanPriorityCartLocations().contains(locationCategoryCode))
			{
				return updateCartLocation(barcodes, techEngLocation, Boolean.TRUE);
			}
			else if (BlInventoryScanUtility.getTechEngRepairLocations().contains(locationCategoryCode))
			{
				return doUpdateLocation(barcodes);
			}
		}
		return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.SOMETHING_WENT_WRONG, barcodes));
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public int checkValidBinLocationInBarcodeList(final String barcodes, final List<String> memberAllowedLocationList)
	{
		final List<String> defaultLocations = BlInventoryScanUtility.getDefaultBinLocation();
		final List<String> filteredLocationList = new ArrayList<>();
		for (final String binLocation : defaultLocations)
		{
			if (barcodes.startsWith(binLocation))
			{
				filteredLocationList.add(barcodes);
			}
		}
		return checkValidInventoryLocation(barcodes, filteredLocationList, memberAllowedLocationList);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<String> getFailedBinBarcodeList(final List<String> barcodes)
	{
		final List<String> failedBarcodeList = new ArrayList<>();
		final String subList = barcodes.get(0);
		final BlInventoryLocationModel blWorkingDeskInventory = getBlInventoryLocation();
		final int result = checkValidBinLocationInBarcodeList(subList, failedBarcodeList);

		if (result == BlInventoryScanLoggingConstants.ONE)
		{
			final BlInventoryLocationModel blBinLocationModel = getBlInventoryLocation();
			blBinLocationModel.setParentInventoryLocation(blWorkingDeskInventory);
			modelService.save(blBinLocationModel);
			modelService.refresh(blBinLocationModel);
			return Collections.emptyList();
		}
		else
		{
			failedBarcodeList.addAll(barcodes);
		}
		BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FAILED_BARCODE_LIST + failedBarcodeList);
		return failedBarcodeList;
	}
	
	/**
	 * {@inheritDoc}
	 */	
	@Override
	public List<String> getFailedPackageBarcodeList(final List<String> barcodes)
	{
		final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
		final Collection<BlSerialProductModel> scannedSerialProduct = getBlInventoryScanToolDao()
				.getSerialProductsByBarcode(subList);

		final List<BlProductModel> serialProductsOnPackage = getPackagingInfoModel().getSerialProducts();
		if (CollectionUtils.isEqualCollection(scannedSerialProduct, serialProductsOnPackage))
		{
			serialProductsOnPackage.forEach(serial -> {
				if (serial instanceof BlSerialProductModel)
				{
					((BlSerialProductModel) serial).setOcLocation(getPackagingInfoModel().getTrackingNumber());
					modelService.save(serial);
				}
			});
			return Collections.emptyList();
		}

		return barcodes;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<String> verifyShippingScan(final List<String> barcodes, final ConsignmentModel selectedConsignment)
	{
		final Collection<BlSerialProductModel> blScannedProduct = getBlInventoryScanToolDao().getSerialProductsByBarcode(barcodes);

		final List<BlProductModel> filteredSerialProduct = new ArrayList<>();
		final List<BlProductModel> filteredSubPartProduct = new ArrayList<>();

		final List<BlSerialProductModel> scannedSerialProduct = new ArrayList<>();
		final List<BlSerialProductModel> scannedSubpartProduct = new ArrayList<>();

		getScannedSerial(blScannedProduct, scannedSerialProduct, scannedSubpartProduct);

		return validateConsignmentEntry(barcodes, selectedConsignment, filteredSerialProduct, filteredSubPartProduct,
				scannedSerialProduct, scannedSubpartProduct);

	}

	/**
	 * This method is used to validate serial on consignment against scanned serial 
	 * @param barcodes
	 * @param selectedConsignment
	 * @param failedBarcodeList
	 * @param blScannedProduct
	 * @param filteredSerialProduct
	 * @param filteredSubPartProduct
	 * @param scannedSerialProduct
	 * @param scannedSubpartProduct
	 */
	private List<String> validateConsignmentEntry(final List<String> barcodes, final ConsignmentModel selectedConsignment,
			final List<BlProductModel> filteredSerialProduct, final List<BlProductModel> filteredSubPartProduct,
			final List<BlSerialProductModel> scannedSerialProduct, final List<BlSerialProductModel> scannedSubpartProduct)
	{
		final List<String> failedBarcodeList = new ArrayList<>();
		
		for (final ConsignmentEntryModel consignmentEntry : selectedConsignment.getConsignmentEntries())
		{
			if (barcodes.size() == consignmentEntry.getSerialProducts().size())
			{
				verifyScan(failedBarcodeList, filteredSerialProduct, filteredSubPartProduct, scannedSerialProduct,
						scannedSubpartProduct, consignmentEntry);
			}
			else
			{
				failedBarcodeList.addAll(barcodes);
			}

		}
		return failedBarcodeList;
	}
	
	@Override
	public int checkValidTrackingId(final String lastScannedItem)
	{
		final PackagingInfoModel packagingInfo = getBlInventoryScanToolDao().getPackageInfoByCode(lastScannedItem);
		if (packagingInfo != null)
		{
			setPackagingInfoModel(packagingInfo);
			return BlInventoryScanLoggingConstants.ONE;
		}
		return BlInventoryScanLoggingConstants.TWO;
	}
	
	/**
	 * This methos is used to verify scan
	 * @param filteredSerialProduct
	 * @param filteredSubPartProduct
	 * @param scannedSerialProduct
	 * @param scannedSubpartProduct
	 * @param consignmentEntry
	 * @param updatedItemMap
	 */
	private void verifyScan(final List<String> failedBarcodeList, final List<BlProductModel> filteredSerialProduct,
			final List<BlProductModel> filteredSubPartProduct, final List<BlSerialProductModel> scannedSerialProduct,
			final List<BlSerialProductModel> scannedSubpartProduct, final ConsignmentEntryModel consignmentEntry)
	{
		final Map<String, ItemStatusEnum> itemsMap = new HashMap<>(consignmentEntry.getItems());
		
		consignmentEntry.getSerialProducts().forEach(serialItem -> 
			getSerialFromConsignment(filteredSerialProduct, filteredSubPartProduct, serialItem)
		);

		validateScannedSerial(failedBarcodeList, filteredSerialProduct, scannedSerialProduct, consignmentEntry, itemsMap);

		final List<BlProductModel> serialProductsList = new ArrayList<>(consignmentEntry.getSerialProducts());
		validateScannedSubpart(failedBarcodeList, filteredSubPartProduct, scannedSubpartProduct,
				serialProductsList, itemsMap);

		consignmentEntry.setItems(itemsMap);
		consignmentEntry.setSerialProducts(serialProductsList);
		modelService.save(consignmentEntry);
		modelService.refresh(consignmentEntry);
	}

	/**
	 * This method is used to validate scanned sub part product
	 * @param failedBarcodeList
	 * @param filteredSubPartProduct
	 * @param scannedSubpartProduct
	 * @param consignmentEntry
	 * @param updatedItemMap
	 * @param serialProductsList
	 */
	private void validateScannedSubpart(final List<String> failedBarcodeList, final List<BlProductModel> filteredSubPartProduct,
			final List<BlSerialProductModel> scannedSubpartProduct,
			final List<BlProductModel> serialProductsList, final Map<String, ItemStatusEnum> itemsMap)
	{
		scannedSubpartProduct.forEach(subpartProduct -> {
			if (!filteredSubPartProduct.contains(subpartProduct))
			{
				failedBarcodeList.add(subpartProduct.getCode());
			}
			else
			{
				updateSubpartMap(serialProductsList, subpartProduct, itemsMap);
			}
		});
	}

	/**
	 * This method is used to validate scanned serial product
	 * @param failedBarcodeList
	 * @param filteredSerialProduct
	 * @param scannedSerialProduct
	 * @param consignmentEntry
	 * @param updatedItemMap
	 */
	private void validateScannedSerial(final List<String> failedBarcodeList, final List<BlProductModel> filteredSerialProduct,
			final List<BlSerialProductModel> scannedSerialProduct, final ConsignmentEntryModel consignmentEntry,
			final Map<String, ItemStatusEnum> updatedItemMap)
	{
		scannedSerialProduct.forEach(serialProduct -> {
			if (!filteredSerialProduct.contains(serialProduct))
			{
				failedBarcodeList.add(serialProduct.getCode());
			}
			else
			{
				updateSerialProductMap(updatedItemMap, serialProduct);
			}
		});
	}
	
	/**
	 * This method is used to update Item Map based on sub part scanned
	 * @param validSerialList
	 * @param consignmentEntry
	 * @param updatedItemMap
	 * @param serialProductsList
	 * @param subpartProduct
	 * @param itemsMap
	 */
	private void updateSubpartMap(final List<BlProductModel> serialProductsList, final BlSerialProductModel subpartProduct,
			final Map<String, ItemStatusEnum> itemsMap)
	{
		String subPartName = subpartProduct.getBlProduct().getName();
		BlProductModel blProduct = subpartProduct.getBlProduct();
		final String updatedName = subPartName.concat(BlInventoryScanLoggingConstants.HYPHEN);

		if (itemsMap.containsKey(subPartName)
				&& itemsMap.get(subPartName).equals(ItemStatusEnum.NOT_INCLUDED))
		{
			itemsMap.remove(subPartName);
			itemsMap.put(blProduct.getCode(), ItemStatusEnum.INCLUDED);
		}
		else
		{
			getUpdatedName(itemsMap, updatedName, subpartProduct);
		}
		if (serialProductsList.contains(blProduct))
		{
			serialProductsList.remove(blProduct);
			serialProductsList.add(subpartProduct);
		}
	}

	private void getUpdatedName(final Map<String, ItemStatusEnum> itemsMap, final String updatedName,
			final BlSerialProductModel subpartProduct)
	{
		final List<String> keyList = itemsMap.keySet().stream()
				.filter(key -> key.contains(updatedName) && itemsMap.get(key).equals(ItemStatusEnum.NOT_INCLUDED))
				.collect(Collectors.toList());
		if (CollectionUtils.isNotEmpty(keyList))
		{
			itemsMap.remove(keyList.get(0));
			itemsMap.put(subpartProduct.getBlProduct().getCode(), ItemStatusEnum.INCLUDED);
		}
	}

	/**
	 * This method is used to update Item Map based on the serial scanned
	 * @param validSerialList
	 * @param consignmentEntry
	 * @param updatedItemMap
	 * @param serialProduct
	 */
	private void updateSerialProductMap(final Map<String, ItemStatusEnum> itemsMap,
			final BlSerialProductModel serialProduct)
	{
		if (itemsMap.containsKey(serialProduct.getCode())
				&& itemsMap.get(serialProduct.getCode()).equals(ItemStatusEnum.NOT_INCLUDED))
		{
			itemsMap.replace(serialProduct.getCode(), ItemStatusEnum.INCLUDED);
			serialProduct.setHardAssigned(true);
			modelService.save(serialProduct);
		}
	}

	/**
	 * This method is used to get the serial/subpart from consignment
	 * @param filteredSerialProduct
	 * @param filteredSubPartProduct
	 * @param serialItem
	 */
	private void getSerialFromConsignment(final List<BlProductModel> filteredSerialProduct,
			final List<BlProductModel> filteredSubPartProduct, final BlProductModel serialItem)
	{
		if (serialItem instanceof BlSerialProductModel)
		{
			if (!ProductTypeEnum.SUBPARTS.equals(serialItem.getProductType()))
			{
				filteredSerialProduct.add(serialItem);
			}
			else if (((BlSerialProductModel) serialItem).getBarcode() !=null && ProductTypeEnum.SUBPARTS.equals(serialItem.getProductType()))
			{
				filteredSubPartProduct.add(serialItem);
			}
		}
	}
	
	/**
	 * This method is used to get scanned serial product
	 * @param blScannedProduct
	 * @param scannedSerialProduct
	 * @param scannedSubpartProduct
	 */
	private void getScannedSerial(final Collection<BlSerialProductModel> blScannedProduct,
			final List<BlSerialProductModel> scannedSerialProduct, final List<BlSerialProductModel> scannedSubpartProduct)
	{
		blScannedProduct.forEach(scannedProduct -> {
			if (!ProductTypeEnum.SUBPARTS.equals(scannedProduct.getProductType()))
			{
				scannedSerialProduct.add(scannedProduct);
			}
			else if (ProductTypeEnum.SUBPARTS.equals(scannedProduct.getProductType()))
			{
				scannedSubpartProduct.add(scannedProduct);
			}
		});
	}



	/**
	 * Do update location on items.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @return the map
	 */
	private Map<String, List<String>> doUpdateLocation(final List<String> barcodes)
	{
		final List<String> failedBarcodeList = getFailedBarcodeList(barcodes);
		return getFailedBarcodesMap(failedBarcodeList, BlInventoryScanLoggingConstants.MISSING_BARCODE_ITEMS);
	}

	/**
	 * Update cart location on items.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param blCleanCartLocation
	 *           the bl clean cart location
	 * @param isPriorityCartLocation
	 *           the is priority cart location
	 * @return the map
	 */
	private Map<String, List<String>> updateCartLocation(final List<String> barcodes,
			final BlInventoryLocationModel blCleanCartLocation, final boolean isPriorityCartLocation)
	{
		final List<String> failedBarcodeList = new ArrayList<>();
		final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
		final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
		if (blSerialProducts.size() == subList.size())
		{
			return isPriorityCartLocation ? updateCleanPriorityCartLocation(blSerialProducts, failedBarcodeList, blCleanCartLocation)
					: updateCleanCartLocation(blSerialProducts, failedBarcodeList, blCleanCartLocation);
		}
		return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.MISSING_BARCODE_ITEMS, barcodes));
	}

	/**
	 * Update clean cart location of item.
	 *
	 * @param blSerialProducts
	 *           the bl serial products
	 * @param failedBarcodeList
	 *           the failed barcode list
	 * @param blCleanCartLocation
	 *           the bl clean cart location
	 * @return the map
	 */
	private Map<String, List<String>> updateCleanCartLocation(final Collection<BlSerialProductModel> blSerialProducts,
			final List<String> failedBarcodeList, final BlInventoryLocationModel blCleanCartLocation)
	{
		blSerialProducts.forEach(serial -> {
			if (BooleanUtils.isFalse(serial.isDirtyPriorityStatus()))
			{ // check for dirtycart flag on serial 
				updateLocationOnItem(serial, blCleanCartLocation);
			}
			else
			{
				failedBarcodeList.add(serial.getBarcode());
			}
		});
		return getFailedBarcodesMap(failedBarcodeList, BlInventoryScanLoggingConstants.WRONG_ITEM_CLEAN_CART);
	}

	/**
	 * Update clean priority cart location of item.
	 *
	 * @param blSerialProducts
	 *           the bl serial products
	 * @param failedBarcodeList
	 *           the failed barcode list
	 * @param blCleanCartLocation
	 *           the bl clean cart location
	 * @return the map
	 */
	private Map<String, List<String>> updateCleanPriorityCartLocation(final Collection<BlSerialProductModel> blSerialProducts,
			final List<String> failedBarcodeList, final BlInventoryLocationModel blCleanCartLocation)
	{
		blSerialProducts.forEach(serial -> {
			if (BooleanUtils.isTrue(serial.isDirtyPriorityStatus()))
			{ // check for dirtycart flag on serial 
				updateLocationOnItem(serial, blCleanCartLocation);
			}
			else
			{
				failedBarcodeList.add(serial.getBarcode());
			}
		});
		return getFailedBarcodesMap(failedBarcodeList, BlInventoryScanLoggingConstants.WRONG_ITEM_CLEAN_PRIORITY_CART);
	}
	
	/**
	 * Gets the failed barcodes map.
	 *
	 * @param failedBarcodeList
	 *           the failed barcode list
	 * @param messageCode
	 *           the message code
	 * @return the failed barcodes map
	 */
	private Map<String, List<String>> getFailedBarcodesMap(final List<String> failedBarcodeList, final String messageCode)
	{
		return Maps.newHashMap(CollectionUtils.isNotEmpty(failedBarcodeList) ? ImmutableMap.of(messageCode, failedBarcodeList)
				: ImmutableMap.of(BlInventoryScanLoggingConstants.SUCCESS, Collections.emptyList()));
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
    
 	/**
 	 * @return the packagingInfoModel
 	 */
 	public PackagingInfoModel getPackagingInfoModel()
 	{
 		return packagingInfoModel;
 	}

 	/**
 	 * @param packagingInfoModel
 	 *           the packagingInfoModel to set
 	 */
 	public void setPackagingInfoModel(final PackagingInfoModel packagingInfoModel)
 	{
 		this.packagingInfoModel = packagingInfoModel;
 	}

	@Override
	public void updateToUpsBound()
	{
		final List<BlProductModel> serialProducts = getPackagingInfoModel().getSerialProducts();
		serialProducts.forEach(serial -> {
			if (serial instanceof BlSerialProductModel)
			{
				final BlSerialProductModel blSerial = ((BlSerialProductModel) serial);
				blSerial.setOcLocation(getBlInventoryLocation().getCode());
				blSerial.setSerialStatus(SerialStatusEnum.BOXED);
				modelService.save(blSerial);
			}
		});
	}
}
