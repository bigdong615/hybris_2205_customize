package com.bl.core.inventory.scan.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.PackagingInfoStatus;
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
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.product.ProductModel;
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
 * This service class is used perform Inventory Scanning Tool services 
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
    
    private boolean isLocationDP;

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
     * This method will update location on serial and save it. Also, it will create a history for scan and will associate with
     * the InventoryLocation if barcode is valid serial product and if not then will fill it into failedBarcodeList to
     * check status of scan that success or failure
     * 
     * @param failedBarcodeList from scanned barcode list
     * @param blSerialProducts  from barcodes
     * @param iteratorBarcode   current iterator
     * 
     */
    public void setInventoryLocationOnSerial(final List<String> failedBarcodeList, final Collection<BlSerialProductModel> blSerialProducts,
                                              final String iteratorBarcode) {
        final BlSerialProductModel blSerialProduct = blSerialProducts.stream()
                .filter(p -> p.getBarcode().equals(iteratorBarcode)).findFirst().orElse(null);
        doUpdateLocation(failedBarcodeList, iteratorBarcode, blSerialProduct);
    }

	/**
	 * Do update location.
	 *
	 * @param failedBarcodeList the failed barcode list
	 * @param iteratorBarcode the iterator barcode
	 * @param blSerialProduct the bl serial product
	 */
	private void doUpdateLocation(final List<String> failedBarcodeList, final String iteratorBarcode,
			final BlSerialProductModel blSerialProduct)
	{
		if (blSerialProduct != null) {
      	  final BlInventoryLocationModel blInventoryLocationLocal = getBlInventoryLocation();
            updateLocationOnItem(blSerialProduct, blInventoryLocationLocal, Boolean.FALSE);     
        } else {
            failedBarcodeList.add(iteratorBarcode);
        }
	}
    
	/**
	 * Sets the inventory location on serial.
	 *
	 * @param failedBarcodeList the failed barcode list
	 * @param blSerialProducts the bl serial products
	 * @param iteratorBarcode the iterator barcode
	 * @param dirtyProductSerialModels the dirty product serial models
	 */
	private void setInventoryLocationOnSerial(final List<String> failedBarcodeList,
			final Collection<BlSerialProductModel> blSerialProducts, final String iteratorBarcode,
			final Collection<String> dirtyProductSerialModels)
	{
		final BlSerialProductModel blSerialProduct = blSerialProducts.stream().filter(p -> p.getBarcode().equals(iteratorBarcode))
				.findFirst().orElse(null);
		if (Objects.nonNull(blSerialProduct))
		{
			if(Objects.nonNull(blSerialProduct.getProductType()) 
					&& blSerialProduct.getProductType().equals(ProductTypeEnum.SUBPARTS))
			{
				blSerialProduct.setSerialStatus(SerialStatusEnum.IN_HOUSE);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing Serial status to In-House for sub part with code : {}", blSerialProduct.getCode());
				modelService.save(blSerialProduct);
				modelService.refresh(blSerialProduct);
			}
			else
			{
				blSerialProduct.setSerialStatus(SerialStatusEnum.PARTIALLY_UNBOXED);
				checkItemIsDirty(blSerialProduct);
				if (blSerialProduct.isDirtyPriorityStatus())
				{
					dirtyProductSerialModels.add(blSerialProduct.getBarcode());
				}
				doUpdateLocation(failedBarcodeList, iteratorBarcode, blSerialProduct);
			}			
		}
		else
		{
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
 			final BlInventoryLocationModel blInventoryLocationLocal, final boolean unboxStatus)
 	{
 		if(unboxStatus)
 		{
 			blSerialProduct.setSerialStatus(SerialStatusEnum.UNBOXED);
 		}
 		blSerialProduct.setOcLocation(blInventoryLocationLocal.getCode());
 		blSerialProduct.setLastLocationScanParent(blInventoryLocationLocal.getParentInventoryLocation() != null
 				? blInventoryLocationLocal.getParentInventoryLocation().getCode()
 				: null);
 		blSerialProduct.setOcLocationDetails(blInventoryLocationLocal);
 		modelService.save(blSerialProduct);
 		modelService.refresh(blSerialProduct);
 		/* Scan History Entry */
 		setBlLocationScanHistory(blSerialProduct, unboxStatus);
 	}

 	/**
 	 * Sets the Location scan history.
 	 *
 	 * @param blSerialProduct
 	 *           the new bl location scan history
 	 */
 	private void setBlLocationScanHistory(final BlSerialProductModel blSerialProduct, final boolean unboxStatus)
 	{
 		final BlInventoryLocationScanHistoryModel blInventoryLocationScanHistory = modelService
 				.create(BlInventoryLocationScanHistoryModel.class);
 		blInventoryLocationScanHistory.setSerialProduct(blSerialProduct);
 		blInventoryLocationScanHistory.setScanUser(userService.getCurrentUser());
 		blInventoryLocationScanHistory.setBlInventoryLocation(blInventoryLocation);
 		blInventoryLocationScanHistory.setScanTime(new Date());
 		blInventoryLocationScanHistory.setUnboxedHistory(unboxStatus);
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

 	private int checkLocationWithType(final List<String> barcodes, final List<String> defaultLocations,
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
		final List<String> missingBarcodeSerialList = new ArrayList<>();
		Map<String, List<String>> processStatus = Maps.newHashMap();
		final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
		final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
		getMissingBarcodeItems(blSerialProducts, missingBarcodeSerialList, Lists.newArrayList(subList));
		if(CollectionUtils.isNotEmpty(missingBarcodeSerialList)) 
		{
			processStatus.put(BlInventoryScanLoggingConstants.MISSING_BARCODE_ITEMS,missingBarcodeSerialList);
		}
		processStatus.putAll(performCartLocationUpdate(blSerialProducts, failedBarcodeList, blCleanCartLocation, isPriorityCartLocation));
		return processStatus;
	}
	
	/**
	 * Gets the missing barcode items.
	 *
	 * @param blSerialProducts the bl serial products
	 * @param missingBarcodeSerialList the missing barcode serial list
	 * @param barcodes the barcodes
	 * @return the missing barcode items
	 */
	private void getMissingBarcodeItems(final Collection<BlSerialProductModel> blSerialProducts, 
			final List<String> missingBarcodeSerialList, final List<String> barcodes)
	{
		final List<String> availableBarcodes = blSerialProducts.stream().map(BlSerialProductModel::getBarcode)
				.collect(Collectors.toList());
		barcodes.removeIf(availableBarcodes::contains);
		missingBarcodeSerialList.addAll(barcodes);
	}
	
	/**
	 * Perform cart location update.
	 *
	 * @param blSerialProducts the bl serial products
	 * @param failedBarcodeList the failed barcode list
	 * @param blCleanCartLocation the bl clean cart location
	 * @param isPriorityCartLocation the is priority cart location
	 * @return the map
	 */
	private Map<String, List<String>> performCartLocationUpdate(final Collection<BlSerialProductModel> blSerialProducts,
			final List<String> failedBarcodeList, final BlInventoryLocationModel blCleanCartLocation, final boolean isPriorityCartLocation)
	{
		if(isPriorityCartLocation)
		{
			return updateCleanPriorityCartLocation(blSerialProducts, failedBarcodeList, blCleanCartLocation);
		}
		return updateCleanCartLocation(blSerialProducts, failedBarcodeList, blCleanCartLocation);
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
				updateLocationOnItem(serial, blCleanCartLocation, Boolean.FALSE);
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
				updateLocationOnItem(serial, blCleanCartLocation, Boolean.FALSE);
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
	
	/**
	 * {@inheritDoc}
	 * 
	 */
	@Override
	public int checkValidLocationInBarcodeListOfDPC(final List<String> barcodes)
	{
		return checkLocationWithType(barcodes, BlInventoryScanUtility.getDefaultInventoryLocationForDPCAndDC(),
				BlInventoryScanUtility.getUnboxingAllowedLocations());
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<PackagingInfoModel> getPackageForSerials(final Collection<String> barcodes)
	{
		return getBlInventoryScanToolDao().getPackageForSerials(barcodes);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<Integer, Collection<String>> doUnboxing(final List<String> barcodes)
	{
		final Map<Integer, Collection<String>> result = new HashMap<>();
		final BlInventoryLocationModel blInventoryLocationModel = getBlInventoryLocation();
		final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
		if (Objects.nonNull(blInventoryLocationModel) && Objects.nonNull(blInventoryLocationModel.getLocationCategory()))
		{
			final String locationCategory = blInventoryLocationModel.getLocationCategory().getCode();
			if(BlInventoryScanUtility.getUnBoxingWorkStationLocations().contains(locationCategory))
			{
				final List<String> failedBarcodeList = new ArrayList<>();
				final Collection<String> dirtyProductSerialModels = new ArrayList<>();
				this.getResultMapForUnboxAtWorkstation(result, failedBarcodeList, dirtyProductSerialModels,
						subList);
			}
			else
			{
				setLocationDP(BlInventoryScanUtility.getDirtyPriorityCartLocations().contains(locationCategory));
				final Collection<PackagingInfoModel> packagingInfoModels = this.getPackageForSerials(subList);
				final Collection<String> errorSerialList = new ArrayList<>();
				this.getMapForUnboxAtDPOrDC(subList, result, blInventoryLocationModel, packagingInfoModels, errorSerialList);
			}
			
		}
		
		return result;
	}

	/**
	 * This method will return resultant map for Unboxing at DirtyCart or DirtyPriorityCart
	 *
	 * @param barcodes
	 *           list
	 * @param result
	 *           map
	 * @param blInventoryLocationModel
	 *           location
	 * @param packagingInfoModels
	 *           packages
	 * @param errorSerialList
	 *           list
	 */
	private void getMapForUnboxAtDPOrDC(final Collection<String> barcodes, final Map<Integer, Collection<String>> result,
			final BlInventoryLocationModel blInventoryLocationModel, final Collection<PackagingInfoModel> packagingInfoModels,
			Collection<String> errorSerialList)
	{
		if(CollectionUtils.isEmpty(packagingInfoModels))
		{
			result.put(BlInventoryScanLoggingConstants.ZERO, barcodes);
		}
		else
		{
			errorSerialList = doPerformDpcOrDcUnboxing(barcodes, blInventoryLocationModel, packagingInfoModels, errorSerialList);
		}
		handleUnboxingErrorMessage(result, errorSerialList);
	}

	/**
	 * Handle unboxing error message.
	 *
	 * @param result the result
	 * @param errorSerialList the error serial list
	 */
	private void handleUnboxingErrorMessage(final Map<Integer, Collection<String>> result, final Collection<String> errorSerialList)
	{
		if (Objects.isNull(errorSerialList)) 
		{
			result.put(BlInventoryScanLoggingConstants.TWO, null);
			BlLogger.logMessage(LOG, Level.DEBUG, "Failed to find package for scanned serials");
		}
		else
		{
			result.put(BlInventoryScanLoggingConstants.THREE, errorSerialList);
			BlLogger.logMessage(LOG, Level.DEBUG, "Scanned Performed with errorSerials if any exists");
		}
	}

	/**
	 * Do perform dirty priority cart or dirty cart unboxing.
	 *
	 * @param barcodes the barcodes
	 * @param blInventoryLocationModel the bl inventory location model
	 * @param packagingInfoModels the packaging info models
	 * @param errorSerialList the error serial list
	 * @return the collection
	 */
	private Collection<String> doPerformDpcOrDcUnboxing(final Collection<String> barcodes,
			final BlInventoryLocationModel blInventoryLocationModel, final Collection<PackagingInfoModel> packagingInfoModels,
			Collection<String> errorSerialList)
	{
		for (final PackagingInfoModel packagingInfoModel : packagingInfoModels)
		{
			final List<BlProductModel> blSerialProductModels = packagingInfoModel.getSerialProducts();
			if (CollectionUtils.isNotEmpty(blSerialProductModels))
			{
				errorSerialList = getBlSerialProductModelBooleanMap(packagingInfoModel, packagingInfoModel.getConsignment(),
						blSerialProductModels.stream().filter(
								serial -> barcodes.stream().anyMatch(b -> b.equals(((BlSerialProductModel) serial).getBarcode())))
								.collect(Collectors.toList()),
						blInventoryLocationModel);
			}
		}
		return errorSerialList;
	}

	/**
	 * This method will return resultant map for Unboxing at workstation location
	 *
	 * @param result
	 *           map
	 * @param failedBarcodeList
	 *           list
	 * @param dirtyProductSerialModels
	 *           serials
	 * @param blSerialProducts
	 *           model
	 */
	private void getResultMapForUnboxAtWorkstation(final Map<Integer, Collection<String>> result,
			final List<String> failedBarcodeList, final Collection<String> dirtyProductSerialModels,
			final Collection<String> barcodes)
	{ 
		final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(barcodes);
		if (CollectionUtils.isEmpty(blSerialProducts))
		{
			result.put(BlInventoryScanLoggingConstants.ZERO, barcodes);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Failed barcode list: {}", failedBarcodeList);	
		}
		else 
		{
			barcodes.forEach(barcode -> setInventoryLocationOnSerial(failedBarcodeList, blSerialProducts, barcode, dirtyProductSerialModels));
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Failed barcode list: {}", failedBarcodeList);			
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Dirty Priority Serials : {}", dirtyProductSerialModels);
			result.put(BlInventoryScanLoggingConstants.ZERO, failedBarcodeList);
			result.put(BlInventoryScanLoggingConstants.ONE, dirtyProductSerialModels);
		}		
	}


	/**
	 * This method will change status of package and consignment also it will check failed barcodes also with correct
	 * barcode and will execute scan records for the same
	 *
	 * @param packagingInfoModel
	 *           package
	 * @param consignmentModel
	 *           consignment
	 * @param availableBarcodeList
	 *           barcode list
	 * @return list of serial with incorrect location
	 */
	private Collection<String> getBlSerialProductModelBooleanMap(final PackagingInfoModel packagingInfoModel,
			final ConsignmentModel consignmentModel, final Collection<BlProductModel> availableBarcodeList,
			final BlInventoryLocationModel blInventoryLocationModel)
	{
		if (CollectionUtils.isNotEmpty(availableBarcodeList))
		{
			BlLogger.logMessage(LOG, Level.DEBUG, "Marking package and consignment as PARTIALLY_UNBOXED");
			changePackagingInfoStatus(packagingInfoModel, PackagingInfoStatus.PARTIALLY_UNBOXED);
			changeConsignmentStatus(consignmentModel, ConsignmentStatus.PARTIALLY_UNBOXED);
			final Collection<String> failedSerials = checkSerialsForDPAndSubParts(availableBarcodeList, consignmentModel,
					blInventoryLocationModel);
			if (CollectionUtils.isEmpty(failedSerials))
			{
				changePackagingInfoStatus(packagingInfoModel, PackagingInfoStatus.UNBOXED);
				final Collection<PackagingInfoModel> consignmentPackages = consignmentModel.getPackaginginfos();
				if (CollectionUtils.isNotEmpty(consignmentPackages) && consignmentPackages.stream()
						.allMatch(pkg -> PackagingInfoStatus.UNBOXED.equals(pkg.getPackagingInfoStatus())))
				{
					changeConsignmentStatus(consignmentModel, ConsignmentStatus.UNBOXED);
					BlLogger.logMessage(LOG, Level.DEBUG, "Marked Consignment as Unboxed as all packages are unboxed");
				}
			}
			return failedSerials;
		}
		return null; //NOSONAR
	}

	/**
	 * This method will check dirtyPriority with SubParts status update
	 *
	 * @param blSerialProductModels the bl serial product models
	 * @param consignmentModel the consignment model
	 * @param blInventoryLocationModel the bl inventory location model
	 * @return the collection
	 */
	public Collection<String> checkSerialsForDPAndSubParts(final Collection<BlProductModel> blSerialProductModels,
			final ConsignmentModel consignmentModel, final BlInventoryLocationModel blInventoryLocationModel)
	{
		final Collection<String> serialList = new ArrayList<>();
		if (CollectionUtils.isNotEmpty(blSerialProductModels))
		{
			for (final BlProductModel model : blSerialProductModels)
			{
				if(model instanceof BlSerialProductModel) 
				{
					final BlSerialProductModel serialProductModel = ((BlSerialProductModel) model);
					serialProductModel.setAssociatedConsignment(consignmentModel);
					serialProductModel.setAssociatedOrder(
							consignmentModel.getOrder() instanceof OrderModel ? ((OrderModel) consignmentModel.getOrder()) : null);
					performLocationUpdateOnSerial(blInventoryLocationModel, serialList, serialProductModel);					
				}
			}
		}
		return serialList;
	}

	/**
	 * Perform location update on serial.
	 *
	 * @param blInventoryLocationModel the bl inventory location model
	 * @param serialList the serial list
	 * @param serialProductModel the serial product model
	 */
	private void performLocationUpdateOnSerial(final BlInventoryLocationModel blInventoryLocationModel, final Collection<String> serialList,
			final BlSerialProductModel serialProductModel)
	{
		if(Objects.nonNull(serialProductModel.getProductType()) && serialProductModel.getProductType().equals(ProductTypeEnum.SUBPARTS))
		{
			serialProductModel.setSerialStatus(SerialStatusEnum.IN_HOUSE);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing Serial status to In-House for sub part with code : {}", serialProductModel.getCode());
			modelService.save(serialProductModel);
			modelService.refresh(serialProductModel);
		}
		else
		{
			checkInventoryLocationForDCOrDPC(serialProductModel, isLocationDP(), blInventoryLocationModel, serialList);
		}
	}

	/**
	 * This method will check Inventory location for DC or DPC
	 *
	 * @param blSerialProductModel
	 *           serial
	 * @param isLocationDPC
	 *           true/false
	 * @param blInventoryLocationLocal
	 *           location
	 * @param serialList
	 *           failedSerialList
	 */
	public void checkInventoryLocationForDCOrDPC(final BlSerialProductModel blSerialProductModel, final boolean isLocationDPC,
			final BlInventoryLocationModel blInventoryLocationLocal, final Collection<String> serialList)
	{
		if (doCheckDirtyPriorityStatus(blSerialProductModel))
		{
			if (isLocationDPC)
			{
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			}
			else
			{
				serialList.add(blSerialProductModel.getBarcode());
			}
		}
		else
		{
			if (isLocationDPC)
			{
				serialList.add(blSerialProductModel.getBarcode());
			}
			else
			{
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean doCheckDirtyPriorityStatus(final BlSerialProductModel serialProductModel)
	{
		this.checkItemIsDirty(serialProductModel);
		return serialProductModel.isDirtyPriorityStatus();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean getStatusOfLocationDP()
	{
		return isLocationDP();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<ConsignmentModel> getTodaysShippingOrders()
	{
		return getBlInventoryScanToolDao().getTodaysShippingOrders();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<ConsignmentModel> getAllConsignmentForSerial(final String serial)
	{
		return getBlInventoryScanToolDao().getAllConsignmentForSerial(serial);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void flagAllDirtyPrioritySerialsOfConsignment()
	{
		final Collection<ConsignmentModel> todaysShippingOrders = this.getTodaysShippingOrders();
		if (CollectionUtils.isNotEmpty(todaysShippingOrders))
		{
			for (final ConsignmentModel consignment : todaysShippingOrders)
			{
				this.flagAllDirtyPrioritySerialsOfNewOrder(consignment);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void flagAllDirtyPrioritySerialsOfNewOrder(final ConsignmentModel consignment)
	{
		final Collection<ConsignmentEntryModel> consignmentEntryModels = consignment.getConsignmentEntries();
		if (CollectionUtils.isNotEmpty(consignmentEntryModels))
		{
			for (final ConsignmentEntryModel consignmentEntry : consignmentEntryModels)
			{
				consignmentEntry.getSerialProducts().forEach(this::checkSerialsForDP);
			}
		}
	}

	/**
	 * This method will check products from Order and will check for DP/DC
	 *
	 * @param serial
	 *           product
	 */
	private void checkSerialsForDP(final BlProductModel serial)
	{
		if (serial instanceof BlSerialProductModel)
		{
			final BlSerialProductModel serialProductModel = ((BlSerialProductModel) serial);
			if (BooleanUtils.isFalse(serialProductModel.isDirtyPriorityStatus()))
			{
				this.checkSerialForDirtyPriority(serialProductModel);
			}
		}
	}

	/**
	 * This method will check serial for dirty Priority and will set status on serial and save it.
	 *
	 * @param serialProductModel
	 *           product
	 */
	private void checkSerialForDirtyPriority(final BlSerialProductModel serialProductModel)
	{
		final Collection<ConsignmentModel> allConsignmentForSerial = this.getAllConsignmentForSerial(serialProductModel.getCode());
		markDirtyToSerial(serialProductModel, allConsignmentForSerial);
	}
	
	/**
	 * Check item is dirty.
	 *
	 * @param serialProductModel the serial product model
	 */
	private void checkItemIsDirty(final BlSerialProductModel serialProductModel)
	{
		final Collection<ConsignmentModel> allConsignmentForSerial = getBlInventoryScanToolDao().getTodaysShippingConsignments(serialProductModel.getCode());
		markDirtyToSerial(serialProductModel, allConsignmentForSerial);
	}
	
	/**
	 * Mark dirty to serial.
	 *
	 * @param serialProductModel the serial product model
	 * @param allConsignmentForSerial the all consignment for serial
	 */
	private void markDirtyToSerial(final BlSerialProductModel serialProductModel,
			final Collection<ConsignmentModel> allConsignmentForSerial)
	{
		if (CollectionUtils.isEmpty(allConsignmentForSerial))
		{
			serialProductModel.setDirtyPriorityStatus(Boolean.FALSE);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Marking serial : {} as Dirty Priority: FALSE", serialProductModel.getCode());
		}
		else
		{
			serialProductModel.setDirtyPriorityStatus(Boolean.TRUE);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Marking serial : {} as Dirty Priority: TRUE", serialProductModel.getCode());
		}
		modelService.save(serialProductModel);
		modelService.refresh(serialProductModel);
	}

	/**
	 * This method will change status of consignment
	 *
	 * @param consignmentModel
	 *           consignment
	 * @param consignmentStatus
	 *           status
	 */
	public void changeConsignmentStatus(final ConsignmentModel consignmentModel, final ConsignmentStatus consignmentStatus)
	{
		consignmentModel.setStatus(consignmentStatus);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing status of Consignment : {} to {}", 
				consignmentModel.getCode(), consignmentStatus.getCode());
		modelService.save(consignmentModel);
		modelService.refresh(consignmentModel);
	}

	/**
	 * This method will change status of packagingInfo
	 *
	 * @param packagingInfoModel
	 *           package
	 * @param packagingInfoStatus
	 *           status
	 */
	public void changePackagingInfoStatus(final PackagingInfoModel packagingInfoModel,
			final PackagingInfoStatus packagingInfoStatus)
	{
		packagingInfoModel.setPackagingInfoStatus(packagingInfoStatus);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing status of Packaging with PK : {} to {}", 
				packagingInfoModel.getPk().toString(), packagingInfoStatus.getCode());
		modelService.save(packagingInfoModel);
		modelService.refresh(packagingInfoModel);
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
	 * @return the isLocationDP
	 */
	public boolean isLocationDP()
	{
		return isLocationDP;
	}

	/**
	 * @param isLocationDP the isLocationDP to set
	 */
	public void setLocationDP(boolean isLocationDP)
	{
		this.isLocationDP = isLocationDP;
	}
}
