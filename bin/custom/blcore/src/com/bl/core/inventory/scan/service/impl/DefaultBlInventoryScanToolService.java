package com.bl.core.inventory.scan.service.impl;

import com.bl.core.stock.BlStockService;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.ordersplitting.WarehouseService;
import de.hybris.platform.ordersplitting.model.ConsignmentEntryModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;
import de.hybris.platform.ordersplitting.model.StockLevelModel;
import de.hybris.platform.ordersplitting.model.WarehouseModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.warehousing.model.PackagingInfoModel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.ConsignmentEntryStatusEnum;
import com.bl.core.enums.ItemStatusEnum;
import com.bl.core.enums.PackagingInfoStatus;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.esp.service.BlESPEventService;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryLocationScanHistoryModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
import com.bl.core.services.order.BlOrderService;
import com.bl.core.stock.BlStockLevelDao;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlInventoryScanUtility;
import com.bl.logging.BlLogger;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

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

 	@Resource(name = "blStockLevelDao")
 	private BlStockLevelDao blStockLevelDao;

	private BlInventoryLocationModel blInventoryLocation;

	private PackagingInfoModel packagingInfoModel;
	private boolean isLocationDP;

	@Resource(name = "blOrderService")
   private BlOrderService blOrderService;

	@Resource(name = "productService")
	private BlProductService blProductService;

	@Resource(name = "warehouseService")
	private WarehouseService warehouseService;

	@Resource(name = "blEspEventService")
	private BlESPEventService  blESPEventService;

	@Resource(name = "blStockService")
	private BlStockService blStockService;

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
    public Collection<BlSerialProductModel> getSerialProductsByBarcode(final Collection<String> barcode, final String version) {
		 return CollectionUtils.isNotEmpty(barcode) && StringUtils.isNotBlank(version)
				 ? getBlInventoryScanToolDao().getSerialsByBarcodesAndVersion(barcode, version)
				 : Lists.newArrayList();
    }

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int checkValidLocationInBarcodeListForBin(final List<String> barcodes, final List<String> memberAllowedLocationList) {
		final List<String> defaultLocations = BlInventoryScanLoggingConstants.getDefaultBinInventoryLocations();
		final List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream()
				.anyMatch(b::startsWith)).collect(Collectors.toList());
		return checkValidInventoryLocation(barcodes.get(0), filteredLocationList, memberAllowedLocationList);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean checkLastBarcodeIsLocationOrNot(final List<String> barcodes, final String maxSequenceScan, final boolean status) {
		final String lastScanBarcode = barcodes.get(barcodes.size() - 1);
		if(barcodes.size() == Integer.parseInt(maxSequenceScan) || status) {
			final List<String> defaultLocations = BlInventoryScanLoggingConstants.getDefaultInventoryLocation();
			return defaultLocations.stream().anyMatch(lastScanBarcode::startsWith);
		} else {
			return true;
		}
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
            if(Objects.isNull(blLocalInventoryLocation)) {
            	BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR_FAILURE_MSG);
               return BlInventoryScanLoggingConstants.TWO;
            }
            else if (isLocationValidForMember(memberAllowedLocationList, blLocalInventoryLocation)) {
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
				&& (memberAllowedLocationList.contains(BlInventoryScanLoggingConstants.ALLOW_SCAN) || memberAllowedLocationList.contains(
				blLocalInventoryLocation.getLocationCategory().getCode()));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<String> getFailedBarcodeList(final List<String> barcodes) {
		long startTime = System.nanoTime();
		final List<String> failedBarcodeList = new ArrayList<>();
		final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
		final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
		if(CollectionUtils.isEmpty(blSerialProducts))
		{
			failedBarcodeList.addAll(subList);
			return failedBarcodeList;
		}
		subList.forEach(barcode -> setInventoryLocationOnSerial(failedBarcodeList, blSerialProducts, barcode));
		long stopTime = System.nanoTime();
		if(LOG.isDebugEnabled()) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "getFailedBarcodeList method with barcodes {} took {} time to execute ", barcodes, stopTime - startTime);
		}
		return failedBarcodeList;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<String> getFailedBarcodeListForBin(final List<String> barcodes) {
		final List<String> failedBarcodeList = new ArrayList<>();
		final BlInventoryLocationModel existingBlInventoryLocation = getBlInventoryLocation();
		final int noOfSize = checkValidLocationInBarcodeListForBin(barcodes, Lists.newArrayList("ALLOW_SCAN"));
		if (noOfSize == 1) {
			final BlInventoryLocationModel newBlInventoryLocation = modelService.create(BlInventoryLocationModel.class);
			newBlInventoryLocation.setCode(existingBlInventoryLocation.getCode());
			newBlInventoryLocation.setParentInventoryLocation(existingBlInventoryLocation);
			modelService.save(newBlInventoryLocation);
			modelService.refresh(newBlInventoryLocation);
		} else {
			failedBarcodeList.addAll(barcodes);
			BlLogger.logMessage(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.FAILED_BARCODE_LIST + failedBarcodeList);
		}
		return failedBarcodeList;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getConfigKeyFromScanConfiguration(final String key) {
		final BlInventoryScanConfigurationModel blInventoryScanConfigurationModel = getBlInventoryScanToolDao().getConfigKeyFromScanConfiguration(key);
		return blInventoryScanConfigurationModel != null ? blInventoryScanConfigurationModel.getBlScanConfigValue() :
				String.valueOf(BlInventoryScanLoggingConstants.ELEVEN);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int doBINScanFromWebScanTool(final List<String> barcodeList) {
		final int totalBarcode = barcodeList.size();
		if (totalBarcode <= BlInventoryScanLoggingConstants.TWO) {
			final String inventoryLocation = barcodeList.get(BlInventoryScanLoggingConstants.ONE);
			if (StringUtils.isNotEmpty(inventoryLocation) && (!inventoryLocation.startsWith(BlInventoryScanLoggingConstants.BIN))) {
				return checkBarcodeINBINLocation(barcodeList, inventoryLocation, BlInventoryScanLoggingConstants.getDefaultInventoryLocation());
			} else {
				return BlInventoryScanLoggingConstants.THREE; //bin can be assign to parent only VALID_PARENT_LOCATION_ERROR_FAILURE_MSG
			}
		} else {
			return BlInventoryScanLoggingConstants.FIVE; //max size limit for BIN MAX_BARCODE_LIMIT_ERROR_FAILURE_MSG
		}
	}

	/**
	 * javadoc
	 * This method will check BIN and last location is valid or not
	 *
	 * @param barcodeList       scannedList
	 * @param inventoryLocation last scan
	 * @param defaultLocations  all locations
	 * @return status in int
	 */
	private int checkBarcodeINBINLocation(final List<String> barcodeList, final String inventoryLocation, final List<String> defaultLocations) {
		if (CollectionUtils.isNotEmpty(defaultLocations) && (defaultLocations.stream().anyMatch(inventoryLocation::startsWith))) {
			final BlInventoryLocationModel blLocalInventoryLocation = getBlInventoryScanToolDao().getInventoryLocationById(inventoryLocation);
			if (blLocalInventoryLocation != null) {
				return storeParentOnBINLocation(barcodeList, blLocalInventoryLocation);
			} else {
				return BlInventoryScanLoggingConstants.THREE; //enter a valid parent location VALID_PARENT_LOCATION_ERROR_FAILURE_MSG
			}
		} else {
			return BlInventoryScanLoggingConstants.FOUR; //last scan must be a location LAST_SCAN_ERROR_FAILURE_MSG
		}
	}

	/**
	 * javadoc
	 * This method will check first Location is valid or not and if valid then will update parent location
	 *
	 * @param barcodeList              scannedList
	 * @param blLocalInventoryLocation location
	 * @return status in int
	 */
	private int storeParentOnBINLocation(final List<String> barcodeList, final BlInventoryLocationModel blLocalInventoryLocation) {
		final BlInventoryLocationModel blBINInventoryLocationModel = getBlInventoryScanToolDao().getInventoryLocationById(
				barcodeList.get(BlInventoryScanLoggingConstants.ZERO));
		if (blBINInventoryLocationModel != null) {
			blBINInventoryLocationModel.setParentInventoryLocation(blLocalInventoryLocation);
			updateParentOnSerialsOfThisBIN(blBINInventoryLocationModel, blLocalInventoryLocation);
			modelService.save(blBINInventoryLocationModel);
			modelService.refresh(blBINInventoryLocationModel);
			return BlInventoryScanLoggingConstants.ONE; //successful scan SCAN_BARCODE_SUCCESS_MSG
		} else {
			return BlInventoryScanLoggingConstants.TWO; //enter valid BIN location VALID_BIN_LOCATION_ERROR_FAILURE_MSG
		}
	}

	/**
	 * Update parent location on all serials of the bin.
	 *
	 * @param blBINInventoryLocationModel the blBINInventoryLocationModel
	 * @param blLocalInventoryLocation the parent location
	 */
	private void updateParentOnSerialsOfThisBIN(
			final BlInventoryLocationModel blBINInventoryLocationModel,
			final BlInventoryLocationModel blLocalInventoryLocation) {

		long startTime = System.nanoTime();
		final Collection<BlSerialProductModel> serialProductModels = getBlInventoryScanToolDao()
				.getAllSerialsByBinLocation(blBINInventoryLocationModel.getCode());

		if (CollectionUtils.isNotEmpty(serialProductModels)) {
			serialProductModels.stream().forEach(serial -> {
				serial.setLastLocationScanParent(blLocalInventoryLocation.getCode());

				//				if (!serial.getProductType().getCode().equals(ProductTypeEnum.SUBPARTS.getCode()))
				//				{
				//					String warehouseCode = null;
				//					try
				//					{
				//						if (blBINInventoryLocationModel.getCode().startsWith(BlInventoryScanLoggingConstants.BIN)
				//								&& blBINInventoryLocationModel.getInventoryType().getCode().equals(BlInventoryScanLoggingConstants.BIN))
				//						{
				//							WarehouseModel wareHouse = null;
				//							warehouseCode = BlInventoryScanLoggingConstants.WAREHOUSE
				//									+ blLocalInventoryLocation.getCode().substring(0, 2).toLowerCase();
				//							wareHouse = warehouseService.getWarehouseForCode(warehouseCode);
				//							serial.setWarehouseLocation(wareHouse);
				//						}
				//					}
				//					catch (final Exception ex)
				//					{
				//						BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, ex, "Unable to find the warehouse - {}",
				//								warehouseCode);
				//					}
				//				}

				modelService.save(serial);
				modelService.refresh(serial);
				if(LOG.isDebugEnabled()) {
					BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
							"Parent Location {} updated for the serial with code : {}",
							blLocalInventoryLocation.getCode(), serial.getCode());
				}
			});
		}
		long stopTime = System.nanoTime();
		if(LOG.isDebugEnabled()) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "updateParentOnSerialsOfThisBIN method with location {} took {} time to execute", blLocalInventoryLocation.getCode(), stopTime - startTime);
		}
	}

	/**
 	 * Update location on item.
	 *
	 * @param blSerialProduct the bl serial product
 	 * @param blInventoryLocationLocal the bl inventory location local
 	 */
	private void updateLocationOnItem(final BlSerialProductModel blSerialProduct, final BlInventoryLocationModel blInventoryLocationLocal,
									  final boolean unboxStatus) {
		if(unboxStatus) {
			blSerialProduct.setSerialStatus(SerialStatusEnum.UNBOXED);
			updateConsignmentEntryStatus(blSerialProduct);
		}

		if (!blSerialProduct.getProductType().getCode().equals(ProductTypeEnum.SUBPARTS.getCode()))
		{
			updateWarehouseLocation(blSerialProduct, blInventoryLocationLocal);
		}

		updateSerialProductDetails(blSerialProduct, blInventoryLocationLocal);
		this.updateLocationOnItemForStaged(blSerialProduct, unboxStatus, blInventoryLocationLocal);

		/* Scan History Entry */
		setBlLocationScanHistory(blSerialProduct, unboxStatus, blInventoryLocationLocal);
	}

	private void updateConsignmentEntryStatus(final BlSerialProductModel blSerialProduct) {
		try {
			// Updating the consignment entry status
			final Map<String, ConsignmentEntryStatusEnum> consEntryStatus =
					blSerialProduct.getConsignmentEntry().getConsignmentEntryStatus();
			consEntryStatus.put(blSerialProduct.getCode(), ConsignmentEntryStatusEnum.UNBOXED);
			if (!consEntryStatus.isEmpty()) {
				blSerialProduct.getConsignmentEntry().setConsignmentEntryStatus(consEntryStatus);
			}
		} catch (final Exception exception) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Unable to update consignment entry status to Unboxed for - - {}", blSerialProduct.getCode());
		}
	}

	private void updateWarehouseLocation(
			final BlSerialProductModel blSerialProduct,
			final BlInventoryLocationModel blInventoryLocationLocal) {
		String warehouseCode = null;
		WarehouseModel warehouse = null;

		try {
			// As we don't have a warehouse attribute in inventory, splitting from the location code
			if (blInventoryLocationLocal.getCode() != null) {
				if (!(blInventoryLocationLocal.getCode().startsWith(BlInventoryScanLoggingConstants.BIN)
						&& blInventoryLocationLocal.getInventoryType().getCode()
						.equals(BlInventoryScanLoggingConstants.BIN))) {
					warehouseCode = BlInventoryScanLoggingConstants.WAREHOUSE
							+ blInventoryLocationLocal.getCode().substring(0, 2).toLowerCase();
				} else {
					warehouseCode = BlInventoryScanLoggingConstants.WAREHOUSE
							+ (blInventoryLocationLocal.getParentInventoryLocation() != null
							? blInventoryLocationLocal.getParentInventoryLocation().getCode().substring(0, 2).toLowerCase()
							: null);
				}

				warehouse = warehouseService.getWarehouseForCode(warehouseCode);

				if (warehouse != null
						&& !blSerialProduct.getWarehouseLocation().getCode().equals(warehouse.getCode())) {
					blSerialProduct.setWarehouseLocation(warehouse);
				}
			}
		} catch (final Exception exception) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
					"Unable to find the warehouse - {}", warehouseCode);
		}
	}

	private void updateSerialProductDetails(
			final BlSerialProductModel blSerialProduct,
			final BlInventoryLocationModel blInventoryLocationLocal) {
		blSerialProduct.setOcLocation(blInventoryLocationLocal.getCode());
		blSerialProduct.setLastLocationScanParent(blInventoryLocationLocal.getParentInventoryLocation() != null
				? blInventoryLocationLocal.getParentInventoryLocation().getCode() : null);
		blSerialProduct.setOcLocationDetails(blInventoryLocationLocal);
		blSerialProduct.setInventoryLocationID(blInventoryLocationLocal.getInventoryLocationID());
		//modelService.save(blSerialProduct);
		//modelService.refresh(blSerialProduct);
	}
	/**
	 * Update location on item
	 * @param blSerialProduct the bl serial product
	 * @param unboxStatus status
	 * @param blInventoryLocationLocal the bl inventory location local
	 */
	private void updateLocationOnItemForStaged(final BlSerialProductModel blSerialProduct, final boolean unboxStatus,
											   final BlInventoryLocationModel blInventoryLocationLocal) {
		final BlSerialProductModel serialStagedProductModel = this.getBlInventoryScanToolDao().getSerialProductByBarcode(
				blSerialProduct.getCode());
		if(null != serialStagedProductModel) {
			if (unboxStatus) {
				serialStagedProductModel.setSerialStatus(SerialStatusEnum.UNBOXED);
			}
			serialStagedProductModel.setOcLocation(blInventoryLocationLocal.getCode());
			serialStagedProductModel.setLastLocationScanParent(blInventoryLocationLocal.getParentInventoryLocation() != null
					? blInventoryLocationLocal.getParentInventoryLocation().getCode() : null);
			serialStagedProductModel.setOcLocationDetails(blInventoryLocationLocal);
			serialStagedProductModel.setInventoryLocationID(blInventoryLocationLocal.getInventoryLocationID());
			modelService.save(serialStagedProductModel);
			modelService.refresh(serialStagedProductModel);
		}
	}

	/**
	 * Sets the Location scan history.
	 *
	 * @param blSerialProduct
	 *           the new bl location scan history
	 */
	public void setBlLocationScanHistory(final BlSerialProductModel blSerialProduct, final boolean unboxStatus,
			final BlInventoryLocationModel inventoryLocation)
	{
		final BlInventoryLocationScanHistoryModel blInventoryLocationScanHistory = modelService
				.create(BlInventoryLocationScanHistoryModel.class);
		blInventoryLocationScanHistory.setSerialProduct(blSerialProduct);
		blInventoryLocationScanHistory.setSerialId(blSerialProduct.getProductId());
		blInventoryLocationScanHistory.setSerialBarcode(blSerialProduct.getBarcode());
		blInventoryLocationScanHistory.setOcParent(getParentLocationCode(inventoryLocation));
		blInventoryLocationScanHistory.setScanUser(userService.getCurrentUser());
		blInventoryLocationScanHistory.setBlInventoryLocation(blInventoryLocation);
		blInventoryLocationScanHistory.setScanTime(new Date());
		blInventoryLocationScanHistory.setUnboxedHistory(unboxStatus);
		modelService.save(blInventoryLocationScanHistory);
		modelService.refresh(blInventoryLocationScanHistory);

		if(LOG.isDebugEnabled()) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Location Scan History creation is completed with pk {} for unboxing flow with unbox status {} for serial Product {} ",
					blInventoryLocationScanHistory.getPk(), unboxStatus, blSerialProduct.getCode());
		}
		if(unboxStatus)
		{
			setLastOcLocationHistoryOnSerial(blSerialProduct, blInventoryLocationScanHistory);
		}
	}


	// for package scan, we need to add tracking number as location here
	public void setBlLocationScanHistoryForPackageScan(final BlSerialProductModel blSerialProduct, final boolean unboxStatus,
			final BlInventoryLocationModel trackingNumberLocation)
	{
		final BlInventoryLocationScanHistoryModel blInventoryLocationScanHistory = modelService

				.create(BlInventoryLocationScanHistoryModel.class);
		blInventoryLocationScanHistory.setSerialProduct(blSerialProduct);
		blInventoryLocationScanHistory.setSerialId(blSerialProduct.getProductId());
		blInventoryLocationScanHistory.setSerialBarcode(blSerialProduct.getBarcode());
		//blInventoryLocationScanHistory.setOcParent(trackingNumberLocation);
		blInventoryLocationScanHistory.setScanUser(userService.getCurrentUser());
		blInventoryLocationScanHistory.setBlInventoryLocation(trackingNumberLocation);
		blInventoryLocationScanHistory.setScanTime(new Date());
		blInventoryLocationScanHistory.setUnboxedHistory(unboxStatus);
		modelService.save(blInventoryLocationScanHistory);
		modelService.refresh(blInventoryLocationScanHistory);
		if(LOG.isDebugEnabled()) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Location Scan History creation is completed with pk {} for package scan flow with unbox status {}: ",
					blInventoryLocationScanHistory.getPk(), unboxStatus);
		}
		if (unboxStatus)
		{
			setLastOcLocationHistoryOnSerial(blSerialProduct, blInventoryLocationScanHistory);
		}
	}

	/**
	 * Gets the parent location code from child location.
	 *
	 * @param locationModel the location model
	 * @return the parent location code
	 */
	private String getParentLocationCode(final BlInventoryLocationModel locationModel)
	{
		if(Objects.nonNull(locationModel))
		{
			return Objects.isNull(locationModel.getParentInventoryLocation()) ? StringUtils.EMPTY :
					locationModel.getParentInventoryLocation().getCode();
		}
		else if(Objects.nonNull(getBlInventoryLocation()))
		{
			return Objects.isNull(getBlInventoryLocation().getParentInventoryLocation()) ? StringUtils.EMPTY :
					getBlInventoryLocation().getParentInventoryLocation().getCode();
		}
		return StringUtils.EMPTY;
	}

	/**
	 * Sets the last oc location history on serial.
	 *
	 * @param blSerialProduct
	 *           the bl serial product
	 * @param blInventoryLocationScanHistory
	 *           the bl inventory location scan history
	 */
	private void setLastOcLocationHistoryOnSerial(final BlSerialProductModel blSerialProduct,
												  final BlInventoryLocationScanHistoryModel blInventoryLocationScanHistory)
	{
		blSerialProduct.setLastUnboxedOcLocationHistory(blInventoryLocationScanHistory);
		blSerialProduct.setLastUnboxedOcLocationDate(blInventoryLocationScanHistory.getCreationtime());
		modelService.save(blSerialProduct);
		modelService.refresh(blSerialProduct);
		//BLS-533
		Set<String> productCode = new HashSet<>();
		productCode.add(blSerialProduct.getCode());
		final ConsignmentModel consignmentModel = blSerialProduct.getAssociatedConsignment();
		getBlStockService().releaseStockForGivenSerial(productCode, new Date(), consignmentModel.getOptimizedShippingEndDate());

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int isValidTechEngLocationBarcode(final List<String> barcodes, final List<String> memberAllowedLocationList)
	{
		return checkLocationWithType(barcodes, BlInventoryScanUtility.getDefaultTechEngLocation(), memberAllowedLocationList);
	}

	/**
	 * {@inheritDoc}
	 */
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
	 * {@inheritDoc}
	 */
	@Override
	public int checkLocationWithTypeForFD(final List<String> barcodes, final List<String> defaultLocations,
									 final List<String> memberAllowedLocationList)
	{
		final List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream().anyMatch(b::contains))
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
	public Map<Integer, List<String>> getFailedBinBarcodeList(final List<String> barcodes)
	{
		return getFailedBinBarcodeList(barcodes, Collections.emptyList());
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<Integer, List<String>> getFailedBinBarcodeList(final List<String> barcodes, final List<String> allowedLocationList)
	{
		final String subList = barcodes.get(0);
		final BlInventoryLocationModel blWorkingDeskInventory = getBlInventoryLocation();
		final int result = checkBinLocationWithType(subList, BlInventoryScanUtility.getDefaultBinLocation(),
				BlInventoryScanUtility.getShippingAllowedLocations());

		if (result == BlInventoryScanLoggingConstants.ONE)
		{
			final BlInventoryLocationModel blBinLocationModel = getBlInventoryLocation();
			blBinLocationModel.setParentInventoryLocation(blWorkingDeskInventory);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Inventory location updated for {} with parent location {} ",
					blWorkingDeskInventory.getCode(), blWorkingDeskInventory.getParentInventoryLocation());
			modelService.save(blBinLocationModel);
			modelService.refresh(blBinLocationModel);
			BlLogger.logMessage(LOG, Level.DEBUG, "Inventory Location has been updated");
			return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.ZERO, Collections.emptyList()));
		}
		else
		{
			return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.ONE, barcodes));
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removeSerialsAndParentLocationFromBinOcLocation(final BlInventoryLocationModel blInventoryLocationModel) {
		final Collection<BlSerialProductModel> serialProductModels = getBlInventoryScanToolDao().getAllSerialsByBinLocation(blInventoryLocationModel.getCode());
		final BlInventoryLocationModel parentInventoryLocation = blInventoryLocationModel.getParentInventoryLocation();
		if (CollectionUtils.isNotEmpty(serialProductModels)) {
				serialProductModels.stream().forEach(serial -> {
					if(StringUtils.isNotBlank(serial.getOcLocation()) && serial.getOcLocation().equals(blInventoryLocationModel.getCode())){
						serial.setOcLocation(null);
						serial.setOcLocationDetails(null);
						modelService.save(serial);
						modelService.refresh(serial);
						BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Bin Location removed from the serial with code : {}", serial.getCode());
					}
				});
			}

		if (parentInventoryLocation != null) {
				blInventoryLocationModel.setParentInventoryLocation(null);
				modelService.save(blInventoryLocationModel);
				modelService.refresh(blInventoryLocationModel);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Parent Location with code removed from bin location: {}", parentInventoryLocation.getCode());
			}
		}

	/**
	 * This method is used to check bin location with type
	 *
	 * @param barcodes as barcodes
	 * @param defaultLocations as default location
	 * @param memberAllowedLocationList as member allowed
	 * @return int
	 */
	public int checkBinLocationWithType(final String barcodes, final List<String> defaultLocations,
										final List<String> memberAllowedLocationList)
	{
		final List<String> filteredLocationList = new ArrayList<>();
		for (final String binLocation : defaultLocations)
		{
			if (barcodes.startsWith(binLocation))
			{
				filteredLocationList.add(barcodes);
				BlLogger.logFormattedMessage(LOG, Level.DEBUG, "Barcodes added to list", barcodes);
			}
		}

		return checkValidInventoryLocation(barcodes, filteredLocationList, memberAllowedLocationList);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<Integer, List<String>> getFailedPackageBarcodeList(final List<String> barcodes)
	{
		final List<String> subList = new ArrayList<>(barcodes.subList(0, barcodes.size() - BlInventoryScanLoggingConstants.ONE));
		final Collection<BlSerialProductModel> scannedSerialProduct = getBlInventoryScanToolDao()
				.getSerialProductsByBarcode(subList);

		if (scannedSerialProduct.size() != subList.size())
		{
			if (StringUtils.isEmpty(subList.get(0)))
			{
				return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.FOUR, Collections.emptyList()));
			}

			final List<String> collect = scannedSerialProduct.stream().map(BlSerialProductModel::getBarcode)
					.collect(Collectors.toList());
			for (final String scannedProduct : collect)
			{
				if (subList.contains(scannedProduct))
				{
					subList.remove(scannedProduct);
				}
			}
			return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.ONE, subList));
		}
		final List<BlProductModel> serialProductsOnPackage = getPackagingInfoModel().getSerialProducts();

		if (serialProductsOnPackage.containsAll(scannedSerialProduct) && scannedSerialProduct.containsAll(serialProductsOnPackage))
		{
			BlLogger.logMessage(LOG, Level.DEBUG, "Iterate over serial product on package");
			serialProductsOnPackage.forEach(serial -> {
				if (serial instanceof BlSerialProductModel)
				{
					((BlSerialProductModel) serial).setOcLocation(getPackagingInfoModel().getOutBoundTrackingNumber()); // NOSONAR
					modelService.save(serial);
					BlLogger.logFormattedMessage(LOG, Level.DEBUG, "OC location updated to Tracking number {} for serial {}",
							getPackagingInfoModel().getOutBoundTrackingNumber(), serial.getCode());
				}
			});
			return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.ZERO, Collections.emptyList()));
		}
		else if (serialProductsOnPackage.size() > scannedSerialProduct.size())
		{
			return getMissingSerialOnScan(scannedSerialProduct, serialProductsOnPackage);

		}

		else if (scannedSerialProduct.size() > serialProductsOnPackage.size())
		{
			return getMissingSerialOnPackage(scannedSerialProduct, serialProductsOnPackage);
		}
		return null;
	}

	/**
	 * method will be used to get the serial which is missing on package
	 * @param scannedSerialProduct
	 * @param serialProductsOnPackage
	 * @return
	 */
	private Map<Integer, List<String>> getMissingSerialOnPackage(final Collection<BlSerialProductModel> scannedSerialProduct,
			final List<BlProductModel> serialProductsOnPackage)
	{
		final List<BlProductModel> serialProductList = new ArrayList<>();
		final List<String> errorList = new ArrayList<>();

		for (final BlProductModel blSerialProduct : scannedSerialProduct)
		{
			serialProductList.add(blSerialProduct);
		}
		serialProductList.removeIf(serialProductsOnPackage::contains);

		serialProductList.forEach(serialProduct -> {

			if (serialProduct instanceof BlSerialProductModel)
			{
				final BlSerialProductModel blSerialProduct = (BlSerialProductModel) serialProduct;
				errorList.add((BlInventoryScanLoggingConstants.ITEM_TEXT).concat(blSerialProduct.getBarcode())
						.concat(BlInventoryScanLoggingConstants.PRODUCT_TEXT).concat(blSerialProduct.getBlProduct().getName()));
			}
		});

		return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.THREE, errorList));
	}

	/**
	 * method will be used to get the serial which is missing on scan
	 * @param scannedSerialProduct
	 * @param serialProductsOnPackage
	 * @return
	 */
	private Map<Integer, List<String>> getMissingSerialOnScan(final Collection<BlSerialProductModel> scannedSerialProduct,
			final List<BlProductModel> serialProductsOnPackage)
	{
		final List<BlProductModel> serialProductList = new ArrayList<>();
		final List<String> errorList = new ArrayList<>();

		for (final BlProductModel blSerialProduct : serialProductsOnPackage)
		{
			serialProductList.add(blSerialProduct);
		}
		serialProductList.removeIf(scannedSerialProduct::contains);

		serialProductList.forEach(serialProduct -> {

			if (serialProduct instanceof BlSerialProductModel)
			{
				final BlSerialProductModel blSerialProduct = (BlSerialProductModel) serialProduct;
				errorList.add((BlInventoryScanLoggingConstants.ITEM_TEXT)
						.concat(blSerialProduct.getBarcode().concat(BlInventoryScanLoggingConstants.PRODUCT_TEXT))
						.concat(blSerialProduct.getBlProduct().getName()));
			}
		});

		return Maps.newHashMap(ImmutableMap.of(BlInventoryScanLoggingConstants.TWO, errorList));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean checkBINOrSerialScan(final List<String> barcodeList) {
		if (CollectionUtils.isNotEmpty(barcodeList)) {
			return barcodeList.get(BlInventoryScanLoggingConstants.ZERO).startsWith(BlInventoryScanLoggingConstants.BIN) ?
					Boolean.TRUE : Boolean.FALSE;
		}
		return Boolean.FALSE;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, List<BlProductModel>> verifyShippingScan(final List<String> barcodes, final ConsignmentModel selectedConsignment) {
		final Collection<BlSerialProductModel> blScannedProduct = getBlInventoryScanToolDao().getSerialProductsByBarcode(barcodes);

		final List<BlProductModel> filteredSerialProduct = new ArrayList<>();
		final List<BlProductModel> filteredSubPartProduct = new ArrayList<>();

		final List<BlSerialProductModel> scannedSerialProduct = new ArrayList<>();
		final List<BlSerialProductModel> scannedSubpartProduct = new ArrayList<>();

      final BlInventoryLocationModel blLocalInventoryLocation = getBlInventoryScanToolDao().getInventoryLocationById(barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE));

		//Update OC location in BlInventoryLocationScanHistoryModel
			getScannedSerial(blScannedProduct, scannedSerialProduct, scannedSubpartProduct, blLocalInventoryLocation);


		return validateConsignmentEntry(barcodes, selectedConsignment, filteredSerialProduct, filteredSubPartProduct,
				scannedSerialProduct, scannedSubpartProduct);

	}

	/**
	 * This method is used to validate serial on consignment against scanned serial
	 *
	 * @param barcodes list
	 * @param selectedConsignment consignment
	 * @param filteredSerialProduct serial
	 * @param filteredSubPartProduct subpart
	 * @param scannedSerialProduct scanned serial
	 * @param scannedSubpartProduct scanned subpart
	 */
	private Map<String, List<BlProductModel>> validateConsignmentEntry(final List<String> barcodes,
			final ConsignmentModel selectedConsignment, final List<BlProductModel> filteredSerialProduct,
			final List<BlProductModel> filteredSubPartProduct, final List<BlSerialProductModel> scannedSerialProduct,
			final List<BlSerialProductModel> scannedSubpartProduct)
	{
		final List<BlProductModel> serials = new ArrayList<>();
		selectedConsignment.getConsignmentEntries().forEach(entry -> entry.getSerialProducts().forEach(blSerialProduct -> {

			if (blSerialProduct instanceof BlSerialProductModel)
			{
				serials.add(blSerialProduct);
			}
			else
			{
				blSerialProduct.getSerialProducts().forEach(serial -> {

					if (serial instanceof BlSerialProductModel && StringUtils.isNotBlank(serial.getBarcode()))
					{
						serials.add(serial);
					}
				});
			}
		}));

		final Map<String, List<BlProductModel>> missingSerial = validateBarcodedSerial(scannedSerialProduct,scannedSubpartProduct,serials,selectedConsignment);

		for (final ConsignmentEntryModel consignmentEntry : selectedConsignment.getConsignmentEntries())
		{
			doScan(filteredSerialProduct, filteredSubPartProduct, scannedSerialProduct, scannedSubpartProduct, consignmentEntry);
		}
		return missingSerial;
	}

	/**
	 * method is used to check scanned serials are valid on not i.e entered barcode serial belongs to
	 * given consignment and that are not already included serial.
	 **/
	private Map<String, List<BlProductModel>> validateBarcodedSerial(
			final List<BlSerialProductModel> scannedSerialProduct,
			final List<BlSerialProductModel> scannedSubpartProduct, final List<BlProductModel> serials,
			final ConsignmentModel selectedConsignment) {

		// collecting all serial for given barcode via shipping scan
		final List<BlProductModel> allEntryBarcodedSerials = new ArrayList<>(scannedSerialProduct);
		allEntryBarcodedSerials.addAll(scannedSubpartProduct);

		// collecting all the serials which not belongs to current consignment
		final List<BlProductModel> outsiderSerials = allEntryBarcodedSerials.stream()
				.filter(blSerialProductModel -> !serials.contains(blSerialProductModel))
				.collect(Collectors.toList());
		allEntryBarcodedSerials.removeAll(
				outsiderSerials); // remove all the serial which not belongs to current consignment.

		final List<BlProductModel> allIncludedEntry = new ArrayList<>();
		selectedConsignment.getConsignmentEntries().forEach(consEntry -> {
			final Map<String, ItemStatusEnum> items = consEntry.getItems();
			final List<BlProductModel> includedSerials = allEntryBarcodedSerials.stream().filter(
					blSerialProductModel -> (items.containsKey(blSerialProductModel.getCode()) && items
							.get(blSerialProductModel.getCode()).equals(ItemStatusEnum.INCLUDED)))
					.collect(Collectors.toList());
			allIncludedEntry.addAll(includedSerials);
			allEntryBarcodedSerials.removeAll(includedSerials);
		});

		final Map<String, List<BlProductModel>> invalidSerials = new HashMap<>();
		if (CollectionUtils.isNotEmpty(outsiderSerials)) {
			invalidSerials.put(BlInventoryScanLoggingConstants.OUTSIDER_BARCODE, outsiderSerials);
		}

		if (CollectionUtils.isNotEmpty(allIncludedEntry)) {
			invalidSerials.put(BlInventoryScanLoggingConstants.INCLUDED_SERIAL, allIncludedEntry);
		}

		if (CollectionUtils.isNotEmpty(allEntryBarcodedSerials)) {
			invalidSerials.put(BlInventoryScanLoggingConstants.SUCCESS_SERIAL, allEntryBarcodedSerials);
		}

		return invalidSerials;
	}


	/**
	 * method will be used to create success response
	 * @param barcodes
	 * @param scannedSerialProduct
	 * @param serials
	 * @param scannedSubpartProduct
	 * @param invalidSerials
	 */
	private void createSuccessResponse(final List<String> barcodes, final List<BlSerialProductModel> scannedSerialProduct,
			final List<BlProductModel> serials, final List<BlSerialProductModel> scannedSubpartProduct,
			final Map<String, List<BlProductModel>> invalidSerials)
	{
		if (barcodes.size() != (scannedSubpartProduct.size() + scannedSerialProduct.size()))
		{
			invalidSerials.put(BlInventoryScanLoggingConstants.MISSING_IN_CONSIGNMENT, serials);
		}
		else
		{
			invalidSerials.put(BlInventoryScanLoggingConstants.SUCCESS_SCAN, Collections.emptyList());
		}
	}

	/**
	 * method will be used to create error response
	 * @param invalidSerials
	 * @param entryBarcode
	 * @param newBarcode
	 * @param lErrorSubParts
	 */
	private void createErrorResponse(final Map<String, List<BlProductModel>> invalidSerials,
			final List<BlProductModel> entryBarcode, final List<BlProductModel> newBarcode,
			final List<BlProductModel> lErrorSubParts)
	{
		if (CollectionUtils.isNotEmpty(newBarcode))
		{
			invalidSerials.put(BlInventoryScanLoggingConstants.MISSING_IN_CONSIGNMENT, newBarcode);
		}
		if (CollectionUtils.isNotEmpty(entryBarcode))
		{
			invalidSerials.put(BlInventoryScanLoggingConstants.MISSING_IN_SCAN, entryBarcode);
		}
		if (CollectionUtils.isNotEmpty(lErrorSubParts))
		{
			invalidSerials.put(BlInventoryScanLoggingConstants.MISSING_SCAN_BARCODE, lErrorSubParts);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public int checkValidTrackingId(final String lastScannedItem) {
		final PackagingInfoModel packagingInfo = getBlInventoryScanToolDao().getPackageInfoByCode(lastScannedItem);
		if (packagingInfo != null) {
			setPackagingInfoModel(packagingInfo);
			return BlInventoryScanLoggingConstants.ONE;
		}
		return BlInventoryScanLoggingConstants.TWO;
	}

	/**
	 * This method is used to verify scan
	 * @param filteredSerialProduct serial
	 * @param filteredSubPartProduct subpart
	 * @param scannedSerialProduct scanned serial
	 * @param scannedSubpartProduct scanned subpart
	 * @param consignmentEntry entry
	 */
	private void doScan(final List<BlProductModel> filteredSerialProduct, final List<BlProductModel> filteredSubPartProduct,
						final List<BlSerialProductModel> scannedSerialProduct, final List<BlSerialProductModel> scannedSubpartProduct,
						final ConsignmentEntryModel consignmentEntry)
	{
		final Map<String, ItemStatusEnum> itemsMap = new HashMap<>(consignmentEntry.getItems());

		consignmentEntry.getSerialProducts()
				.forEach(serialItem -> getSerialFromConsignment(filteredSerialProduct, filteredSubPartProduct, serialItem));

		validateScannedSerial(scannedSerialProduct, itemsMap, consignmentEntry.getConsignment());

		final List<BlProductModel> serialProductsList = new ArrayList<>(consignmentEntry.getSerialProducts());
		validateScannedSubpart(scannedSubpartProduct, serialProductsList, itemsMap);

		consignmentEntry.setItems(itemsMap);
		consignmentEntry.setSerialProducts(serialProductsList);
		modelService.save(consignmentEntry);
		modelService.refresh(consignmentEntry);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Scan verified for consignment {}",
				consignmentEntry.getConsignment().getCode());
	}

	/**
	 * This method is used to validate scanned sub part product
	 * @param scannedSubpartProduct scanned subpart
	 * @param itemsMap item
	 * @param serialProductsList list
	 */
	private void validateScannedSubpart(final List<BlSerialProductModel> scannedSubpartProduct,
										final List<BlProductModel> serialProductsList, final Map<String, ItemStatusEnum> itemsMap)
	{
		scannedSubpartProduct.forEach(subpartProduct -> updateSubpartMap(serialProductsList, subpartProduct, itemsMap));
	}

	/**
	 * This method is used to validate scanned serial product
	 * @param scannedSerialProduct
	 * @param updatedItemMap
	 * @param consignment
	 */
	private void validateScannedSerial(final List<BlSerialProductModel> scannedSerialProduct,
			final Map<String, ItemStatusEnum> updatedItemMap, final ConsignmentModel consignment)
	{
		scannedSerialProduct.forEach(serialProduct -> updateSerialProductMap(updatedItemMap, serialProduct, consignment));
	}

	/**
	 * This method is used to update Item Map based on sub part scanned
	 * @param serialProductsList list
	 * @param subpartProduct product
	 * @param itemsMap map
	 */
	private void updateSubpartMap(final List<BlProductModel> serialProductsList, final BlSerialProductModel subpartProduct,
								  final Map<String, ItemStatusEnum> itemsMap)
	{
		final String subPartName = subpartProduct.getBlProduct().getName();
		final BlProductModel blProduct = subpartProduct.getBlProduct();

		if (itemsMap.containsKey(subPartName)
				&& itemsMap.get(subPartName).equals(ItemStatusEnum.NOT_INCLUDED))
		{
			itemsMap.remove(subPartName);
			itemsMap.put(subpartProduct.getCode(), ItemStatusEnum.INCLUDED);
			subpartProduct.setHardAssigned(true);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SERIAL_HARD_ASSIGN, subpartProduct);
			modelService.save(subpartProduct);

			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Replaced the subpart name {} with subpart serial product code {} in Items Map ", subPartName,subpartProduct.getCode());
		}
		else
		{
			final String updatedName = subPartName.concat(BlInventoryScanLoggingConstants.DOUBLE_HYPHEN);
			getUpdatedName(itemsMap, updatedName, subpartProduct);
		}
		if (serialProductsList.contains(blProduct))
		{
			serialProductsList.remove(blProduct);
			serialProductsList.add(subpartProduct);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Replaced the blproduct model {} with subpart serial product model {} in serial product list ", blProduct.getCode(),subpartProduct.getCode());
		}
	}

	/**
	 * This method is used to update the subpart map
	 * @param itemsMap map
	 * @param updatedName name
	 * @param subpartProduct product
	 */
	private void getUpdatedName(final Map<String, ItemStatusEnum> itemsMap, final String updatedName,
								final BlSerialProductModel subpartProduct)
	{
		BlLogger.logMessage(LOG, Level.DEBUG, updatedName);

		final List<String> keyList = itemsMap.keySet().stream()
				.filter(key -> key.contains(updatedName) && itemsMap.get(key).equals(ItemStatusEnum.NOT_INCLUDED))
				.collect(Collectors.toList());
		if (CollectionUtils.isNotEmpty(keyList))
		{
			itemsMap.remove(keyList.get(0));
			itemsMap.put(subpartProduct.getCode(), ItemStatusEnum.INCLUDED);
			subpartProduct.setHardAssigned(true);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, BlInventoryScanLoggingConstants.SERIAL_HARD_ASSIGN, subpartProduct);
			modelService.save(subpartProduct);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Replaced the subpart name {} with subpart serial product code {} in Items Map ", keyList.get(0) ,subpartProduct.getCode());
		}
	}

	/**
	 * This method is used to update Item Map based on the serial scanned
	 * @param serialProduct
	 * @param consignment
	 */
	private void updateSerialProductMap(final Map<String, ItemStatusEnum> itemsMap, final BlSerialProductModel serialProduct,
			final ConsignmentModel consignment)
	{
		if (itemsMap.containsKey(serialProduct.getCode())
				&& itemsMap.get(serialProduct.getCode()).equals(ItemStatusEnum.NOT_INCLUDED))
		{
			itemsMap.replace(serialProduct.getCode(), ItemStatusEnum.INCLUDED);
			serialProduct.setHardAssigned(true);
			serialProduct.setAssociatedShippedConsignment(consignment);
			if(BooleanUtils.isTrue(serialProduct.getIsBufferedInventory())) {
				serialProduct.setIsBufferedInventory(Boolean.FALSE);
				blProductService.changeBufferInvFlagInStagedVersion(serialProduct.getCode(), Boolean.FALSE);
			}
			final Collection<StockLevelModel> findSerialStockLevelForDate = (consignment.getOptimizedShippingStartDate() !=null && consignment.getOptimizedShippingEndDate() !=null ) ? blStockLevelDao.findSerialStockLevelForDate(
					serialProduct.getCode(), consignment.getOptimizedShippingStartDate(), consignment.getOptimizedShippingEndDate())  : CollectionUtils.EMPTY_COLLECTION;
			if (CollectionUtils.isNotEmpty(findSerialStockLevelForDate))
			{
				findSerialStockLevelForDate.forEach(stockLevel -> stockLevel.setHardAssigned(true));
			}
			modelService.save(serialProduct);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Serial product with code {} is scanned successfully and marked as included and hard assigned to true.", serialProduct.getCode());
		}
	}

	/**
	 * This method is used to get the serial/subpart from consignment
	 * @param filteredSerialProduct serial
	 * @param filteredSubPartProduct subpart
	 * @param serialItem serial
	 */
	private void getSerialFromConsignment(final List<BlProductModel> filteredSerialProduct,
										  final List<BlProductModel> filteredSubPartProduct, final BlProductModel serialItem)
	{
		if (serialItem instanceof BlSerialProductModel)
		{
			if (!ProductTypeEnum.SUBPARTS.equals(serialItem.getProductType()))
			{
				filteredSerialProduct.add(serialItem);
				BlLogger.logFormattedMessage(LOG, Level.DEBUG, "Serial Product with code {} added to list", serialItem.getCode());
			}
			else if (((BlSerialProductModel) serialItem).getBarcode() != null
					&& ProductTypeEnum.SUBPARTS.equals(serialItem.getProductType()))
			{
				filteredSubPartProduct.add(serialItem);
				BlLogger.logFormattedMessage(LOG, Level.DEBUG, "Sub Part Product with code {} added to list", serialItem.getCode());
			}
		}
	}

	/**
	 * This method is used to get scanned serial product
	 * @param blScannedProduct product
	 * @param scannedSerialProduct serial
	 * @param scannedSubpartProduct subpart
	 */
	private void getScannedSerial(final Collection<BlSerialProductModel> blScannedProduct,
			final List<BlSerialProductModel> scannedSerialProduct, final List<BlSerialProductModel> scannedSubpartProduct,
			final BlInventoryLocationModel blLocalInventoryLocation)
	{
		blScannedProduct.forEach(scannedProduct -> {
			try
			{
				// to enter OC location details in BLinventory
				if (null != blLocalInventoryLocation)
				{
					setBlInventoryLocation(blLocalInventoryLocation);

					setBlLocationScanHistory(scannedProduct, false, blLocalInventoryLocation);
				}
			}
			catch (final Exception e)
			{
				e.printStackTrace();
			}

			if (!ProductTypeEnum.SUBPARTS.equals(scannedProduct.getProductType()))
			{
				scannedSerialProduct.add(scannedProduct);
				BlLogger.logFormattedMessage(LOG, Level.DEBUG, "Scanned serial product with code {} added to list", scannedProduct.getCode());
			}
			else if (ProductTypeEnum.SUBPARTS.equals(scannedProduct.getProductType()))
			{
				scannedSubpartProduct.add(scannedProduct);
				BlLogger.logFormattedMessage(LOG, Level.DEBUG, "Scanned subpart product with code {} added to list", scannedProduct.getCode());
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
		final List<String> missingBarcodeSerialList = new ArrayList<>();
		final Map<String, List<String>> processStatus = Maps.newHashMap();
		final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
		final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
		if(CollectionUtils.isEmpty(blSerialProducts))
		{
			processStatus.put(BlInventoryScanLoggingConstants.MISSING_BARCODE_ITEMS,subList);
			return processStatus;
		}
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
															  final List<String> failedBarcodeList, final BlInventoryLocationModel blCleanCartLocation) {
		blSerialProducts.forEach(serial -> {
			if (BooleanUtils.isFalse(serial.isDirtyPriorityStatus())) { // check for dirtycart flag on serial
				updateLocationOnItem(serial, blCleanCartLocation, Boolean.FALSE);
				modelService.save(serial);
			} else {
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
				serial.setDirtyPriorityStatus(Boolean.FALSE); // As per BL-822 AC.1 setting dirty to FALSE.
				updateLocationOnItem(serial, blCleanCartLocation, Boolean.FALSE);
				modelService.save(serial);
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
		return checkLocationWithType(barcodes, BlInventoryScanLoggingConstants.getDefaultInventoryLocation(),
				Lists.newArrayList(BlInventoryScanLoggingConstants.ALLOW_SCAN));
	}

	/**
	 * {@inheritDoc}
	 *
	 */
	@Override
	public boolean checkIfFirstEntryIsLocation(final List<String> barcodes)
	{
		if (CollectionUtils.isNotEmpty(barcodes))
		{
			final String barcode = barcodes.get(0);
			return BlInventoryScanLoggingConstants.getDefaultInventoryLocation().stream()
					.anyMatch(location -> barcode.startsWith(location));
		}
		return Boolean.TRUE;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<Integer, Collection<String>> doUnboxing(final List<String> barcodes)
	{
		long startTime = System.nanoTime();
		final Map<Integer, Collection<String>> result = new HashMap<>();
		final BlInventoryLocationModel blInventoryLocationModel = getBlInventoryLocation();
		final List<String> subList = barcodes.subList(BlInventoryScanLoggingConstants.INT_ZERO, barcodes.size() - BlInventoryScanLoggingConstants.ONE);
		final List<String> missingBarcodeList = new ArrayList<>();
		final Set<String> missingPackageBarcodeList = new HashSet<>();
		final List<String> removedUnboxedSerialBarcodeList = new ArrayList<>();
		final Set<String> availablePackageBarcodes = new HashSet<>();
		final Collection<BlSerialProductModel> serialsByBarcodesAndVersion = getBlInventoryScanToolDao().getSerialsByBarcodesAndVersion(subList,BlInventoryScanLoggingConstants.ONLINE);
		if(CollectionUtils.isEmpty(serialsByBarcodesAndVersion))
		{
			result.put(BlInventoryScanLoggingConstants.INT_TEN, subList);
			return result;
		}
		else if(serialsByBarcodesAndVersion.size() != subList.size())
		{
			getInavlidBarcodeList(result, subList, missingBarcodeList, serialsByBarcodesAndVersion);
		}

		removeUnboxedSerialsFromList(serialsByBarcodesAndVersion, removedUnboxedSerialBarcodeList);

		if (Objects.nonNull(blInventoryLocationModel) && Objects.nonNull(blInventoryLocationModel.getLocationCategory()))
		{
			final String locationCategory = blInventoryLocationModel.getLocationCategory().getCode();
			if(BlInventoryScanUtility.getUnBoxingWorkStationLocations().contains(locationCategory))
			{
				performWorkstationScanOnSerial(result, serialsByBarcodesAndVersion);
			}
			else
			{
				if(CollectionUtils.isNotEmpty(removedUnboxedSerialBarcodeList))
				{
				setLocationDP(BlInventoryScanUtility.getDirtyPriorityCartLocations().contains(locationCategory));
				final Collection<PackagingInfoModel> packagingInfoModels = this.getPackageForSerials(removedUnboxedSerialBarcodeList);
				if(CollectionUtils.isEmpty(packagingInfoModels))
				{
					  //result.put(BlInventoryScanLoggingConstants.INT_NINE, removedUnboxedSerialBarcodeList);
					   final Collection<ConsignmentEntryModel> consignmentEntryModels = this.getConignmentEntriesForSerials(removedUnboxedSerialBarcodeList);
					performSerialToDPCOrDCLocationScanWithConsignment(result, blInventoryLocationModel, missingPackageBarcodeList, removedUnboxedSerialBarcodeList,
							availablePackageBarcodes, consignmentEntryModels);
					return result;
				}else {
				performSerialToDPCOrDCLocationScan(result, blInventoryLocationModel, missingPackageBarcodeList, removedUnboxedSerialBarcodeList,
							availablePackageBarcodes, packagingInfoModels);
				}}
			}

		}

		long stopTime = System.nanoTime();
		if(LOG.isDebugEnabled()) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "doUnboxing method having barcodes {} took {} time to execute", barcodes, stopTime - startTime);
		}
		return result;
	}

	private void performSerialToDPCOrDCLocationScanWithConsignment(final Map<Integer, Collection<String>> result, final BlInventoryLocationModel blInventoryLocationModel,
																   final Set<String> missingPackageBarcodeList, final List<String> removedUnboxedSerialBarcodeList,
																   final Set<String> availablePackageBarcodes, final Collection<ConsignmentEntryModel> consignmentEntryModels) {

		final Set<String> packageSerialBarcodes = new HashSet<>();
		getSerialBarcodesFromConsignmentEntry(consignmentEntryModels, packageSerialBarcodes);
		separateSerialsMissingAndPresentInPackages(removedUnboxedSerialBarcodeList,packageSerialBarcodes , missingPackageBarcodeList, availablePackageBarcodes);
		if(CollectionUtils.isNotEmpty(missingPackageBarcodeList))
		{
			result.put(BlInventoryScanLoggingConstants.INT_NINE, missingPackageBarcodeList);
		}
		final Map<Integer,List<String>> errorSerialList = new HashMap<>();
		this.getMapForUnboxAtDPOrDCWithCons(Lists.newArrayList(availablePackageBarcodes), result, blInventoryLocationModel, consignmentEntryModels, errorSerialList);

	}

	private void getMapForUnboxAtDPOrDCWithCons(final ArrayList<String> barcodes, final Map<Integer, Collection<String>> result, final BlInventoryLocationModel blInventoryLocationModel, final Collection<ConsignmentEntryModel> consignmentEntryModels, Map<Integer, List<String>> errorSerialList) {

			if(CollectionUtils.isEmpty(consignmentEntryModels))
			{
				result.put(BlInventoryScanLoggingConstants.ZERO, barcodes);
			}
			else
			{
				errorSerialList = doPerformDpcOrDcUnboxingForCons(barcodes, blInventoryLocationModel, consignmentEntryModels, errorSerialList);
			}
			handleUnboxingErrorMessage(result, errorSerialList);

	}

	private Map<Integer, List<String>> doPerformDpcOrDcUnboxingForCons(final ArrayList<String> barcodes, final BlInventoryLocationModel blInventoryLocationModel, final Collection<ConsignmentEntryModel> consignmentEntryModels, Map<Integer, List<String>> errorSerialList) {

		for (final ConsignmentEntryModel consignmentEntryModel : consignmentEntryModels)
		{
			final List<BlProductModel> blSerialProductModels = consignmentEntryModel.getSerialProducts().stream().filter(product -> product instanceof BlSerialProductModel).collect(Collectors.toList());

			if (CollectionUtils.isNotEmpty(blSerialProductModels) && blSerialProductModels.stream().anyMatch(serialCode -> barcodes.stream().anyMatch(b -> b.equals(((BlSerialProductModel) serialCode).getBarcode()))))
			{
				errorSerialList = getBlSerialProductModelBooleanMapConsign(consignmentEntryModel, consignmentEntryModel.getConsignment(),
						blSerialProductModels.stream().filter(
										serial -> barcodes.stream().anyMatch(b -> b.equals(((BlSerialProductModel) serial).getBarcode()))) // NOSONAR
								.collect(Collectors.toList()),
						blInventoryLocationModel);
			}
		}
		return errorSerialList;
	}

	private Map<Integer, List<String>> getBlSerialProductModelBooleanMapConsign(final ConsignmentEntryModel consignmentEntryModel, final ConsignmentModel consignmentModel, final List<BlProductModel> availableBarcodeList, final BlInventoryLocationModel blInventoryLocationModel) {

		if (CollectionUtils.isNotEmpty(availableBarcodeList))
		{
			BlLogger.logMessage(LOG, Level.DEBUG, "Marking consignment as PARTIALLY_UNBOXED");
			//changePackagingInfoStatus(packagingInfoModel, PackagingInfoStatus.PARTIALLY_UNBOXED);
			changeConsignmentStatus(consignmentModel, ConsignmentStatus.PARTIALLY_UNBOXED);
			final Map<Integer,List<String>> failedSerials = checkSerialsForDPAndSubParts(availableBarcodeList, consignmentModel,
					blInventoryLocationModel);
			if (canChangeToUnBoxStatusConsign(consignmentModel))
			{
				//changePackagingInfoStatus(packagingInfoModel, PackagingInfoStatus.UNBOXED);
				final Collection<PackagingInfoModel> consignmentPackages = consignmentModel.getPackaginginfos();
				//if (CollectionUtils.isNotEmpty(consignmentPackages) && consignmentPackages.stream()
					//	.allMatch(pkg -> PackagingInfoStatus.UNBOXED.equals(pkg.getPackagingInfoStatus())))
				//{
					changeConsignmentStatus(consignmentModel, ConsignmentStatus.UNBOXED);
					getBlOrderService().checkAndUpdateOrderStatus(consignmentModel.getOrder());
					BlLogger.logMessage(LOG, Level.DEBUG, "Marked Consignment as Unboxed as all packages are unboxed");
				//}
			}
			return failedSerials;
		}
		return null; //NO SONAR
	}

	private boolean canChangeToUnBoxStatusConsign(final ConsignmentModel consignmentModel) {
		final HashSet<SerialStatusEnum> itemStatuses = Sets.newHashSet();
		consignmentModel.getConsignmentEntries().forEach(consignmentEntry -> {
			consignmentEntry.getSerialProducts().forEach(serial -> {
				if (serial instanceof BlSerialProductModel)
				{
					itemStatuses.add(((BlSerialProductModel) serial).getSerialStatus());
				}
			});
		});

		return itemStatuses.size() == BlCoreConstants.STATUS_LIST_SIZE_ONE
				&& itemStatuses.iterator().next().equals(SerialStatusEnum.UNBOXED);
	}

	private void getSerialBarcodesFromConsignmentEntry(final Collection<ConsignmentEntryModel> consignmentEntryModels, final Set<String> packageSerialBarcodes) {

		consignmentEntryModels.forEach(consignmentEntry -> consignmentEntry.getSerialProducts().forEach(blSerial -> {
			if (blSerial instanceof BlSerialProductModel)
			{
				packageSerialBarcodes.add(((BlSerialProductModel) blSerial).getBarcode());
			}
		}));
	}

	@Override
	public Collection<ConsignmentEntryModel> getConignmentEntriesForSerials(final List<String> removedUnboxedSerialBarcodeList) {
		final Collection<ConsignmentEntryModel> consignmentEntryModels = new ArrayList<>();
		final Collection<ConsignmentModel> consignmentModels =  getBlInventoryScanToolDao().getConignmentEntriesForSerials(removedUnboxedSerialBarcodeList);
		consignmentModels.forEach(consignmentModel -> {
			consignmentModel.getConsignmentEntries().forEach(consignmentEntryModel -> {
				consignmentEntryModels.add(consignmentEntryModel);
			});
		});
		return consignmentEntryModels;
	}

	/**
	 * This method is used to perform scan of serial on DPC or DC location
	 * @param result
	 * @param blInventoryLocationModel
	 * @param missingPackageBarcodeList
	 * @param removedUnboxedSerialBarcodeList
	 * @param availablePackageBarcodes
	 * @param packagingInfoModels
	 */
	private void performSerialToDPCOrDCLocationScan(final Map<Integer, Collection<String>> result, final BlInventoryLocationModel blInventoryLocationModel,
			final Set<String> missingPackageBarcodeList, final List<String> removedUnboxedSerialBarcodeList,
			final Set<String> availablePackageBarcodes, final Collection<PackagingInfoModel> packagingInfoModels)
	{
		final Set<String> packageSerialBarcodes = new HashSet<>();
		getSerialBarcodesFromPackages(packagingInfoModels, packageSerialBarcodes);
		separateSerialsMissingAndPresentInPackages(removedUnboxedSerialBarcodeList,packageSerialBarcodes , missingPackageBarcodeList, availablePackageBarcodes);
		if(CollectionUtils.isNotEmpty(missingPackageBarcodeList))
		{
		result.put(BlInventoryScanLoggingConstants.INT_NINE, missingPackageBarcodeList);
		}
		final Map<Integer,List<String>> errorSerialList = new HashMap<>();
		this.getMapForUnboxAtDPOrDC(Lists.newArrayList(availablePackageBarcodes), result, blInventoryLocationModel, packagingInfoModels, errorSerialList);
	}

	/**
	 * This method is used to perform scan of serial on work station location
	 * @param result
	 * @param serialsByBarcodesAndVersion
	 */
	private void performWorkstationScanOnSerial(final Map<Integer, Collection<String>> result,
			final Collection<BlSerialProductModel> serialsByBarcodesAndVersion)
	{
		final List<String> failedBarcodeList = new ArrayList<>();
		final Collection<String> dirtyProductSerialModels = new ArrayList<>();
		final List<String> collect = serialsByBarcodesAndVersion.stream().map(BlSerialProductModel::getBarcode).collect(Collectors.toList());
		this.getResultMapForUnboxAtWorkstation(result, failedBarcodeList, dirtyProductSerialModels,
				collect);
	}

	/**
	 * This method is used to get list of invalid barcodes whose serials are not present in DB
	 * @param result
	 * @param subList
	 * @param missingBarcodeList
	 * @param serialsByBarcodesAndVersion
	 */
	private void getInavlidBarcodeList(final Map<Integer, Collection<String>> result, final List<String> subList,
			final List<String> missingBarcodeList,final Collection<BlSerialProductModel> serialsByBarcodesAndVersion)
	{
		subList.forEach(barcode -> {
			if (serialsByBarcodesAndVersion.stream().noneMatch(blSerial -> barcode.equals(blSerial.getBarcode())))
			{
				missingBarcodeList.add(barcode);
			}
		});
		if(CollectionUtils.isNotEmpty(missingBarcodeList))
		{
			result.put(BlInventoryScanLoggingConstants.INT_TEN, missingBarcodeList);
		}
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
										Map<Integer,List<String>> errorSerialList)
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
	private void handleUnboxingErrorMessage(final Map<Integer, Collection<String>> result, final Map<Integer,List<String>> errorSerialList)
	{
		if (Objects.isNull(errorSerialList))
		{
			result.put(BlInventoryScanLoggingConstants.TWO, null);
			BlLogger.logMessage(LOG, Level.DEBUG, "Failed to find package for scanned serials");
		}
		else
		{
			if(CollectionUtils.isEmpty(errorSerialList.get(BlInventoryScanLoggingConstants.FOUR))
					&& CollectionUtils.isEmpty(errorSerialList.get(BlInventoryScanLoggingConstants.FIVE)))
			{
				result.put(BlInventoryScanLoggingConstants.THREE, Lists.newArrayList());
			}
			else
			{
				result.put(BlInventoryScanLoggingConstants.THREE, Lists.newArrayList(BlInventoryScanLoggingConstants.ERROR_EXIST));
				result.putAll(errorSerialList);
			}
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
	private Map<Integer,List<String>> doPerformDpcOrDcUnboxing(final Collection<String> barcodes,
															   final BlInventoryLocationModel blInventoryLocationModel, final Collection<PackagingInfoModel> packagingInfoModels,
															   Map<Integer,List<String>> errorSerialList)
	{

		for (final PackagingInfoModel packagingInfo : packagingInfoModels)
		{
			final List<BlProductModel> blSerialProductModels = packagingInfo.getSerialProducts();
			if (CollectionUtils.isNotEmpty(blSerialProductModels))
			{
				errorSerialList = getBlSerialProductModelBooleanMap(packagingInfo, packagingInfo.getConsignment(),
						blSerialProductModels.stream().filter(
								serial -> barcodes.stream().anyMatch(b -> b.equals(((BlSerialProductModel) serial).getBarcode()))) // NOSONAR
								.collect(Collectors.toList()),
						blInventoryLocationModel);
			}
		}
		return errorSerialList;
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
	private Map<Integer,List<String>> getBlSerialProductModelBooleanMap(final PackagingInfoModel packagingInfoModel,
																		final ConsignmentModel consignmentModel, final Collection<BlProductModel> availableBarcodeList,
																		final BlInventoryLocationModel blInventoryLocationModel)
	{
		if (CollectionUtils.isNotEmpty(availableBarcodeList))
		{
			BlLogger.logMessage(LOG, Level.DEBUG, "Marking package and consignment as PARTIALLY_UNBOXED");
			changePackagingInfoStatus(packagingInfoModel, PackagingInfoStatus.PARTIALLY_UNBOXED);
			changeConsignmentStatus(consignmentModel, ConsignmentStatus.PARTIALLY_UNBOXED);
			final Map<Integer,List<String>> failedSerials = checkSerialsForDPAndSubParts(availableBarcodeList, consignmentModel,
					blInventoryLocationModel);
			if (canChangeToUnBoxStatus(packagingInfoModel))
			{
				changePackagingInfoStatus(packagingInfoModel, PackagingInfoStatus.UNBOXED);
				final Collection<PackagingInfoModel> consignmentPackages = consignmentModel.getPackaginginfos();
				if (CollectionUtils.isNotEmpty(consignmentPackages) && consignmentPackages.stream()
						.allMatch(pkg -> PackagingInfoStatus.UNBOXED.equals(pkg.getPackagingInfoStatus())))
				{
					changeConsignmentStatus(consignmentModel, ConsignmentStatus.UNBOXED);
					getBlOrderService().checkAndUpdateOrderStatus(consignmentModel.getOrder());
					BlLogger.logMessage(LOG, Level.DEBUG, "Marked Consignment as Unboxed as all packages are unboxed");
				}
			}
			return failedSerials;
		}
		return null; //NOSONAR
	}

	/**
	 * Can change to un box status.
	 *
	 * @param packagingInfoModel the packaging info model
	 * @return true, if successful
	 */
	private boolean canChangeToUnBoxStatus(final PackagingInfoModel packagingInfoModel)
	{
		final HashSet<SerialStatusEnum> itemStatuses = Sets.newHashSet();
		packagingInfoModel.getSerialProducts().forEach(serial -> {
			if (serial instanceof BlSerialProductModel)
			{
				itemStatuses.add(((BlSerialProductModel) serial).getSerialStatus());
			}
		});
		return itemStatuses.size() == BlCoreConstants.STATUS_LIST_SIZE_ONE
				&& itemStatuses.iterator().next().equals(SerialStatusEnum.UNBOXED);
	}

	/**
	 * This method will check dirtyPriority with SubParts status update
	 *
	 * @param blSerialProductModels the bl serial product models
	 * @param consignmentModel the consignment model
	 * @param blInventoryLocationModel the bl inventory location model
	 * @return the collection
	 */
	public Map<Integer,List<String>> checkSerialsForDPAndSubParts(final Collection<BlProductModel> blSerialProductModels,
																  final ConsignmentModel consignmentModel, final BlInventoryLocationModel blInventoryLocationModel)
	{
		final List<String> dirtyPrioritySerialList = new ArrayList<>();
		final List<String> dirtySerialList = new ArrayList<>();
		final Map<Integer,List<String>> errorList = new HashMap<>();
		if (CollectionUtils.isNotEmpty(blSerialProductModels))
		{
			for (final BlProductModel model : blSerialProductModels)
			{
				if(model instanceof BlSerialProductModel) {
						final BlSerialProductModel serialProductModel = ((BlSerialProductModel) model);
						setAssociatedConsignment(consignmentModel, serialProductModel);
						serialProductModel.setAssociatedOrder(
								consignmentModel.getOrder() instanceof OrderModel ? ((OrderModel) consignmentModel.getOrder()) : null);
						serialProductModel
								.setConsignmentEntry(getConsignmentEntryFromConsignment(consignmentModel, serialProductModel.getCode()));

						updateNumberOfRentedDaysForReturnedSerials(consignmentModel, serialProductModel);

						performLocationUpdateOnSerial(blInventoryLocationModel, dirtyPrioritySerialList, dirtySerialList, serialProductModel);
				}
			}
		}
		errorList.put(BlInventoryScanLoggingConstants.FOUR, dirtySerialList);
		errorList.put(BlInventoryScanLoggingConstants.FIVE, dirtyPrioritySerialList);
		return errorList;
	}

	/**
	 * This method will calculate and set the number of days rented for the serial
	 *
	 * @param serialProductModel the bl serial product model
	 * @param consignmentModel   the consignment model
	 */
	private void updateNumberOfRentedDaysForReturnedSerials(
			final ConsignmentModel consignmentModel,
			final BlSerialProductModel serialProductModel) {

		final long daysRentedEarlier =
				null != serialProductModel.getNoDaysRented() ? serialProductModel.getNoDaysRented() : 0;
		final PackagingInfoModel packagingInfo = consignmentModel.getPackagingInfo();
		final AbstractOrderModel orderModel = consignmentModel.getOrder();

		long daysRented = 0;

		if (null != packagingInfo && null != orderModel) {

			final Date latePackageDate =
					null != packagingInfo.getLatePackageDate() ? packagingInfo.getLatePackageDate()
							: orderModel.getRentalEndDate();
			//For used gear, we dont have startDate and endDate
			if (orderModel.getRentalStartDate() != null && latePackageDate != null)
			{
				daysRented = BlDateTimeUtils.getDaysBetweenDates(orderModel.getRentalStartDate(), latePackageDate);
			}
		}

		serialProductModel.setNoDaysRented(daysRentedEarlier + daysRented);
	}

	/**
	 * Gets the consignment entry from consignment.
	 *
	 * @param consignmentModel
	 *           the consignment model
	 * @param serialCode
	 *           the serial code
	 * @return the consignment entry from consignment
	 */
	private ConsignmentEntryModel getConsignmentEntryFromConsignment(final ConsignmentModel consignmentModel,
																	 final String serialCode)
	{
		return consignmentModel.getConsignmentEntries().stream()
				.filter(entry -> isSerialPresentInConsignmentEntry(entry, serialCode)).findFirst().orElse(null);
	}

	/**
	 * This method will update location on serial and save it. Also, it will create a history for scan and will associate with
	 * the InventoryLocation if barcode is valid serial product and if not then will fill it into failedBarcodeList to
	 * check status of scan that success or failure
	 *
	 * @param failedBarcodeList from scanned barcode list
	 * @param blSerialProducts  from barcodes
	 * @param iteratorBarcode   current iterator
	 */
	public void setInventoryLocationOnSerial(final List<String> failedBarcodeList, final Collection<BlSerialProductModel> blSerialProducts,
											 final String iteratorBarcode) {
		final BlSerialProductModel blSerialProduct = blSerialProducts.stream()
				.filter(p -> p.getBarcode().equals(iteratorBarcode)).findFirst().orElse(null);
		doUpdateLocation(failedBarcodeList, iteratorBarcode, blSerialProduct);
	}

	/**
	 * Perform location update on serial.
	 *
	 * @param blInventoryLocationModel the bl inventory location model
	 * @param dirtyPrioritySerialList the serial list
	 * @param dirtySerialList the serial list
	 * @param serialProductModel the serial product model
	 */
	private void performLocationUpdateOnSerial(final BlInventoryLocationModel blInventoryLocationModel, final Collection<String> dirtyPrioritySerialList,
											   final Collection<String> dirtySerialList, final BlSerialProductModel serialProductModel)
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
			checkInventoryLocationForDCOrDPC(serialProductModel, isLocationDP(), blInventoryLocationModel, dirtyPrioritySerialList,dirtySerialList);
		}
	}

	/**
	 * This method will check Inventory location for DC or DPC
	 *
	 * @param blSerialProductModel the bl serial product model
	 * @param isLocationDPC the is location DPC
	 * @param blInventoryLocationLocal the bl inventory location local
	 * @param dirtyPrioritySerialList the dirty priority serial list
	 * @param dirtySerialList the dirty serial list
	 */
	public void checkInventoryLocationForDCOrDPC(final BlSerialProductModel blSerialProductModel, final boolean isLocationDPC,
												 final BlInventoryLocationModel blInventoryLocationLocal, final Collection<String> dirtyPrioritySerialList,
												 final Collection<String> dirtySerialList)
	{
		if (blSerialProductModel.isDirtyPriorityStatus() || doCheckDirtyPriorityStatus(blSerialProductModel))
		{
			if (isLocationDPC)
			{
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			}
			else
			{
				//dirtyPrioritySerialList.add(blSerialProductModel.getBarcode());
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			}
		}
		else
		{
			if (isLocationDPC || !getStatusOfLocationDC())
			{
				//dirtySerialList.add(blSerialProductModel.getBarcode());
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			}
			else
			{
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			}
		}
	}

	/**
	 * Do update location.
	 *
	 * @param failedBarcodeList the failed barcode list
	 * @param iteratorBarcode   the iterator barcode
	 * @param blSerialProduct   the bl serial product
	 */
	private void doUpdateLocation(final List<String> failedBarcodeList, final String iteratorBarcode,
								  final BlSerialProductModel blSerialProduct) {
		if (blSerialProduct != null) {
			final BlInventoryLocationModel blInventoryLocationLocal = getBlInventoryLocation();
			updateLocationOnItem(blSerialProduct, blInventoryLocationLocal, Boolean.FALSE);
			modelService.save(blSerialProduct);
		} else {
			failedBarcodeList.add(iteratorBarcode);
		}
	}

	/**
	 * Sets the inventory location on serial.
	 *
	 * @param failedBarcodeList        the failed barcode list
	 * @param blSerialProducts         the bl serial products
	 * @param iteratorBarcode          the iterator barcode
	 * @param dirtyProductSerialModels the dirty product serial models
	 */
	private void setInventoryLocationOnSerial(final List<String> failedBarcodeList,
											  final Collection<BlSerialProductModel> blSerialProducts, final String iteratorBarcode,
											  final Collection<String> dirtyProductSerialModels) {
		final BlSerialProductModel blSerialProduct = blSerialProducts.stream().filter(p -> p.getBarcode().equals(iteratorBarcode))
				.findFirst().orElse(null);
		if (Objects.nonNull(blSerialProduct)) {
			if (Objects.nonNull(blSerialProduct.getProductType())
					&& blSerialProduct.getProductType().equals(ProductTypeEnum.SUBPARTS)) {
				blSerialProduct.setSerialStatus(SerialStatusEnum.IN_HOUSE);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing Serial status to In-House for sub part with code : {}", blSerialProduct.getCode());
				modelService.save(blSerialProduct);
				modelService.refresh(blSerialProduct);
			} else {
				doSetOcLocationOnSerial(failedBarcodeList, iteratorBarcode, dirtyProductSerialModels, blSerialProduct);
			}
		} else {
			failedBarcodeList.add(iteratorBarcode);
		}
	}

	/**
	 * Do set oc location on serial.
	 *
	 * @param failedBarcodeList        the failed barcode list
	 * @param iteratorBarcode          the iterator barcode
	 * @param dirtyProductSerialModels the dirty product serial models
	 * @param blSerialProduct          the bl serial product
	 */
	private void doSetOcLocationOnSerial(final List<String> failedBarcodeList, final String iteratorBarcode,
										 final Collection<String> dirtyProductSerialModels, final BlSerialProductModel blSerialProduct) {
		blSerialProduct.setSerialStatus(SerialStatusEnum.PARTIALLY_UNBOXED);
		if (blSerialProduct.isDirtyPriorityStatus()) {
			dirtyProductSerialModels.add(blSerialProduct.getBarcode());
		} else {
			checkItemIsDirty(blSerialProduct);
			if (blSerialProduct.isDirtyPriorityStatus()) {
				dirtyProductSerialModels.add(blSerialProduct.getBarcode());
			}
		}
		doUpdateLocation(failedBarcodeList, iteratorBarcode, blSerialProduct);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean doCheckDirtyPriorityStatus(final BlSerialProductModel serialProductModel) {
		this.checkItemIsDirty(serialProductModel);
		return serialProductModel.isDirtyPriorityStatus();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void flagAllDirtyPrioritySerialsOfConsignment() {
		final Collection<ConsignmentModel> todaysShippingOrders = this.getTodaysShippingOrders();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "DefaultBlInventoryScanToolService : Consignments found : {} size is : {}"
				, todaysShippingOrders.toString(), todaysShippingOrders.size());
		if (CollectionUtils.isNotEmpty(todaysShippingOrders)) {
			for (final ConsignmentModel consignment : todaysShippingOrders) {
				this.flagAllDirtyPrioritySerialsOfNewOrder(consignment);
			}
		}
	}

	/**
	 * Check item is dirty.
	 *
	 * @param serialProductModel the serial product model
	 */
	private void checkItemIsDirty(final BlSerialProductModel serialProductModel) {
		final Collection<ConsignmentModel> allConsignmentForSerial = getBlInventoryScanToolDao().getTodaysShippingConsignments(serialProductModel.getCode());
		markDirtyToSerial(serialProductModel, allConsignmentForSerial);
	}

	/**
	 * Mark dirty to serial.
	 *
	 * @param serialProductModel      the serial product model
	 * @param allConsignmentForSerial the all consignment for serial
	 */
	private void markDirtyToSerial(final BlSerialProductModel serialProductModel,
								   final Collection<ConsignmentModel> allConsignmentForSerial) {
		if (CollectionUtils.isEmpty(allConsignmentForSerial)) {
			serialProductModel.setDirtyPriorityStatus(Boolean.FALSE);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Marking serial : {} as Dirty Priority: FALSE", serialProductModel.getCode());
		} else {
			serialProductModel.setDirtyPriorityStatus(Boolean.TRUE);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Marking serial : {} as Dirty Priority: TRUE", serialProductModel.getCode());
		}
		modelService.save(serialProductModel);
		modelService.refresh(serialProductModel);
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
			if (BooleanUtils.isFalse(serialProductModel.isDirtyPriorityStatus())
					&& !SerialStatusEnum.RECEIVED_OR_RETURNED.equals(serialProductModel.getSerialStatus()))
			{
				this.checkSerialForDirtyPriority(serialProductModel);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<PackagingInfoModel> getPackageForSerials(final Collection<String> barcodes) {
		return getBlInventoryScanToolDao().getPackageForSerials(barcodes);
	}

	/**
	 * Handle unboxing error message.
	 *
	 * @param result          the result
	 * @param errorSerialList the error serial list
	 */
	private void handleUnboxingErrorMessage(final Map<Integer, Collection<String>> result, final Collection<String> errorSerialList) {
		if (Objects.isNull(errorSerialList)) {
			result.put(BlInventoryScanLoggingConstants.TWO, null);
			BlLogger.logMessage(LOG, Level.DEBUG, "Failed to find package for scanned serials");
		} else {
			result.put(BlInventoryScanLoggingConstants.THREE, errorSerialList);
			BlLogger.logMessage(LOG, Level.DEBUG, "Scanned Performed with errorSerials if any exists");
		}
	}

	/**
	 * This method will return resultant map for Unboxing at workstation location
	 *
	 * @param result                   map
	 * @param failedBarcodeList        list
	 * @param dirtyProductSerialModels serials
	 */
	private void getResultMapForUnboxAtWorkstation(final Map<Integer, Collection<String>> result,
												   final List<String> failedBarcodeList, final Collection<String> dirtyProductSerialModels,
												   final Collection<String> barcodes) {
		final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(barcodes);
		if (CollectionUtils.isEmpty(blSerialProducts)) {
			result.put(BlInventoryScanLoggingConstants.ZERO, barcodes);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Failed barcode list: {}", failedBarcodeList);
		} else {
			barcodes.forEach(barcode -> setInventoryLocationOnSerial(failedBarcodeList, blSerialProducts, barcode, dirtyProductSerialModels));
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Failed barcode list: {}", failedBarcodeList);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Dirty Priority Serials : {}", dirtyProductSerialModels);
			if(CollectionUtils.isNotEmpty(failedBarcodeList))
			{
				result.put(BlInventoryScanLoggingConstants.ZERO, failedBarcodeList);
			}

			if(CollectionUtils.isNotEmpty(dirtyProductSerialModels))
			{
			result.put(BlInventoryScanLoggingConstants.ONE, dirtyProductSerialModels);
			}
		}
	}

	/**
	 * Checks if is serial present in consignment entry.
	 *
	 * @param entry      the entry
	 * @param serialCode the serial code
	 * @return true, if is serial present in consignment entry
	 */
	private boolean isSerialPresentInConsignmentEntry(final ConsignmentEntryModel entry, final String serialCode) {
		final Optional<BlProductModel> findAny = entry.getSerialProducts().stream()
				.filter(serial -> serialCode.equals(serial.getCode())).findAny();
		return findAny.isPresent();
	}

	/**
	 * Perform location update on serial.
	 *
	 * @param blInventoryLocationModel the bl inventory location model
	 * @param serialList               the serial list
	 * @param serialProductModel       the serial product model
	 */
	private void performLocationUpdateOnSerial(final BlInventoryLocationModel blInventoryLocationModel, final Collection<String> serialList,
											   final BlSerialProductModel serialProductModel) {
		if (Objects.nonNull(serialProductModel.getProductType()) && serialProductModel.getProductType().equals(ProductTypeEnum.SUBPARTS)) {
			serialProductModel.setSerialStatus(SerialStatusEnum.IN_HOUSE);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing Serial status to In-House for sub part with code : {}", serialProductModel.getCode());
			modelService.save(serialProductModel);
			modelService.refresh(serialProductModel);
		} else {
			checkInventoryLocationForDCOrDPC(serialProductModel, isLocationDP(), blInventoryLocationModel, serialList);
		}
	}

	/**
	 * This method will check Inventory location for DC or DPC
	 *
	 * @param blSerialProductModel     serial
	 * @param isLocationDPC            true/false
	 * @param blInventoryLocationLocal location
	 * @param serialList               failedSerialList
	 */
	public void checkInventoryLocationForDCOrDPC(final BlSerialProductModel blSerialProductModel, final boolean isLocationDPC,
												 final BlInventoryLocationModel blInventoryLocationLocal, final Collection<String> serialList) {
		if (blSerialProductModel.isDirtyPriorityStatus() || doCheckDirtyPriorityStatus(blSerialProductModel)) {
			if (isLocationDPC) {
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			} else {
				serialList.add(blSerialProductModel.getBarcode());
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			}
		} else {
			if (isLocationDPC) {
				serialList.add(blSerialProductModel.getBarcode());
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			} else {
				updateLocationOnItem(blSerialProductModel, blInventoryLocationLocal, Boolean.TRUE);
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean getStatusOfLocationDP() {
		return isLocationDP();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<ConsignmentModel> getTodaysShippingOrders() {
		return getBlInventoryScanToolDao().getTodaysShippingOrders();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Collection<ConsignmentModel> getAllConsignmentForSerial(final String serial) {
		return getBlInventoryScanToolDao().getAllConsignmentForSerial(serial);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void flagAllDirtyPrioritySerialsOfNewOrder(final ConsignmentModel consignment) {
		final Collection<ConsignmentEntryModel> consignmentEntryModels = consignment.getConsignmentEntries();
		if (CollectionUtils.isNotEmpty(consignmentEntryModels)) {
			for (final ConsignmentEntryModel consignmentEntry : consignmentEntryModels) {
				consignmentEntry.getSerialProducts().forEach(this::checkSerialsForDP);
			}
		}
	}

	/**
	 * This method will check serial for dirty Priority and will set status on serial and save it.
	 *
	 * @param serialProductModel product
	 */
	private void checkSerialForDirtyPriority(final BlSerialProductModel serialProductModel) {
		final Collection<ConsignmentModel> allConsignmentForSerial = this.getAllConsignmentForSerial(serialProductModel.getCode());
		markDirtyToSerial(serialProductModel, allConsignmentForSerial);
	}


	/**
	 * This method will change status of consignment
	 *
	 * @param consignmentModel  consignment
	 * @param consignmentStatus status
	 */
	public void changeConsignmentStatus(final ConsignmentModel consignmentModel, final ConsignmentStatus consignmentStatus) {
		consignmentModel.setStatus(consignmentStatus);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing status of Consignment : {} to {}",
				consignmentModel.getCode(), consignmentStatus.getCode());
		modelService.save(consignmentModel);
		modelService.refresh(consignmentModel);
		final AbstractOrderModel order = consignmentModel.getOrder();
		if (Objects.nonNull(order)) {
			order.setOrderReturnedToWarehouse(Boolean.TRUE);
			if(Objects.isNull(order.getOrderUnboxingTimestamp()))
			{
				final Date unboxingTimestamp = new Date();
				order.setOrderUnboxingTimestamp(unboxingTimestamp);
				blESPEventService.sendOrderUnboxed((OrderModel) order);
				BlLogger.logFormatMessageInfo(LOG, Level.INFO,
						"Setting Order Unboxing Timestamp : {} for Order with code : {}", unboxingTimestamp, order.getCode());
			}
			modelService.save(order);
			modelService.refresh(order);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
					"Setting Order Returned to Warehouse Flag as true for Order with code : {}", order.getCode());
		}
	}

	/**
	 * This method will change status of packagingInfo
	 *
	 * @param packagingInfoModel  package
	 * @param packagingInfoStatus status
	 */
	public void changePackagingInfoStatus(final PackagingInfoModel packagingInfoModel, final PackagingInfoStatus packagingInfoStatus) {
		packagingInfoModel.setPackagingInfoStatus(packagingInfoStatus);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Changing status of Packaging with PK : {} to {}",
				packagingInfoModel.getPk().toString(), packagingInfoStatus.getCode());
		modelService.save(packagingInfoModel);
		modelService.refresh(packagingInfoModel);
	}

	public BlInventoryLocationModel getBlInventoryLocation() {
		return blInventoryLocation;
	}

	public void setBlInventoryLocation(final BlInventoryLocationModel blInventoryLocation) { this.blInventoryLocation = blInventoryLocation; }

	public BlInventoryScanToolDao getBlInventoryScanToolDao() { return blInventoryScanToolDao; }

	public void setBlInventoryScanToolDao(final BlInventoryScanToolDao blInventoryScanToolDao) {
		this.blInventoryScanToolDao = blInventoryScanToolDao;
	}

	/**
	 * @return the packagingInfoModel
	 */
	public PackagingInfoModel getPackagingInfoModel()	{	return packagingInfoModel;	}

	/**
	 * @param packagingInfoModel
	 *           the packagingInfoModel to set
	 */
	public void setPackagingInfoModel(final PackagingInfoModel packagingInfoModel) 	{	this.packagingInfoModel = packagingInfoModel;	}

	@Override
	public void updateToUpsBound()	{
		final List<BlProductModel> serialProducts = getPackagingInfoModel().getSerialProducts();
		serialProducts.forEach(serial -> {
			if (serial instanceof BlSerialProductModel)	{
				final BlSerialProductModel blSerial = ((BlSerialProductModel) serial); // NOSONAR
				blSerial.setOcLocation(getBlInventoryLocation().getCode());
				blSerial.setInventoryLocationID(getBlInventoryLocation().getInventoryLocationID());
				blSerial.setOcLocationDetails(getBlInventoryLocation());
				modelService.save(blSerial);
			}
		});
	}

	public boolean isLocationDP()
	{
		return isLocationDP;
	}

	public void setLocationDP(final boolean isLocationDP)
	{
		this.isLocationDP = isLocationDP;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean getStatusOfLocationDC()	{
		final BlInventoryLocationModel blInventoryLocationModel = getBlInventoryLocation();
		if(Objects.nonNull(blInventoryLocationModel) && Objects.nonNull(blInventoryLocationModel.getLocationCategory())) {
			return BlInventoryScanUtility.getDirtyCartLocations().contains(blInventoryLocationModel.getLocationCategory().getCode());
		}
		return false;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<String> getSuccessString(final List<String> barcodes) {
		final List<String> resultList = new ArrayList<>();
		final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
		final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
		if(CollectionUtils.isNotEmpty(blSerialProducts)) {
			for (final String barcode: subList) {
				blSerialProducts.stream().filter(prod -> barcode.equals(prod.getBarcode())).findFirst()
						.ifPresent(blSerialProductModel -> resultList.add(blSerialProductModel.getBarcode() +
								BlInventoryScanLoggingConstants.FOR + blSerialProductModel.getCode()));
			}
		}
		return resultList;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void updateSerialLastScanLocation(final ConsignmentModel consignmentModel,final String parentLocation)
	{
		consignmentModel.getConsignmentEntries().forEach(consignmentEntry -> consignmentEntry.getSerialProducts().forEach(serialProduct -> {
			if (serialProduct instanceof BlSerialProductModel)
			{
				final BlSerialProductModel serial = (BlSerialProductModel) serialProduct;

				//to update the OC location, in blInventoryLocationScanHistory
				try
				{
					final BlInventoryLocationModel blLocalInventoryLocation = getBlInventoryScanToolDao()
							.getInventoryLocationById(parentLocation);

					if (null != blLocalInventoryLocation)
					{
						setBlInventoryLocation(blLocalInventoryLocation);

						setBlLocationScanHistory(serial, false, blLocalInventoryLocation);
					}
				}
				catch (final Exception e)
				{
					e.printStackTrace();
				}

				serial.setLastLocationScanParent(parentLocation);
				modelService.save(serial);
				modelService.refresh(serial);
				BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "lastScanParentLocation updated to {} for serial {}", parentLocation,serial.getCode());
			}
		}));
	}

	/**
	 * @return the blOrderService
	 */
	public BlOrderService getBlOrderService()
	{
		return blOrderService;
	}

	/**
	 * @param blOrderService the blOrderService to set
	 */
	public void setBlOrderService(final BlOrderService blOrderService)
	{
		this.blOrderService = blOrderService;
	}

	@Override
	public boolean checkIfLocationIsBin(final String barcode, final boolean checkInDb)
	{
		if (StringUtils.isNotBlank(barcode))
		{
			final boolean startsWithBin = barcode.startsWith(BlInventoryScanLoggingConstants.BIN);
			if (checkInDb && startsWithBin)
			{
				final BlInventoryLocationModel inventoryLocationById = getInventoryLocationById(barcode);
				if (Objects.isNull(inventoryLocationById) || Objects.isNull(inventoryLocationById.getInventoryType())
						|| !BlInventoryScanLoggingConstants.BIN.equals(inventoryLocationById.getInventoryType().getCode()))
				{
					return false;
				}
				setBlInventoryLocation(inventoryLocationById);
				return true;
			}
			return startsWithBin;
		}
		return true;
	}

	@Override
	public boolean checkIfGivenBarcodeIsValidLocation(final String barcode)
	{
		return StringUtils.isNotBlank(barcode)
				&& BlInventoryScanLoggingConstants.getDefaultInventoryLocation().stream()
						.anyMatch(location -> barcode.startsWith(location));
	}

	@Override
	public Map<Integer, Collection<String>> performBinToCartScanning(final List<String> barcodes, final boolean isUnboxingFlow)
	{
		long startTime = System.nanoTime();
		final Map<Integer, Collection<String>> errors = Maps.newHashMap();
		final BlInventoryLocationModel binLocation = getBlInventoryScanToolDao()
				.getInventoryLocationById(barcodes.get(BlCoreConstants.INT_ZERO));
		final BlInventoryLocationModel blLocalInventoryLocation = getBlInventoryScanToolDao()
				.getInventoryLocationById(barcodes.get(BlCoreConstants.INT_ONE));
		checkForValidBinLocation(barcodes, errors, binLocation);
		if (Objects.isNull(blLocalInventoryLocation))
		{
			if (errors.containsKey(BlCoreConstants.INT_SIX))
			{
				final List<String> collection = Lists.newArrayList(errors.get(BlCoreConstants.INT_SIX));
				collection.add(barcodes.get(BlCoreConstants.INT_ONE));
				errors.put(BlCoreConstants.INT_SIX, collection); // location not found in database
			}
			else
			{
				errors.put(BlCoreConstants.INT_SIX, Lists.newArrayList(barcodes.get(BlCoreConstants.INT_ONE))); // location not found in database
			}
			return errors;
		}
		else if (BooleanUtils.isTrue(isUnboxingFlow)
				&& (Objects.isNull(blLocalInventoryLocation.getLocationCategory()) || (!BlInventoryScanUtility
						.getDirtyPriorityCartLocations().contains(blLocalInventoryLocation.getLocationCategory().getCode())
						&& !BlInventoryScanUtility.getDirtyCartLocations()
								.contains(blLocalInventoryLocation.getLocationCategory().getCode()))))
		{
			errors.put(BlCoreConstants.INT_EIGHT, Lists.newArrayList(barcodes.get(BlCoreConstants.INT_ONE))); //not a DC or DPC location
			return errors;
		}
		else if (BooleanUtils.isFalse(isUnboxingFlow) && (Objects.isNull(blLocalInventoryLocation.getLocationCategory())
				|| isBinValidForTechEngScan(blLocalInventoryLocation)))
		{
			errors.put(BlCoreConstants.INT_TWO, Lists.newArrayList(barcodes.get(BlCoreConstants.INT_ONE))); //not a Workstation or CC or CPC or Repair Cart location
			return errors;
		}
		binLocation.setParentInventoryLocation(blLocalInventoryLocation);
		modelService.save(binLocation);
		modelService.refresh(binLocation);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "BIN with code : {} scanned with Parent location with code : {}",
				binLocation.getCode(), blLocalInventoryLocation.getCode());
		performBinScannedSerials(barcodes, errors, binLocation, blLocalInventoryLocation, isUnboxingFlow);
		long stopTime = System.nanoTime();
		if(LOG.isDebugEnabled()) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "performBinToCartScanning method having barcodes {} took {} time to execute", barcodes, stopTime - startTime);
		}
		return errors;
	}

	/**
	 * Checks if is bin valid for tech eng scan.
	 *
	 * @param blLocalInventoryLocation the bl local inventory location
	 * @return true, if is bin valid for tech eng scan
	 */
	private boolean isBinValidForTechEngScan(final BlInventoryLocationModel blLocalInventoryLocation)
	{
		final List<String> listOfAllTechEngSupportedLocations = Lists.newArrayList();
		listOfAllTechEngSupportedLocations.addAll(BlInventoryScanUtility.getTechEngCleanPriorityCartLocations());
		listOfAllTechEngSupportedLocations.addAll(BlInventoryScanUtility.getTechEngCleanCartLocations());
		listOfAllTechEngSupportedLocations.addAll(BlInventoryScanUtility.getTechEngRepairLocations());
		listOfAllTechEngSupportedLocations.addAll(BlInventoryScanUtility.getTechEngWorkStationLocations());
		return !listOfAllTechEngSupportedLocations.contains(blLocalInventoryLocation.getLocationCategory().getCode());
	}

	/**
	 * Check for valid bin location.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param errors
	 *           the errors
	 * @param binLocation
	 *           the bin location
	 */
	private void checkForValidBinLocation(final List<String> barcodes, final Map<Integer, Collection<String>> errors,
			final BlInventoryLocationModel binLocation)
	{
		if (Objects.isNull(binLocation))
		{
			errors.put(BlCoreConstants.INT_SIX, Lists.newArrayList(barcodes.get(BlCoreConstants.INT_ZERO))); // location not found in database
		}
		else if (Objects.isNull(binLocation.getInventoryType())
				|| !BlInventoryScanLoggingConstants.BIN.equals(binLocation.getInventoryType().getCode()))
		{
			errors.put(BlCoreConstants.INT_SEVEN, Lists.newArrayList(barcodes.get(BlCoreConstants.INT_ONE))); // not a bin location
		}
	}

	/**
	 * Perform bin scanned serials.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param errors
	 *           the errors
	 * @param binLocation
	 *           the bin location
	 * @param blLocalInventoryLocation
	 *           the bl local inventory location
	 * @param isUnboxingFlow
	 *           the is unboxing flow
	 */
	private void performBinScannedSerials(final List<String> barcodes, final Map<Integer, Collection<String>> errors,
			final BlInventoryLocationModel binLocation, final BlInventoryLocationModel blLocalInventoryLocation,
			final boolean isUnboxingFlow)
	{
		final Collection<BlSerialProductModel> allSerialsByBinLocationAndVersion = StringUtils
				.isNotBlank(barcodes.get(BlCoreConstants.INT_ZERO))
						? getBlInventoryScanToolDao().getAllSerialsByBinLocationAndVersion(barcodes.get(BlCoreConstants.INT_ZERO),
								BlCoreConstants.ONLINE)
						: Lists.newArrayList();
		if(CollectionUtils.isNotEmpty(allSerialsByBinLocationAndVersion))
		{
			setBlInventoryLocation(blLocalInventoryLocation);
			if (isUnboxingFlow)
			{
				//performBinScannedSerialsToDPCOrDC(allSerialsByBinLocationAndVersion, errors, binLocation,
						//blLocalInventoryLocation);
				allSerialsByBinLocationAndVersion.forEach(blSerialProductModel -> {

					updateLocationOnItem(blSerialProductModel, binLocation, Boolean.FALSE);

				});
				modelService.saveAll(allSerialsByBinLocationAndVersion);
			}
			else
			{
				performBinScannedSerialsToCPCOrCC(allSerialsByBinLocationAndVersion, errors, binLocation,
						blLocalInventoryLocation);
			}
		}

	}

	/**
	 * Perform bin scanned serials to CPC or CC.
	 *
	 * @param allSerialsByBinLocationAndVersion
	 *           the all serials by bin location and version
	 * @param barcodes
	 *           the barcodes
	 * @param errors
	 *           the errors
	 * @param binLocation
	 *           the bin location
	 * @param blLocalInventoryLocation
	 *           the bl local inventory location
	 */
	private void performBinScannedSerialsToCPCOrCC(final Collection<BlSerialProductModel> allSerialsByBinLocationAndVersion,
			final Map<Integer, Collection<String>> errors, final BlInventoryLocationModel binLocation,
			final BlInventoryLocationModel blLocalInventoryLocation)
	{
		final List<BlSerialProductModel> prioritySerials = allSerialsByBinLocationAndVersion.stream()
				.filter(serial -> BooleanUtils.isTrue(serial.isDirtyPriorityStatus())).collect(Collectors.toList());
		final List<BlSerialProductModel> nonPrioritySerials = allSerialsByBinLocationAndVersion.stream()
				.filter(serial -> BooleanUtils.isFalse(serial.isDirtyPriorityStatus())).collect(Collectors.toList());
		if (BlInventoryScanUtility.getTechEngWorkStationLocations()
				.contains(blLocalInventoryLocation.getLocationCategory().getCode())
				|| BlInventoryScanUtility.getTechEngRepairLocations()
						.contains(blLocalInventoryLocation.getLocationCategory().getCode()))
		{
			performScanning(Lists.newArrayList(allSerialsByBinLocationAndVersion), binLocation, false);
		}
		else if (BlInventoryScanUtility.getTechEngCleanPriorityCartLocations()
				.contains(blLocalInventoryLocation.getLocationCategory().getCode()))
		{
			performCleanPriorityCartTechEngScan(errors, prioritySerials, nonPrioritySerials, binLocation);
		}
		else if (BlInventoryScanUtility.getTechEngCleanCartLocations()
				.contains(blLocalInventoryLocation.getLocationCategory().getCode()))
		{
			performCleanCartTechEngScan(errors, prioritySerials, nonPrioritySerials, binLocation);
		}
	}

	/**
	 * Perform clean cart tech eng scan.
	 *
	 * @param errors
	 *           the errors
	 * @param prioritySerials
	 *           the priority serials
	 * @param nonPrioritySerials
	 *           the non priority serials
	 * @param binLocation
	 *           the bin location
	 */
	private void performCleanCartTechEngScan(final Map<Integer, Collection<String>> errors,
			final List<BlSerialProductModel> prioritySerials, final List<BlSerialProductModel> nonPrioritySerials,
			final BlInventoryLocationModel binLocation)
	{
		if (CollectionUtils.isEmpty(prioritySerials))
		{
			performScanning(nonPrioritySerials, binLocation, false);
		}
		else
		{
			addErrorsForSerials(errors, prioritySerials, nonPrioritySerials);
		}
	}

	/**
	 * Perform clean priority cart tech eng scan.
	 *
	 * @param errors
	 *           the errors
	 * @param prioritySerials
	 *           the priority serials
	 * @param nonPrioritySerials
	 *           the non priority serials
	 * @param binLocation
	 *           the bin location
	 */
	private void performCleanPriorityCartTechEngScan(final Map<Integer, Collection<String>> errors,
			final List<BlSerialProductModel> prioritySerials, final List<BlSerialProductModel> nonPrioritySerials,
			final BlInventoryLocationModel binLocation)
	{
		if (CollectionUtils.isEmpty(nonPrioritySerials))
		{
			performScanning(prioritySerials, binLocation, true);
		}
		else
		{
			addErrorsForSerials(errors, prioritySerials, nonPrioritySerials);
		}
	}

	/**
	 * Perform scanning.
	 *
	 * @param serialsToScan
	 *           the serials to scan
	 * @param binLocation
	 *           the bin location
	 */
	private void performScanning(final List<BlSerialProductModel> serialsToScan, final BlInventoryLocationModel binLocation,
			final boolean checkForDirtyStatus)
	{
		serialsToScan.forEach(serial -> {
			if(checkForDirtyStatus && BooleanUtils.isTrue(serial.isDirtyPriorityStatus()))
			{
				serial.setDirtyPriorityStatus(Boolean.FALSE); // As per BL-822 AC.1 setting dirty to FALSE.
			}
			updateLocationOnItem(serial, binLocation, false);
		});
		modelService.saveAll(serialsToScan);
	}

	/**
	 * Adds the errors for serials.
	 *
	 * @param errors
	 *           the errors
	 * @param prioritySerials
	 *           the priority serials
	 * @param nonPrioritySerials
	 *           the non priority serials
	 */
	private void addErrorsForSerials(final Map<Integer, Collection<String>> errors,
			final List<BlSerialProductModel> prioritySerials, final List<BlSerialProductModel> nonPrioritySerials)
	{
		if(CollectionUtils.isNotEmpty(prioritySerials))
		{
			errors.put(0, prioritySerials.stream().map(BlSerialProductModel::getBarcode).collect(Collectors.toList()));//priority serials
		}
		if(CollectionUtils.isNotEmpty(nonPrioritySerials))
		{
			errors.put(1, nonPrioritySerials.stream().map(BlSerialProductModel::getBarcode).collect(Collectors.toList()));//non priority serials
		}
	}

	/**
	 * Perform bin scanned serials to DPC or DC.
	 *
	 * @param barcodes
	 *           the barcodes
	 * @param errors
	 *           the errors
	 * @param binLocation
	 *           the bin location
	 * @param blLocalInventoryLocation
	 *           the bl local inventory location
	 */
	private void performBinScannedSerialsToDPCOrDC(final Collection<BlSerialProductModel> allSerialsByBinLocationAndVersion,
			final Map<Integer, Collection<String>> errors, final BlInventoryLocationModel binLocation,
			final BlInventoryLocationModel blLocalInventoryLocation)
	{
		setLocationDP(BlInventoryScanUtility.getDirtyPriorityCartLocations()
				.contains(blLocalInventoryLocation.getLocationCategory().getCode()));
		final List<String> subList = Lists.newArrayList();
		removeUnboxedSerialsFromList(allSerialsByBinLocationAndVersion, subList);
		if (CollectionUtils.isNotEmpty(subList))
		{
			processSerialParentScanning(errors, binLocation, allSerialsByBinLocationAndVersion, subList);
		}
	}

	/**
	 * Removes the unboxed serials from list.
	 *
	 * @param allSerialsByBinLocationAndVersion
	 *           the all serials by bin location and version
	 * @param subList
	 *           the sub list
	 */
	private void removeUnboxedSerialsFromList(final Collection<BlSerialProductModel> allSerialsByBinLocationAndVersion,
			final List<String> subList)
	{
		allSerialsByBinLocationAndVersion.forEach(serial -> {
			if (!serial.getSerialStatus().getCode().equals(SerialStatusEnum.UNBOXED.getCode())
					&& StringUtils.isNotBlank(serial.getBarcode()))
			{
				subList.add(serial.getBarcode());
			}
		});
	}

	/**
	 * Process serial parent scanning.
	 *
	 * @param errors
	 *           the errors
	 * @param binLocation
	 *           the bin location
	 * @param allSerialsByBinLocationAndVersion
	 *           the all serials by bin location and version
	 * @param subList
	 *           the sub list
	 */
	private void processSerialParentScanning(final Map<Integer, Collection<String>> errors,
			final BlInventoryLocationModel binLocation, final Collection<BlSerialProductModel> allSerialsByBinLocationAndVersion,
			final List<String> subList)
	{
		final Collection<PackagingInfoModel> packagingInfoModels = this.getPackageForSerials(subList);
		if (CollectionUtils.isNotEmpty(packagingInfoModels))
		{
			final Set<String> packageSerialBarcodes = Sets.newHashSet();
			final Set<String> missingSerialPackages = Sets.newHashSet();
			final Set<String> barcodesPresentInPackage = Sets.newHashSet();
			getSerialBarcodesFromPackages(packagingInfoModels, packageSerialBarcodes);
			separateSerialsMissingAndPresentInPackages(subList, packageSerialBarcodes, missingSerialPackages,
					barcodesPresentInPackage);
			if(CollectionUtils.isNotEmpty(missingSerialPackages))
			{
				errors.put(BlCoreConstants.INT_NINE, Lists.newArrayList(missingSerialPackages)); // missing packages for serials
			}
			errors.putAll(doPerformDpcOrDcUnboxing(barcodesPresentInPackage, binLocation, packagingInfoModels, Maps.newHashMap()));
		}
		else
		{
			errors.put(BlCoreConstants.INT_NINE, subList); // missing packages for serials
		}
	}

	/**
	 * Gets the serial barcodes from packages.
	 *
	 * @param packagingInfoModels
	 *           the packaging info models
	 * @param packageSerialBarcodes
	 *           the package serial barcodes
	 * @return the serial barcodes from packages
	 */
	private void getSerialBarcodesFromPackages(final Collection<PackagingInfoModel> packagingInfoModels,
			final Set<String> packageSerialBarcodes)
	{
		packagingInfoModels.forEach(pack -> pack.getSerialProducts().forEach(blSerial -> {
			if (blSerial instanceof BlSerialProductModel)
			{
				packageSerialBarcodes.add(((BlSerialProductModel) blSerial).getBarcode());
			}
		}));
	}

	/**
	 * Separate serials missing and present in packages.
	 *
	 * @param subList
	 *           the sub list
	 * @param packageSerialBarcodes
	 *           the package serial barcodes
	 * @param missingSerialPackages
	 *           the missing serial packages
	 * @param barcodesPresentInPackage
	 *           the barcodes present in package
	 */
	private void separateSerialsMissingAndPresentInPackages(final List<String> subList, final Set<String> packageSerialBarcodes,
			final Set<String> missingSerialPackages, final Set<String> barcodesPresentInPackage)
	{
		subList.forEach(serialBarcode -> {
			if (packageSerialBarcodes.stream().anyMatch(serialBarcode::equals))
			{
				barcodesPresentInPackage.add(serialBarcode);
			}
			else
			{
				missingSerialPackages.add(serialBarcode);
			}
		});
	}

	@Override
	public Map<Integer, Collection<String>> doSerialLocationToBinScanningForUnboxing(final List<String> barcodes)
	{
		final Map<Integer, Collection<String>> errors = Maps.newHashMap();
		final List<String> defaultLocations = BlInventoryScanLoggingConstants.getDefaultInventoryLocation();
		final List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream().anyMatch(b::startsWith))
				.collect(Collectors.toList());

		if (CollectionUtils.isNotEmpty(filteredLocationList) && filteredLocationList.size() > BlCoreConstants.INT_ONE)
		{
			errors.put(BlCoreConstants.INT_TWELVE, Lists.newArrayList());
			return errors;
		}
		final List<String> serialBarcodeList = barcodes.subList(0, barcodes.size() - BlCoreConstants.INT_ONE);
		final Collection<BlSerialProductModel> serialsByBarcodesAndVersion = CollectionUtils.isNotEmpty(serialBarcodeList)
				? getBlInventoryScanToolDao().getSerialsByBarcodesAndVersion(serialBarcodeList, BlCoreConstants.ONLINE)
				: Lists.newArrayList();
		if (CollectionUtils.isEmpty(serialsByBarcodesAndVersion))
		{
			errors.put(BlCoreConstants.INT_TEN, serialBarcodeList); // Serial Not found in system
		}
		else
		{
			processSerialsForUnboxing(errors, serialsByBarcodesAndVersion, serialBarcodeList);
		}
		return errors;
	}


	private Map<Integer, Collection<String>> processSerialsForUnboxing(Map<Integer, Collection<String>> errors, Collection<BlSerialProductModel> serialsByBarcodesAndVersion, List<String> serialBarcodeList) {
		long startTime = System.nanoTime();
		final Set<String> missingPackageBarcodeList = new HashSet<>();
		final List<String> missingBarcodeList = new ArrayList<>();
		final List<String> removedUnboxedSerialBarcodeList = new ArrayList<>();
		final Set<String> availablePackageBarcodes = new HashSet<>();
		final List<String> missingBarcodesInDB = Lists.newArrayList();
		final BlInventoryLocationModel blInventoryLocationModel = getBlInventoryLocation();

		serialBarcodeList.forEach(barcode -> {
			if (serialsByBarcodesAndVersion.stream().noneMatch(blSerial -> barcode.equals(blSerial.getBarcode()))) {
				missingBarcodesInDB.add(barcode);
			}
		});
		if (CollectionUtils.isNotEmpty(missingBarcodesInDB)) {
			errors.put(BlCoreConstants.INT_TEN, missingBarcodesInDB); // Serial Not found in system
		}

		 if (serialsByBarcodesAndVersion.size() != serialBarcodeList.size()) {
			getInavlidBarcodeList(errors, serialBarcodeList, missingBarcodeList, serialsByBarcodesAndVersion);
		}

		removeUnboxedSerialsFromList(serialsByBarcodesAndVersion, removedUnboxedSerialBarcodeList);

		if (Objects.nonNull(blInventoryLocationModel) && Objects.nonNull(blInventoryLocationModel.getLocationCategory())) {
			final String locationCategory = blInventoryLocationModel.getLocationCategory().getCode();
			if (BlInventoryScanUtility.getUnBoxingWorkStationLocations().contains(locationCategory)) {
				performWorkstationScanOnSerial(errors, serialsByBarcodesAndVersion);
			} else if (CollectionUtils.isNotEmpty(removedUnboxedSerialBarcodeList)) {
					setLocationDP(BlInventoryScanUtility.getDirtyPriorityCartLocations().contains(locationCategory));

					final Collection<PackagingInfoModel> packagingInfoModels = this.getPackageForSerials(removedUnboxedSerialBarcodeList);

					if (CollectionUtils.isEmpty(packagingInfoModels)) {
						final Collection<ConsignmentEntryModel> consignmentEntryModels = this.getConignmentEntriesForSerials(removedUnboxedSerialBarcodeList);
						performSerialToDPCOrDCLocationScanWithConsignment(errors, blInventoryLocationModel, missingPackageBarcodeList, removedUnboxedSerialBarcodeList,
								availablePackageBarcodes, consignmentEntryModels);
					} else {
						performSerialToDPCOrDCLocationScan(errors, blInventoryLocationModel, missingPackageBarcodeList, removedUnboxedSerialBarcodeList,
								availablePackageBarcodes, packagingInfoModels);
					}

			}

		}
		long stopTime = System.nanoTime();
		if(LOG.isDebugEnabled()) {
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "processSerialsForUnboxing method having barcodes {} took {} time to execute", serialBarcodeList, stopTime - startTime);
		}
		return errors;
	}
	/**
	 * Adds the to error list.
	 *
	 * @param errors
	 *           the errors
	 * @param dirtyPrioritySerialList
	 *           the dirty priority serial list
	 * @param dirtySerialList
	 *           the dirty serial list
	 */
	private void addToErrorList(final Map<Integer, Collection<String>> errors, final List<String> dirtyPrioritySerialList,
			final List<String> dirtySerialList)
	{
		if (CollectionUtils.isNotEmpty(dirtyPrioritySerialList))
		{
			errors.put(BlCoreConstants.INT_ELEVEN, dirtyPrioritySerialList); //dirty priority barcodes
		}
		if (CollectionUtils.isNotEmpty(dirtySerialList))
		{
			errors.put(BlCoreConstants.INT_THIRTEEN, dirtySerialList); //dirty barcodes
		}
	}

	@Override
	public Map<Integer, Collection<String>> doSerialLocationToBinScanningForTechEng(final List<String> barcodes)
	{
		final Map<Integer, Collection<String>> errors = Maps.newHashMap();
		final List<String> defaultLocations = BlInventoryScanLoggingConstants.getDefaultInventoryLocation();
		final List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream().anyMatch(b::startsWith))
				.collect(Collectors.toList());
		if (CollectionUtils.isNotEmpty(filteredLocationList) && filteredLocationList.size() > BlCoreConstants.INT_ONE)
		{
			errors.put(BlCoreConstants.INT_TWELVE, Lists.newArrayList()); //More than one location found
			return errors;
		}
		final List<String> serialBarcodeList = barcodes.subList(0, barcodes.size() - BlCoreConstants.INT_ONE);
		final Collection<BlSerialProductModel> serialsByBarcodesAndVersion = CollectionUtils.isNotEmpty(serialBarcodeList)
				? getBlInventoryScanToolDao().getSerialsByBarcodesAndVersion(serialBarcodeList, BlCoreConstants.ONLINE)
				: Lists.newArrayList();
		if (CollectionUtils.isEmpty(serialsByBarcodesAndVersion))
		{
			errors.put(BlCoreConstants.INT_TEN, serialBarcodeList); // Serial Not found in system
		}
		else
		{
			final List<String> missingBarcodesInDB = Lists.newArrayList();
			serialBarcodeList.forEach(barcode -> {
				if (serialsByBarcodesAndVersion.stream().noneMatch(blSerial -> barcode.equals(blSerial.getBarcode())))
				{
					missingBarcodesInDB.add(barcode);
				}
			});
			if (CollectionUtils.isNotEmpty(missingBarcodesInDB))
			{
				errors.put(BlCoreConstants.INT_TEN, missingBarcodesInDB); // Serial Not found in system
			}
			serialsByBarcodesAndVersion.forEach(serial -> updateLocationOnItem(serial, getBlInventoryLocation(), Boolean.FALSE));
			modelService.saveAll(serialsByBarcodesAndVersion);
		}
		return errors;
	}

	/**
	 * Method to set the Associate Consignment
	 * @param consignmentModel  Consignmnet Model
	 * @param serialProductModel Serial Product Model
	 */
	private void setAssociatedConsignment(final ConsignmentModel consignmentModel, final BlSerialProductModel serialProductModel) {
		serialProductModel.setAssociatedConsignment(consignmentModel);
		//modelService.save(serialProductModel);
		//modelService.refresh(serialProductModel);
	}

	public BlStockService getBlStockService() {
		return blStockService;
	}

	public void setBlStockService(BlStockService blStockService) {
		this.blStockService = blStockService;
	}
}
