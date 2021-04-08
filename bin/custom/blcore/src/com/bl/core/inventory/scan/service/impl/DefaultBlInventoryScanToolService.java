package com.bl.core.inventory.scan.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BlInventoryScanToolDao;
import com.bl.core.inventory.scan.service.BlInventoryScanToolService;
import com.bl.core.model.BlInventoryLocationModel;
import com.bl.core.model.BlInventoryLocationScanHistoryModel;
import com.bl.core.model.BlInventoryScanConfigurationModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.commons.collections.CollectionUtils;
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

    @Autowired
    UserService userService;

    @Autowired
    private ModelService modelService;

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
    public int checkValidLocationInBarcodeList(final List<String> barcodes) {
        final List<String> defaultLocations = BlInventoryScanLoggingConstants.getDefaultInventoryLocation();
        final List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream()
                .anyMatch(b::startsWith)).collect(Collectors.toList());
        return checkValidInventoryLocation(barcodes.get(barcodes.size() - BlInventoryScanLoggingConstants.ONE), filteredLocationList);
    }

    /**
     * @param inventoryLocation    for update
     * @param filteredLocationList all locations in batch
     * @return int for success/error message
     */
    public int checkValidInventoryLocation(final String inventoryLocation, final List<String> filteredLocationList) {
        if (CollectionUtils.isNotEmpty(filteredLocationList)) {
            if (filteredLocationList.size() == BlInventoryScanLoggingConstants.ONE) {
                return validateLocation(inventoryLocation, filteredLocationList);
            }
            return BlInventoryScanLoggingConstants.FOUR;
        }
        return BlInventoryScanLoggingConstants.THREE;
    }

    /**
     * @param inventoryLocation    for update
     * @param filteredLocationList all locations in batch
     * @return int for success/error message
     */
    public int validateLocation(final String inventoryLocation, final List<String> filteredLocationList) {
        if (filteredLocationList.get(BlInventoryScanLoggingConstants.ZERO).equals(inventoryLocation)) {
            final BlInventoryLocationModel blLocalInventoryLocation = getBlInventoryScanToolDao().getInventoryLocationById(inventoryLocation);
            if (blLocalInventoryLocation != null) {
                setBlInventoryLocation(blLocalInventoryLocation);
                return BlInventoryScanLoggingConstants.ONE;
            }
            return BlInventoryScanLoggingConstants.TWO;
        }
        return BlInventoryScanLoggingConstants.THREE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getFailedBarcodeList(final List<String> barcodes) {
        final List<String> failedBarcodeList = new ArrayList<>();
        final List<String> subList = barcodes.subList(0, barcodes.size() - 1);
        final Collection<BlSerialProductModel> blSerialProducts = getBlInventoryScanToolDao().getSerialProductsByBarcode(subList);
        subList.forEach(barcode -> {
            setInventoryLocationOnSerial(failedBarcodeList, blSerialProducts, barcode);
        });
        return failedBarcodeList;
    }

    @Override
    public String getConfigKeyFromScanConfiguration(final String key) {
        BlInventoryScanConfigurationModel blInventoryScanConfigurationModel = getBlInventoryScanToolDao().getConfigKeyFromScanConfiguration(key);
        return blInventoryScanConfigurationModel != null ? blInventoryScanConfigurationModel.getBlScanConfigValue() :
                String.valueOf(BlInventoryScanLoggingConstants.TEN);
    }

    /**
     * @param failedBarcodeList from scanned barcode list
     * @param blSerialProducts  from barcodes
     * @param iteratorBarcode   current iterator
     */
    public void setInventoryLocationOnSerial(final List<String> failedBarcodeList, final Collection<BlSerialProductModel> blSerialProducts,
                                              final String iteratorBarcode) {
        final BlSerialProductModel blSerialProduct = blSerialProducts.stream()
                .filter(p -> p.getBarcode().equals(iteratorBarcode)).findFirst().orElse(null);
        if (blSerialProduct != null) {
            final BlInventoryLocationModel blInventoryLocation = getBlInventoryLocation();
            blSerialProduct.setSerialInventoryLocation(blInventoryLocation);
            blSerialProduct.setSerialLastLocation(blInventoryLocation);
            blSerialProduct.setSerialLastParentLocation(blInventoryLocation.getParentInventoryLocation());
            modelService.save(blSerialProduct);
            modelService.refresh(blSerialProduct);

            /* Scan History Entry*/
            final BlInventoryLocationScanHistoryModel blInventoryLocationScanHistory = modelService.create(BlInventoryLocationScanHistoryModel.class);
            blInventoryLocationScanHistory.setSerialProduct(blSerialProduct);
            blInventoryLocationScanHistory.setScanUser(userService.getCurrentUser());
            blInventoryLocationScanHistory.setBlInventoryLocation(blInventoryLocation);
            blInventoryLocationScanHistory.setScanTime(new Date());
            modelService.save(blInventoryLocationScanHistory);
            modelService.refresh(blInventoryLocationScanHistory);
        } else {
            failedBarcodeList.add(iteratorBarcode);
        }
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

    public void setBlInventoryScanToolDao(BlInventoryScanToolDao blInventoryScanToolDao) {
        this.blInventoryScanToolDao = blInventoryScanToolDao;
    }
}
