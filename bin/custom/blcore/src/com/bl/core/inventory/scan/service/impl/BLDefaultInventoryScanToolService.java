package com.bl.core.inventory.scan.service.impl;

import com.bl.constants.BLInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BLInventoryScanToolDao;
import com.bl.core.inventory.scan.service.BLInventoryScanToolService;
import com.bl.core.jalo.BLInventoryLocation;
import com.bl.core.jalo.BLInventoryLocationScanHistory;
import com.bl.core.jalo.BlSerialProduct;
import com.bl.facades.inventoryScan.model.BlInventoryScanResultData;
import de.hybris.platform.core.Constants;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.util.Config;
import org.springframework.beans.factory.annotation.Autowired;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * {javadoc}
 *
 * @author Namrata Lohar
 **/
public class BLDefaultInventoryScanToolService implements BLInventoryScanToolService {

    @Autowired
    UserService userService;

    @Autowired
    private ModelService modelService;

    @Resource(name = "blInventoryScanToolDao")
    BLInventoryScanToolDao blInventoryScanToolDao;

    private BLInventoryLocation blInventoryLocation;

    private BlInventoryScanResultData blInventoryScanResultData;

    public BLInventoryLocation getBlInventoryLocation() {
        return blInventoryLocation;
    }

    public void setBlInventoryLocation(BLInventoryLocation blInventoryLocation) {
        this.blInventoryLocation = blInventoryLocation;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean validateEmployeeByUsernameAndPassword(String username, String password) {
        EmployeeModel employee = userService.getUserForUID(username, EmployeeModel.class);
        if (!userService.isMemberOfGroup(employee, userService.getUserGroupForUID(Constants.USER.EMPLOYEE_USERGROUP))) {
            return employee != null && (username.equals(employee.getUid()) && password.equals(employee.getEncodedPassword()));
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BLInventoryLocation getInventoryLocationById(String locationId) {
        return blInventoryScanToolDao.getInventoryLocationById(locationId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BlSerialProduct getSerialProductByBarcode(String barcode) {
        return blInventoryScanToolDao.getSerialProductByBarcode(barcode);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<BlSerialProduct> getSerialProductsByBarcode(Collection<String> barcode) {
        return blInventoryScanToolDao.getSerialProductsByBarcode(barcode);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BlSerialProduct getSerialProductBySerialId(String serialId) {
        return blInventoryScanToolDao.getSerialProductBySerialId(serialId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BlInventoryScanResultData webScanToolUpdateInventoryLocation(List<String> barcodes) {
        int barcodeSize = barcodes.size();
        if (barcodeSize >= BLInventoryScanLoggingConstants.EIGHT || barcodeSize <= BLInventoryScanLoggingConstants.ONE) {
            return createMaxMinBarcodeScanErrorResponseData(barcodes, barcodeSize);
        } else {
            return createBatchScanResponseData(checkValidLocationInBarcodeList(barcodes), barcodes, barcodeSize);
        }
    }

    /**
     * @param result      of Scanning
     * @param barcodes    of Input List
     * @param barcodeSize from scanned barcode
     * @return BlInventoryScanResultData
     */
    private BlInventoryScanResultData createBatchScanResponseData(int result, List<String> barcodes, int barcodeSize) {
        blInventoryScanResultData = new BlInventoryScanResultData();
        blInventoryScanResultData.setUpdateLocationSuccessStatus(Boolean.FALSE);
        blInventoryScanResultData.setFailedBarcodeList(null);
        blInventoryScanResultData.setScanBarcodeList(barcodes);
        blInventoryScanResultData.setScanBarcodeCount(barcodeSize);
        if (result == BLInventoryScanLoggingConstants.ONE) {
            List<String> failedBarcodeList = new ArrayList<>();
            Collection<BlSerialProduct> blSerialProducts = blInventoryScanToolDao.getSerialProductsByBarcode(barcodes);
            for (String barcode : barcodes) {
                setInventoryLocationOnSerial(failedBarcodeList, blSerialProducts, barcode);
            }
            return createResponseScanData(failedBarcodeList, barcodes);
        } else if (result == BLInventoryScanLoggingConstants.TWO) {
            blInventoryScanResultData.setResponseMessage(BLInventoryScanLoggingConstants.LAST_SCAN_INVALID_ERROR);
        } else if (result == BLInventoryScanLoggingConstants.THREE) {
            blInventoryScanResultData.setResponseMessage(BLInventoryScanLoggingConstants.LAST_SCAN_ERROR);
        } else {
            blInventoryScanResultData.setResponseMessage(BLInventoryScanLoggingConstants.MANY_LOC_ERROR);
        }
        return blInventoryScanResultData;
    }

    /**
     * @param barcodes of Input List
     * @return BlInventoryScanResultData
     */
    private BlInventoryScanResultData createMaxMinBarcodeScanErrorResponseData(List<String> barcodes, int barcodeSize) {
        blInventoryScanResultData = new BlInventoryScanResultData();

        blInventoryScanResultData.setUpdateLocationSuccessStatus(Boolean.FALSE);
        blInventoryScanResultData.setFailedBarcodeList(null);
        blInventoryScanResultData.setScanBarcodeList(barcodes);
        blInventoryScanResultData.setScanBarcodeCount(barcodes.size());
        if(barcodeSize >= BLInventoryScanLoggingConstants.EIGHT)
        {
            blInventoryScanResultData.setResponseMessage(BLInventoryScanLoggingConstants.MAX_BARCODE_LIMIT_ERROR
                    + Config.getParameter("bl.inventory.scan.maxSequenceScan.limit"));
        } else {
            blInventoryScanResultData.setResponseMessage(BLInventoryScanLoggingConstants.MUST_2_ERROR
                    + BLInventoryScanLoggingConstants.SCAN_DATA_ERROR + barcodes);
        }
        return blInventoryScanResultData;
    }

    /**
     * @param failedBarcodeList from scanned barcode list
     * @param blSerialProducts  from barcodes
     * @param iteratorBarcode   current iterator
     */
    private void setInventoryLocationOnSerial(List<String> failedBarcodeList, Collection<BlSerialProduct> blSerialProducts, String iteratorBarcode) {
        BlSerialProduct blSerialProduct = blSerialProducts.stream().filter(p -> p.getBarcode().equals(iteratorBarcode)).findFirst().orElse(null);
        if (blSerialProduct != null) {
            blSerialProduct.setSerialInventoryLocation(getBlInventoryLocation());
            blSerialProduct.setSerialLastLocation(getBlInventoryLocation());
            blSerialProduct.setSerialLastParentLocation(getBlInventoryLocation().getParentInventoryLocation());
            modelService.save(blSerialProduct);
            modelService.refresh(blSerialProduct);

            /* Scan History Entry*/
            BLInventoryLocationScanHistory blInventoryLocationScanHistory = new BLInventoryLocationScanHistory();
            blInventoryLocationScanHistory.setSerialProduct(blSerialProduct);
            blInventoryLocationScanHistory.setScanUser(modelService.getSource(userService.getCurrentUser()));
            blInventoryLocationScanHistory.setInventoryLocation(getBlInventoryLocation());
            blInventoryLocationScanHistory.setScanTime(new Date());
            modelService.save(blInventoryLocationScanHistory);
            modelService.refresh(blInventoryLocationScanHistory);
        } else {
            failedBarcodeList.add(iteratorBarcode);
        }
    }

    /**
     * @param failedBarcodeList from scanned barcode
     * @param barcodes          input list
     * @return BlInventoryScanResultData
     */
    private BlInventoryScanResultData createResponseScanData(List<String> failedBarcodeList, List<String> barcodes) {
        blInventoryScanResultData = new BlInventoryScanResultData();
        int barcodesSize = barcodes.size();
        if (failedBarcodeList != null) {
            blInventoryScanResultData.setUpdateLocationSuccessStatus(Boolean.FALSE);
            blInventoryScanResultData.setFailedBarcodeList(failedBarcodeList);
            blInventoryScanResultData.setResponseMessage(BLInventoryScanLoggingConstants.SCAN_ERROR_FOR_BATCH + failedBarcodeList);
        } else {
            blInventoryScanResultData.setUpdateLocationSuccessStatus(Boolean.TRUE);
            blInventoryScanResultData.setFailedBarcodeList(null);
            blInventoryScanResultData.setResponseMessage(BLInventoryScanLoggingConstants.SCAN_SUCCESS + barcodesSize);
        }
        blInventoryScanResultData.setScanBarcodeList(barcodes);
        blInventoryScanResultData.setScanBarcodeCount(barcodesSize);
        return blInventoryScanResultData;
    }

    /**
     * @param barcodes from input list
     * @return int for success/error message
     */
    private int checkValidLocationInBarcodeList(List<String> barcodes) {
        List<String> defaultLocations = BLInventoryScanLoggingConstants.getDefaultInventoryLocation();
        List<String> filteredLocationList = Stream.of(defaultLocations.toArray(new String[BLInventoryScanLoggingConstants.ZERO]))
                .filter(str -> defaultLocations.stream().anyMatch(str::startsWith))
                .collect(Collectors.toList());
        return checkValidInventoryLocation(barcodes.get(barcodes.size() - BLInventoryScanLoggingConstants.ONE), filteredLocationList);
    }

    /**
     * @param inventoryLocation    for update
     * @param filteredLocationList all locations in batch
     * @return int for success/error message
     */
    private int checkValidInventoryLocation(String inventoryLocation, List<String> filteredLocationList) {
        if ((filteredLocationList != null && !filteredLocationList.isEmpty())) {
            if (filteredLocationList.size() == BLInventoryScanLoggingConstants.ONE) {
                return validateLocation(inventoryLocation, filteredLocationList);
            }
            return BLInventoryScanLoggingConstants.FOUR;
        }
        return BLInventoryScanLoggingConstants.THREE;
    }

    /**
     * @param inventoryLocation    for update
     * @param filteredLocationList all locations in batch
     * @return int for success/error message
     */
    private int validateLocation(String inventoryLocation, List<String> filteredLocationList) {
        if (filteredLocationList.get(BLInventoryScanLoggingConstants.ZERO).equals(inventoryLocation)) {
            BLInventoryLocation blLocalInventoryLocation = blInventoryScanToolDao.getInventoryLocationById(inventoryLocation);
            if (blLocalInventoryLocation != null) {
                setBlInventoryLocation(blLocalInventoryLocation);
                return BLInventoryScanLoggingConstants.ONE;
            }
            return BLInventoryScanLoggingConstants.TWO;
        }
        return BLInventoryScanLoggingConstants.THREE;
    }
}
