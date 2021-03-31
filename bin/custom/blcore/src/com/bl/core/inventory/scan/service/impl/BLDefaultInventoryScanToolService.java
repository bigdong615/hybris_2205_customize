package com.bl.core.inventory.scan.service.impl;

import com.bl.constants.BLInventoryScanLoggingConstants;
import com.bl.core.inventory.scan.dao.BLInventoryScanToolDao;
import com.bl.core.inventory.scan.service.BLInventoryScanToolService;
import com.bl.core.model.BLInventoryLocationModel;
import com.bl.core.model.BLInventoryLocationScanHistoryModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.Constants;
import de.hybris.platform.core.model.user.EmployeeModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.springframework.beans.factory.annotation.Autowired;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

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

    private BLInventoryLocationModel blInventoryLocation;

    public BLInventoryLocationModel getBlInventoryLocation() {
        return blInventoryLocation;
    }

    public void setBlInventoryLocation(BLInventoryLocationModel blInventoryLocation) {
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
     * @return
     */
    @Override
    public BLInventoryLocationModel getInventoryLocationById(String locationId) {
        return blInventoryScanToolDao.getInventoryLocationById(locationId);
    }

    /**
     * {@inheritDoc}
     * @return
     */
    @Override
    public BlSerialProductModel getSerialProductByBarcode(String barcode) {
        return blInventoryScanToolDao.getSerialProductByBarcode(barcode);
    }

    /**
     * {@inheritDoc}
     * @return
     */
    @Override
    public Collection<BlSerialProductModel> getSerialProductsByBarcode(Collection<String> barcode) {
        return blInventoryScanToolDao.getSerialProductsByBarcode(barcode);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int checkValidLocationInBarcodeList(List<String> barcodes) {
        List<String> defaultLocations = BLInventoryScanLoggingConstants.getDefaultInventoryLocation();
        List<String> filteredLocationList = barcodes.stream().filter(b -> defaultLocations.stream().anyMatch(b::startsWith)).collect(Collectors.toList());
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
            BLInventoryLocationModel blLocalInventoryLocation = blInventoryScanToolDao.getInventoryLocationById(inventoryLocation);
            if (blLocalInventoryLocation != null) {
                setBlInventoryLocation(blLocalInventoryLocation);
                return BLInventoryScanLoggingConstants.ONE;
            }
            return BLInventoryScanLoggingConstants.TWO;
        }
        return BLInventoryScanLoggingConstants.THREE;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getFailedBarcodeList(List<String> barcodes) {
        List<String> failedBarcodeList = new ArrayList<>();
        List<String> subList = barcodes.subList(0, barcodes.size()-1);
        Collection<BlSerialProductModel> blSerialProducts = blInventoryScanToolDao.getSerialProductsByBarcode(subList);
        for (String barcode : subList) {
            setInventoryLocationOnSerial(failedBarcodeList, blSerialProducts, barcode);
        }
        return failedBarcodeList;
    }

    /**
     * @param failedBarcodeList from scanned barcode list
     * @param blSerialProducts  from barcodes
     * @param iteratorBarcode   current iterator
     */
    private void setInventoryLocationOnSerial(List<String> failedBarcodeList, Collection<BlSerialProductModel> blSerialProducts, String iteratorBarcode) {
        BlSerialProductModel blSerialProduct = blSerialProducts.stream().filter(p -> p.getBarcode().equals(iteratorBarcode)).findFirst().orElse(null);
        if (blSerialProduct != null) {
            blSerialProduct.setSerialInventoryLocation(getBlInventoryLocation());
            blSerialProduct.setSerialLastLocation(getBlInventoryLocation());
            blSerialProduct.setSerialLastParentLocation(getBlInventoryLocation().getParentInventoryLocation());
            modelService.save(blSerialProduct);
            modelService.refresh(blSerialProduct);

            /* Scan History Entry*/
            BLInventoryLocationScanHistoryModel blInventoryLocationScanHistory = new BLInventoryLocationScanHistoryModel();
            blInventoryLocationScanHistory.setSerialProduct(blSerialProduct);
            blInventoryLocationScanHistory.setScanUser(userService.getCurrentUser());
            blInventoryLocationScanHistory.setInventoryLocation(getBlInventoryLocation());
            blInventoryLocationScanHistory.setScanTime(new Date());
            modelService.save(blInventoryLocationScanHistory);
            modelService.refresh(blInventoryLocationScanHistory);
        } else {
            failedBarcodeList.add(iteratorBarcode);
        }
    }
}
