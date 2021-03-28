package com.bl.core.inventory.scan.dao;

import com.bl.core.jalo.BLInventoryLocation;
import com.bl.core.jalo.BlSerialProduct;
import de.hybris.platform.core.model.user.EmployeeModel;

import java.util.Collection;
import java.util.List;

/**
 * {javadoc}
 * @author Namrata Lohar
 */
public interface BLInventoryScanToolDao {

    /**
     * @param locationId for InventoryLocation
     * @return InventoryLocation
     */
    BLInventoryLocation getInventoryLocationById(String locationId);

    /**
     * @param barcode for Serial
     * @return Serial Product
     */
    BlSerialProduct getSerialProductByBarcode(String barcode);

    /**
     * @param barcode for Serial
     * @return List of Serials
     */
    Collection<BlSerialProduct> getSerialProductsByBarcode(Collection<String> barcode);

    /**
     * @param serialId of Serial
     * @return Serial Product
     */
    BlSerialProduct getSerialProductBySerialId(String serialId);

}
