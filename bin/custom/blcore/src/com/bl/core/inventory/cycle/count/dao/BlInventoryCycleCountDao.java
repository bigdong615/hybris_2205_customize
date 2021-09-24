package com.bl.core.inventory.cycle.count.dao;

import com.bl.core.model.BlInventoryCycleCountModel;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.ordersplitting.model.ConsignmentModel;

import java.util.Collection;

/**
 * ICC DAO
 *
 * @author Namrata Lohar
 */
public interface BlInventoryCycleCountDao {

    /**
     * this method will return currently active inventory cycle
     *
     * @return InventoryCycleCount details
     */
    BlInventoryCycleCountModel getActiveInventoryCycleCount();

    /**
     * This method will return all active SKUs checking the serials those are in-house and will return in current cycle count
     *
     * @return lit of all active SKU's
     */
    Collection<BlProductModel> getAllActiveSKUsWithSerialStatus();
}
