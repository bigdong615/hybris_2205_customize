package com.bl.core.inventory.cycle.count.service;

import com.bl.core.model.BlInventoryCycleCountModel;
import com.bl.core.model.BlProductModel;

import java.util.Collection;

/**
 * @author Namrata Lohar
 */
public interface BlInventoryCycleCountService {

    /**
     * this method will return currently active inventory cycle from dao
     *
     * @return InventoryCycleCount details
     */
    BlInventoryCycleCountModel getActiveInventoryCycleCount();

    /**
     * This method will return all active SKUs checking the serials those are in-house and will return in current cycle
     * count from dao
     *
     * @return lit of all active SKU's
     */
    Collection<BlProductModel> getAllActiveSKUsWithSerialStatus();

    /**
     * This method will check, if current cycle count ended or not
     *
     * @param blInventoryCycleCountModel details
     * @return true if ended
     */
    boolean isCurrentCycleEnded(final BlInventoryCycleCountModel blInventoryCycleCountModel);

    /**
     * This method will create next inventory cycle count if existing ended
     */
    void createNextInventoryCycleCount();
}
