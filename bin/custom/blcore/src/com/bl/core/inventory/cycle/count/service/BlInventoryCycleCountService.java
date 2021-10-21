package com.bl.core.inventory.cycle.count.service;

import com.bl.core.model.*;

import java.util.Collection;
import java.util.Date;
import java.util.Optional;

/**
 * ICC Service
 *
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
     *
     * @return true if cycle count creation completed
     */
    boolean createNextInventoryCycleCount();

    /**
     * This method will give present SKU list to be get scanned
     *
     * @return ICC details with skus
     */
    Optional<BlInventoryCycleCountDetailsModel> getAllActiveSKUs();

    /**
     * This method will check if scanned SKU and cycle count SKU are equal or not
     *
     * @param inputList scannedList
     * @return true if equal
     */
    boolean checkIsSKUListMatching(final Collection<String> inputList);

    /**
     * This method will execute flow for report generation
     * @param serialBarcodes barcodes
     * @return string message
     * @return String scanHistory result
     */
    String executeInventoryCycleCount(final Collection<String> serialBarcodes);

    /**
     * This method will return day code
     *
     * @return code
     */
    String getInventoryDayCode();

    /**
     * This method will set day code
     *
     * @param inventoryDayCode code
     */
    void setInventoryDayCode(final String inventoryDayCode);

    /**
     * This method will return date
     *
     * @return date
     */
    Date getInventoryDayDate();

    /**
     * This method will set date
     *
     * @param inventoryDayDate code
     */
    void setInventoryDayDate(final Date inventoryDayDate);
}
