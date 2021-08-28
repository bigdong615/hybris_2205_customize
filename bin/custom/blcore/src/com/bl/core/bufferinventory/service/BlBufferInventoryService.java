package com.bl.core.bufferinventory.service;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.servicelayer.exceptions.BusinessException;

/**
 * This class is used to manage the buffer inventory of SKU products
 * @author Moumita
 */
public interface BlBufferInventoryService {

    /**
     * It updates buffer inventory at SKU product level
     * @throws BusinessException throws exception when base store is null
     */
    public void updateBufferInvProducts() throws BusinessException;

    /**
     * It checks whether the SKU product is eligible to have buffer inventory as per
     * min qty configured in base store
     * @param minQtyForBufferInventory SKU product to have min qty needed to be part
     *                                 of buffer inventory
     * @param blProductModel bl product
     * @return true if it is eligible to have buffer inventory
     */
    boolean minQtyEligibleForBufferInv(final Integer minQtyForBufferInventory, final BlProductModel blProductModel);
}
