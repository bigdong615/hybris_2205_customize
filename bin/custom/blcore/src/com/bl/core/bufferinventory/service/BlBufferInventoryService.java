package com.bl.core.bufferinventory.service;

import com.bl.core.enums.DurationEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlPricingLogicModel;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.europe1.model.PriceRowModel;
import java.util.List;

/**
 * This class is used to manage the buffer inventory of SKU products
 * @author Moumita
 */
public interface BlBufferInventoryService {

    /**
     * It updates buffer inventory at SKU product level
     */
    public void updateBufferInvProducts();
}
