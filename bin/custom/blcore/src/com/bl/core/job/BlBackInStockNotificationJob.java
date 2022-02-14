package com.bl.core.job;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.core.model.ItemModel;
import de.hybris.platform.customerinterestsservices.model.ProductInterestModel;
import de.hybris.platform.stocknotificationservices.cronjob.StockLevelStatusJob;
import de.hybris.platform.stocknotificationservices.cronjob.StockNotificationTask;
import de.hybris.platform.store.BaseStoreModel;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import org.apache.commons.collections4.CollectionUtils;

/**
 * @author vijay vishwakarma
 * This job is created to run back in stock job.
 */
public class BlBackInStockNotificationJob extends StockLevelStatusJob {

    /**
     *  This method is override to check product in stock status.
     * @param productInterest
     * @param now
     * @return
     */
    @Override
    public boolean isProductInStock(final ProductInterestModel productInterest, final Date now){

        final BlProductModel product = (BlProductModel)productInterest.getProduct();
        if (product == null)
        {
            modelService.remove(productInterest);
            return false;
        }
        final Collection<BlSerialProductModel> blSerialProducts = product.getSerialProducts();
        final BaseStoreModel currentBaseStore = productInterest.getBaseStore();
        boolean hasActiveSerialProduct= false;
        if(CollectionUtils.isNotEmpty(blSerialProducts) && currentBaseStore!=null && CollectionUtils.isNotEmpty(currentBaseStore.getWarehouses())){
            hasActiveSerialProduct = blSerialProducts.stream().anyMatch(blSerialProductModel ->
                    blSerialProductModel.getSerialStatus() != null && blSerialProductModel.getSerialStatus().equals(SerialStatusEnum.ACTIVE)
            );

        }
        return hasActiveSerialProduct;
    }

    @Override
    protected StockNotificationTask createTask(final Map<String, ItemModel> data)
    {
        return new BlStockNotificationTask(getNotificationService(), modelService, data);
    }

}
