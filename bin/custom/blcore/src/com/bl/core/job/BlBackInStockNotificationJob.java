package com.bl.core.job;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import de.hybris.platform.customerinterestsservices.model.ProductInterestModel;
import de.hybris.platform.stocknotificationservices.cronjob.StockLevelStatusJob;
import de.hybris.platform.store.BaseStoreModel;
import org.apache.commons.collections4.CollectionUtils;

import java.util.Collection;
import java.util.Date;

/**
 * @author vijay vishwakarma
 * This job is created to run back in stock job.
 */
public class BlBackInStockNotificationJob extends StockLevelStatusJob {

    /**
     *  this method is overide oto check product in stock status.
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
        if(CollectionUtils.isNotEmpty(blSerialProducts) && CollectionUtils.isNotEmpty(currentBaseStore.getWarehouses())){
            hasActiveSerialProduct = blSerialProducts.stream().anyMatch(blSerialProductModel ->
                    blSerialProductModel.getSerialStatus() != null && blSerialProductModel.getSerialStatus().equals(SerialStatusEnum.ACTIVE)
            );

        }
        return hasActiveSerialProduct;
    }


}
