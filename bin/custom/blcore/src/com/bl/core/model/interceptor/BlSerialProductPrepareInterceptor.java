package com.bl.core.model.interceptor;

import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.calculation.BlPricingService;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import java.math.BigDecimal;

public class BlSerialProductPrepareInterceptor implements PrepareInterceptor<BlSerialProductModel> {

  private BlPricingService blPricingService;

  @Override
  public void onPrepare(final BlSerialProductModel blSerialProduct,final InterceptorContext ctx) throws InterceptorException {
    if(ctx.isNew(blSerialProduct) && hasForSaleBaseAndConditionalRating(blSerialProduct)){
        blSerialProduct.setFinalSalePrice(getBlPricingService().calculateSerialForSalePrice(blSerialProduct.getForSaleBasePrice(),blSerialProduct.getConditionRatingOverallScore()));
    }
    if( blSerialProduct.getFinalSalePrice() == null || ctx.isModified(blSerialProduct,BlSerialProductModel.FORSALEBASEPRICE) || ctx.isModified(blSerialProduct,BlSerialProductModel.CONDITIONRATINGOVERALLSCORE) && hasForSaleBaseAndConditionalRating(blSerialProduct))
    {
      blSerialProduct.setFinalSalePrice(getBlPricingService().calculateSerialForSalePrice(blSerialProduct.getForSaleBasePrice(),blSerialProduct.getConditionRatingOverallScore()));
    }
  }

  /**
   * Check if the sale price is not null and greater than zero
   * and conditional rating is also not zero
   * @param blSerialProduct
   * @return
   */
  private boolean hasForSaleBaseAndConditionalRating(final BlSerialProductModel blSerialProduct) {
    return blSerialProduct.getForSaleBasePrice().compareTo(BigDecimal.ZERO) != 0  &&  blSerialProduct.getConditionRatingOverallScore() > 0.0D;
  }

  public BlPricingService getBlPricingService() {
    return blPricingService;
  }

  public void setBlPricingService(BlPricingService blPricingService) {
    this.blPricingService = blPricingService;
  }
}
