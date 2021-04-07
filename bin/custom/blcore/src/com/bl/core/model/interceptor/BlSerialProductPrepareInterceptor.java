package com.bl.core.model.interceptor;

import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.calculation.BlPricingService;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import java.math.BigDecimal;
import java.math.RoundingMode;
import org.apache.commons.lang.BooleanUtils;

public class BlSerialProductPrepareInterceptor implements PrepareInterceptor<BlSerialProductModel> {

  private BlPricingService blPricingService;

  @Override
  public void onPrepare(final BlSerialProductModel blSerialProduct,final InterceptorContext ctx) throws InterceptorException {
    calculateForSalePriceForSerial(blSerialProduct,ctx);
    calculateIncentivizedPriceForSerial(blSerialProduct,ctx);
  }

  /**
   *
   * @param blSerialProduct
   * @param ctx
   */
  private void updateForSalePriceForSerial(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx) {
    if (BooleanUtils.isTrue(blSerialProduct.getForSale()) && hasForSaleBaseAndConditionalRating(blSerialProduct)) {
     if (ctx.isNew(blSerialProduct) || blSerialProduct.getFinalSalePrice() == null || ctx.isModified(blSerialProduct, BlSerialProductModel.FORSALEBASEPRICE)
          || ctx.isModified(blSerialProduct, BlSerialProductModel.CONDITIONRATINGOVERALLSCORE)) {
        blSerialProduct.setFinalSalePrice(getBlPricingService().calculateSerialForSalePrice(blSerialProduct.getForSaleBasePrice(),
                blSerialProduct.getConditionRatingOverallScore()));
      }
    }
  }

  /**
   * Calculate incentivized price based
   * on the finalSalePrice
   * @param blSerialProduct
   * @param ctx
   */
  private void calculateIncentivizedPriceForSerial(BlSerialProductModel blSerialProduct, InterceptorContext ctx) {
    final BigDecimal finalSalePrice = blSerialProduct.getFinalSalePrice().setScale(2, RoundingMode.DOWN);
    final Integer forSaleDiscount = blSerialProduct.getForSaleDiscount();
    if( finalSalePrice.compareTo(BigDecimal.ZERO) > 0 && forSaleDiscount > 0 && (blSerialProduct.getIncentivizedPrice().compareTo(BigDecimal.ZERO) == 0 || ctx
        .isModified(blSerialProduct,BlSerialProductModel.FINALSALEPRICE) || ctx.isModified(blSerialProduct,BlSerialProductModel.FORSALEDISCOUNT))){
      blSerialProduct.setIncentivizedPrice(finalSalePrice.multiply(new BigDecimal(forSaleDiscount)).divide(new BigDecimal(100)).setScale(2,RoundingMode.DOWN));
    }
  }


  /**
   * Check if the sale price is not null and greater than zero
   * and conditional rating is also not zero
   * @param blSerialProduct
   * @return
   */
  private boolean hasForSaleBaseAndConditionalRating(final BlSerialProductModel blSerialProduct) {
    return blSerialProduct.getForSaleBasePrice().compareTo(BigDecimal.ZERO) > 0  &&  blSerialProduct.getConditionRatingOverallScore() > 0.0D;
  }

  public BlPricingService getBlPricingService() {
    return blPricingService;
  }

  public void setBlPricingService(BlPricingService blPricingService) {
    this.blPricingService = blPricingService;
  }
}
