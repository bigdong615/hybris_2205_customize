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
  public void onPrepare(final BlSerialProductModel blSerialProduct, final InterceptorContext ctx)
      throws InterceptorException {
    calculateForSalePriceForSerial(blSerialProduct, ctx);
    calculateIncentivizedPriceForSerial(blSerialProduct, ctx);
  }

  /**
   * Calculate For Sale price for serial products
   *
   * @param blSerialProduct
   * @param ctx
   */
  private void calculateForSalePriceForSerial(final BlSerialProductModel blSerialProduct,
      final InterceptorContext ctx) {
    if (BooleanUtils.isTrue(blSerialProduct.getForSale()) && hasForSaleBaseAndConditionalRating(
        blSerialProduct)) {
      if (ctx.isNew(blSerialProduct) || blSerialProduct.getFinalSalePrice() == null || ctx
          .isModified(blSerialProduct, BlSerialProductModel.FORSALEBASEPRICE)
          || ctx.isModified(blSerialProduct, BlSerialProductModel.CONDITIONRATINGOVERALLSCORE)) {
        blSerialProduct.setFinalSalePrice(
            getBlPricingService().calculateSerialForSalePrice(blSerialProduct.getForSaleBasePrice(),
                blSerialProduct.getConditionRatingOverallScore()));
      }
    }
  }

  /**
   * Calculate incentivized price based on the finalSalePrice
   *
   * @param blSerialProduct
   * @param ctx
   */
  private void calculateIncentivizedPriceForSerial(BlSerialProductModel blSerialProduct,
      InterceptorContext ctx) {
    if(blSerialProduct.getFinalSalePrice() != null && blSerialProduct.getForSaleDiscount() != null) {
      final BigDecimal finalSalePrice = blSerialProduct.getFinalSalePrice()
          .setScale(2, RoundingMode.DOWN);
      final Integer forSaleDiscount = blSerialProduct.getForSaleDiscount();
      if (finalSalePrice.compareTo(BigDecimal.ZERO) > 0 && forSaleDiscount > 0 && (
          blSerialProduct.getIncentivizedPrice().compareTo(BigDecimal.ZERO) == 0 || ctx
              .isModified(blSerialProduct, BlSerialProductModel.FINALSALEPRICE) || ctx
              .isModified(blSerialProduct, BlSerialProductModel.FORSALEDISCOUNT))) {
        blSerialProduct.setIncentivizedPrice(
            finalSalePrice.multiply(new BigDecimal(forSaleDiscount)).divide(new BigDecimal(100))
                .setScale(2, RoundingMode.DOWN));
      }
    }
  }


  /**
   * Check if the sale price is not null and greater than zero and conditional rating is also not
   * zero
   *
   * @param blSerialProduct
   * @return
   */
  private boolean hasForSaleBaseAndConditionalRating(final BlSerialProductModel blSerialProduct) {
    final Double conditionRatingOverallScore = blSerialProduct.getConditionRatingOverallScore();
    final BigDecimal forSaleBasePrice = blSerialProduct.getForSaleBasePrice();

    return forSaleBasePrice != null && conditionRatingOverallScore != null
        && forSaleBasePrice.compareTo(BigDecimal.ZERO) > 0 && conditionRatingOverallScore
        > 0.0D;
  }

  public BlPricingService getBlPricingService() {
    return blPricingService;
  }

  public void setBlPricingService(BlPricingService blPricingService) {
    this.blPricingService = blPricingService;
  }
}
