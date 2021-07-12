/**
 *
 */
package com.bl.core.model.handler;

import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.calculation.BlPricingService;
import com.bl.logging.BlLogger;


/**
 * This class is responsible to get dynamic value of ConditionRatingOverallScore by calculating Functional and Cosmetic
 * conditions as per the formula provided.
 *
 * @author Ravikumar
 *
 */
public class BlConditionRatingOverallScoreHandler implements
    DynamicAttributeHandler<Double, BlSerialProductModel> {

  private static final Logger LOG = Logger.getLogger(BlConditionRatingOverallScoreHandler.class);

  private BlPricingService blPricingService;

  @Override
  public Double get(final BlSerialProductModel blSerialProductModel) {
    if (Objects.isNull(blSerialProductModel)) {
      BlLogger.logMessage(LOG, Level.ERROR,
          "Cannot evaluate the value for BlSerialProduct.conditionRatingOverallScore because Serial is null");
      return Double.valueOf(0.0d);
    }
    return getConditionOverallRating(blSerialProductModel);
  }

  /**
   * Gets the condition overall rating.
   *
   * @param blSerialProductModel
   *           the bl serial product model
   * @return the condition overall rating
   */
  private Double getConditionOverallRating(final BlSerialProductModel blSerialProductModel) {
    if (isFunctionalAndCosmeticIsAvailable(blSerialProductModel)) {
      final float cosmeticRating = Float
          .parseFloat(blSerialProductModel.getCosmeticRating().getCode());
      final float functionalRating = Float
          .parseFloat(blSerialProductModel.getFunctionalRating().getCode());
      final Double calculatedConditionalRating = getBlPricingService()
          .getCalculatedConditionalRating(cosmeticRating,
              functionalRating);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
          "Serial : {} with Functional Condition Rating : {} and Cosmetic Condition Rating : {} calculated Overall Condition Rating is {}",
          blSerialProductModel.getProductId(), functionalRating, cosmeticRating,
          calculatedConditionalRating);
      return calculatedConditionalRating;
    }
    return Double.valueOf(0.0d);
  }

  /**
   * Checks if functional condition and cosmetic condition is available on serial.
   *
   * @param blSerialProductModel
   *           the bl serial product model
   * @return true, if is functional and cosmetic is available
   */
  private boolean isFunctionalAndCosmeticIsAvailable(
      final BlSerialProductModel blSerialProductModel) {
    boolean isEligible = true;
    if (Objects.isNull(blSerialProductModel.getFunctionalRating())
        || StringUtils.equalsIgnoreCase(blSerialProductModel.getFunctionalRating().getCode(),
        BlCoreConstants.ZERO_RATING)) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
          "Cannot evaluate conditional overall rating because functional rating is null or it is 0 on serial {}",
          blSerialProductModel.getProductId());
      isEligible = false;
    }
    if (Objects.isNull(blSerialProductModel.getCosmeticRating())
        || StringUtils.equalsIgnoreCase(blSerialProductModel.getCosmeticRating().getCode(),
        BlCoreConstants.ZERO_RATING)) {
      BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
          "Cannot evaluate conditional overall rating because cosmetic rating is null or it is 0 on serial {}",
          blSerialProductModel.getProductId());
      isEligible = false;
    }
    return isEligible;
  }

  @Override
  public void set(final BlSerialProductModel blSerialProductModel,
      final Double conditionRatingOverallScore) {
    BlLogger.logMessage(LOG, Level.ERROR,
        "Setter for attribute BlSerialProduct.conditionRatingOverallScore is not supported");
    throw new UnsupportedOperationException();
  }

  /**
   * @return the blPricingService
   */
  public BlPricingService getBlPricingService() {
    return blPricingService;
  }

  /**
   * @param blPricingService
   *           the blPricingService to set
   */
  public void setBlPricingService(final BlPricingService blPricingService) {
    this.blPricingService = blPricingService;
  }

}
