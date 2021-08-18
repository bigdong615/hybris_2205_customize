/**
 *
 */
package com.bl.core.model.handler;

import de.hybris.platform.servicelayer.model.attribute.DynamicAttributeHandler;

import java.util.Objects;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import com.bl.core.model.BlSerialProductModel;
import com.bl.core.product.service.BlProductService;
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
  private BlProductService blProductService; 

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
    if (getBlProductService().isFunctionalAndCosmeticIsAvailable(blSerialProductModel)) {
      final double cosmeticRating = Double
          .parseDouble(blSerialProductModel.getCosmeticRating().getCode());
      final double functionalRating = Double
          .parseDouble(blSerialProductModel.getFunctionalRating().getCode());
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

/**
 * @return the blProductService
 */
public BlProductService getBlProductService()
{
	return blProductService;
}

/**
 * @param blProductService the blProductService to set
 */
public void setBlProductService(BlProductService blProductService)
{
	this.blProductService = blProductService;
}

}
