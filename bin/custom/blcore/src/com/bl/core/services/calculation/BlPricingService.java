package com.bl.core.services.calculation;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.europe1.model.PriceRowModel;
import java.math.BigDecimal;


/**
 * This interface includes all methods for create/update seven day price,
 *  retrieving duration prices and creating/updation fixed duration
 *  prices
 *
 * @author Ritika
 */
public interface BlPricingService {

  /**
   * Create or Update base price for product.
   *
   * @param blProductModel the product
   * @param retailPrice    the retail price
   * @param isNew          isNew price
   * @return the updated/ newly created seven day price
   */
  PriceRowModel createOrUpdateSevenDayPrice(final BlProductModel blProductModel, final Double retailPrice, final boolean isNew);

  /**
   * Gets price row by duration.
   *
   * @param duration       the duration
   * @param blProductModel the bl product model
   * @return the price row by duration
   */
  PriceRowModel getPriceRowByDuration(final String duration,final BlProductModel blProductModel);

  /**
   * Calculate Serial for Sale price based on conditional rating
   *
   * @param forSaleBasePrice the for sale base price
   * @param conditionRatingOverallScore the condition rating overall score
   * @return the big decimal
   */
  BigDecimal calculateFinalSalePriceForSerial(final BigDecimal forSaleBasePrice,final Double conditionRatingOverallScore);

  /**
   * calculate conditional rating on the basis of cosmetic and functional rating.
   *
   * @param cosmeticRating the cosmetic rating
   * @param functionalRating the functional rating
   * @return the calculated conditional rating
   */
  Double getCalculatedConditionalRating(final double cosmeticRating,final double functionalRating);

  /**
   * Get the  promotion price for serial
   *
   * @param serialProductPrice the serial product price
   * @param ugPromotionDiscount the ug promotion discount
   * @return the serial promotion price
   */
  BigDecimal getSerialPromotionPrice(final BigDecimal serialProductPrice,final Integer ugPromotionDiscount);
}
