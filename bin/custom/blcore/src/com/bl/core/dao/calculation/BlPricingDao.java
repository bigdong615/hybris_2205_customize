package com.bl.core.dao.calculation;

import com.bl.core.enums.DurationEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlPricingLogicModel;
import com.bl.core.model.BlProductModel;
import de.hybris.platform.europe1.model.PriceRowModel;
import java.util.List;

/**
 * This interface is
 * created for duration prices and retrieving pricing logic based on product Type
 *
 * @author Ritika
 */
public interface BlPricingDao {

  /**
   * Gets blPricing by product type.
   *
   * @param productTypeEnum the product type enum
   * @return the Blpricing by product type
   */
  List<BlPricingLogicModel> getBlPricingByProductType(final ProductTypeEnum productTypeEnum);

  /**
   * Get price row by duration.
   *
   * @param duration the duration
   * @param blProductModel
   * @return the price row model
   */
  PriceRowModel getPriceRowByDuration(final DurationEnum duration,final BlProductModel blProductModel);

}
