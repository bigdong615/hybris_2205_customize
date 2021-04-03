package com.bl.core.services.calculation;

import com.bl.core.model.BlProductModel;
import de.hybris.platform.europe1.model.PriceRowModel;
import java.util.Collection;
import java.util.List;

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

}
