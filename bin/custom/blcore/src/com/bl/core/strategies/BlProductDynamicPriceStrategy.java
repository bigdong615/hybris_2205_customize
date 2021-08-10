package com.bl.core.strategies;

import java.math.BigDecimal;
import java.util.Map;

/**
 * BlProductDynamicPriceStrategy interface is used to calculate dynamic price of a product
 * if it is not falling into predefined price ratio
 *
 * @author Kalyan Kumar
 */
public interface BlProductDynamicPriceStrategy
{
  /**
   * getDynamicPrice method is used to get product dynamic price based on given no of days
   *
   * @param priceList which is a Map contains no of days as key and rental price as value
   * @param rentalDays  used to get actual rental days
   * @return BigDecimal which is actual rental price of a product
   *
   */
  BigDecimal getDynamicPrice(final Map<Integer, BigDecimal> priceList, final long rentalDays);
}
