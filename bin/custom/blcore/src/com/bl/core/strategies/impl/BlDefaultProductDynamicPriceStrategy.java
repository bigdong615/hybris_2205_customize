package com.bl.core.strategies.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.strategies.BlProductDynamicPriceStrategy;
import com.bl.logging.BlLogger;
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.MapUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * The type Bl default product dynamic price strategy.
 *
 * @author Kalyan Kumar
 *
 */
public class BlDefaultProductDynamicPriceStrategy implements BlProductDynamicPriceStrategy {
  private static final Logger LOG = Logger.getLogger(BlDefaultProductDynamicPriceStrategy.class);

  /**
   *  {@inheritDoc}  
   */
  @Override
  public BigDecimal getDynamicPrice(final Map<Integer, BigDecimal> priceList, final long rentalDays) {
    if (MapUtils.isNotEmpty(priceList) && rentalDays > BlCoreConstants.MINIMUM_RENTAL_DAYS) {
      //find nearest lowest day and nearest highest day
      int lowest = 0 ;
      int highest = 0;
      int noOfDays = (int) rentalDays;
      //Sort those days which we are getting from keys
      final Set<Integer> sortedKeys = priceList.keySet().stream().sorted()
          .collect(Collectors.toCollection(LinkedHashSet::new));
      for (final int k : sortedKeys) {
        if (k < noOfDays) {
          lowest = k;
        } else {
          highest = k;
          break;
        }
      }
      final int diffInDays = highest - lowest;
      final BigDecimal diffInPrice = priceList.get(highest).subtract(priceList.get(lowest));
      final BigDecimal perDayPrice = diffInPrice
          .divide(BigDecimal.valueOf(diffInDays), new MathContext(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
      return priceList.get(lowest).setScale(BlCoreConstants.DECIMAL_PRECISION,
          BlCoreConstants.ROUNDING_MODE).add((perDayPrice.multiply((BigDecimal.valueOf(noOfDays - lowest))))
          .setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
    }
    BlLogger.logMessage(LOG, Level.WARN, "!Check rental days");
    return null;
  }
}
