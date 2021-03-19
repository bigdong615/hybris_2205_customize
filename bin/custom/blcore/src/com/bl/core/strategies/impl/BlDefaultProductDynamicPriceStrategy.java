package com.bl.core.strategies.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.strategies.BlProductDynamicPriceStrategy;
import com.bl.logging.BlLogger;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections.MapUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class BlDefaultProductDynamicPriceStrategy implements BlProductDynamicPriceStrategy
{
  private static final Logger LOG = Logger.getLogger(BlDefaultProductDynamicPriceStrategy.class);

  /**
   *  {@inheritDoc}  
   */
  @Override
  public BigDecimal getDynamicPrice(Map<Integer, BigDecimal> priceList, long rentalDays) {
    if(MapUtils.isNotEmpty(priceList) && rentalDays > BlCoreConstants.MINIMUM_RENTAL_DAYS)
    {
      //find nearest lowest day and nearest highest day
      int lowest = 0 ;
      int highest = 0;
      int noOfDays = (int) rentalDays;
      Set<Integer> keys = priceList.keySet();
      //Sort those days which we are getting from keys
      Set<Integer> sortedKeys = keys.stream().sorted().collect(Collectors.toCollection(LinkedHashSet::new));
      for (int k : sortedKeys)
      {
        if (k < noOfDays) {
          lowest = k;
        } else {
          highest = k;
          break;
        }
      }
      int diffInDays = highest - lowest;
      BigDecimal diffInPrice = priceList.get(highest).subtract(priceList.get(lowest));
      BigDecimal perDayPrice = diffInPrice.divide(new BigDecimal(diffInDays), BlCoreConstants.PRECISION, RoundingMode.DOWN);
      return priceList.get(lowest).add(perDayPrice.multiply((new BigDecimal(noOfDays - lowest)))).setScale(BlCoreConstants.PRECISION, RoundingMode.DOWN);
    }
    BlLogger.logMessage(LOG, Level.WARN, "!Check rental days");
    return null;
  }
}
