package com.bl.core.service.impl;

import com.bl.core.service.BlBackOfficePriceService;
import com.bl.core.strategies.BlProductDynamicPriceStrategy;
import com.bl.core.util.BlPriceRatioUtil;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.product.ProductModel;
import java.math.BigDecimal;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.Map;
import org.apache.commons.collections.MapUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * DefaultBlBackOfficePriceService class is used to get product dynamic price
 *
 * @author Kalyan Kumar
 */

public class DefaultBlBackOfficePriceService implements BlBackOfficePriceService {

  private BlProductDynamicPriceStrategy blProductDynamicPriceStrategy;
  private BlPriceRatioUtil blProductPriceRatioUtil;
  private static final Logger LOG = Logger.getLogger(DefaultBlBackOfficePriceService.class);

  /**
   *  {@inheritDoc}  
   */
  @Override
  public BigDecimal getProductPrice(ProductModel productModel,Date arrivalDate,Date returnDate) throws ParseException {
    Map<Integer, BigDecimal> priceList ;
    if(productModel != null)
    {
      // Prepare map for BL described Price info
      priceList=getBlProductPriceRatioUtil().getPriceRatios(productModel);
      //((PriceRowModel) ((List)productModel.getEurope1Prices()).get(0)).getDuration()
      if(MapUtils.isNotEmpty(priceList))
      {
        // convert String format time to LocalDate type
        LocalDate arrDate = arrivalDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        LocalDate retDate = returnDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();

        long d = ChronoUnit.DAYS.between(arrDate, retDate);
        BlLogger.logMessage(LOG, Level.INFO, "##### Arrival Date :"+arrivalDate+" Return Date :"+returnDate+" Rental Days :"+d+" #####");
        if (priceList.containsKey((int) d)) {
          return priceList.get((int) d);
        } else {
          return getBlProductDynamicPriceStrategy().getDynamicPrice(priceList, d);
        }
      }
    }
    BlLogger.logMessage(LOG, Level.WARN, "!Rental Price not found");
    return null;
  }

  public BlProductDynamicPriceStrategy getBlProductDynamicPriceStrategy() {
    return blProductDynamicPriceStrategy;
  }

  public void setBlProductDynamicPriceStrategy(
      BlProductDynamicPriceStrategy blProductDynamicPriceStrategy) {
    this.blProductDynamicPriceStrategy = blProductDynamicPriceStrategy;
  }

  public BlPriceRatioUtil getBlProductPriceRatioUtil() {
    return blProductPriceRatioUtil;
  }

  public void setBlProductPriceRatioUtil(BlPriceRatioUtil blProductPriceRatioUtil) {
    this.blProductPriceRatioUtil = blProductPriceRatioUtil;
  }

}