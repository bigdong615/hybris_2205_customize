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
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.assertj.core.util.Preconditions;

/**
 * DefaultBlBackOfficePriceService class is used to get product dynamic price
 *
 * @author Kalyan Kumar
 */

public class DefaultBlBackOfficePriceService implements BlBackOfficePriceService {

  private BlProductDynamicPriceStrategy blCustomProductDynamicPriceStrategy;
  private BlPriceRatioUtil blProductPriceRatioUtil;
  private static final Logger LOG = Logger.getLogger(DefaultBlBackOfficePriceService.class);

  /**
   *  {@inheritDoc}  
   */
  @Override
  public BigDecimal getProductPrice(final ProductModel productModel, final Date arrivalDate,
      final Date returnDate) throws ParseException {
    Preconditions.checkNotNull(productModel);
    // Prepare map for BL described Price info
    final Map<Integer, BigDecimal> priceList = getBlProductPriceRatioUtil()
        .getPriceRatios(productModel);
    //((PriceRowModel) ((List)productModel.getEurope1Prices()).get(0)).getDuration()
    if (MapUtils.isEmpty(priceList)) {
      BlLogger.logMessage(LOG, Level.WARN, "! Rental Price not found");
      return null;
    }
    // convert String format time to LocalDate type
    final LocalDate arrDate = arrivalDate.toInstant().atZone(ZoneId.systemDefault())
        .toLocalDate();
    final LocalDate retDate = returnDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate().plusDays(1);

    final long daysDiff = ChronoUnit.DAYS.between(arrDate, retDate);
    BlLogger.logFormattedMessage(LOG, Level.INFO, StringUtils.EMPTY,
        "##### Arrival Date : {} Return Date : {} Rental Days : {} ########"
        , arrivalDate.toString(), returnDate.toString(),daysDiff);
    if (priceList.containsKey((int) daysDiff)) {
      return priceList.get((int) daysDiff);
    } else {
      return getBlCustomProductDynamicPriceStrategy().getDynamicPrice(priceList, daysDiff);
    }
  }

  public BlPriceRatioUtil getBlProductPriceRatioUtil() {
    return blProductPriceRatioUtil;
  }

  public void setBlProductPriceRatioUtil(BlPriceRatioUtil blProductPriceRatioUtil) {
    this.blProductPriceRatioUtil = blProductPriceRatioUtil;
  }

/**
 * @return the blCustomProductDynamicPriceStrategy
 */
public BlProductDynamicPriceStrategy getBlCustomProductDynamicPriceStrategy()
{
	return blCustomProductDynamicPriceStrategy;
}

/**
 * @param blCustomProductDynamicPriceStrategy the blCustomProductDynamicPriceStrategy to set
 */
public void setBlCustomProductDynamicPriceStrategy(BlProductDynamicPriceStrategy blCustomProductDynamicPriceStrategy)
{
	this.blCustomProductDynamicPriceStrategy = blCustomProductDynamicPriceStrategy;
}

}