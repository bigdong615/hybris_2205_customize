package com.bl.core.service.impl;

import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.price.service.BlCommercePriceService;
import com.bl.core.service.BlBackOfficePriceService;
import com.bl.core.strategies.BlProductDynamicPriceStrategy;
import com.bl.core.util.BlPriceRatioUtil;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.util.PriceValue;
import java.math.BigDecimal;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.Map;
import javax.annotation.Resource;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
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
  @Resource(name = "commercePriceService")
  private BlCommercePriceService commercePriceService;
  /**
   *  {@inheritDoc}  
   */
  @Override
  public BigDecimal getProductPrice(final ProductModel productModel, final Date arrivalDate,
      final Date returnDate , final boolean isExtendOrder) throws ParseException {
    Preconditions.checkNotNull(productModel);
    if(productModel instanceof BlSerialProductModel)
    {
   	 BlSerialProductModel blSerialProductModel =  ((BlSerialProductModel) productModel);
   	 return blSerialProductModel.getIncentivizedPrice() !=null ? blSerialProductModel.getIncentivizedPrice() : blSerialProductModel.getFinalSalePrice();
     }
    // Prepare map for BL described Price info
    final Map<Integer, BigDecimal> priceList = getBlProductPriceRatioUtil()
        .getPriceRatios(productModel);
    //((PriceRowModel) ((List)productModel.getEurope1Prices()).get(0)).getDuration()
    if (MapUtils.isEmpty(priceList) && !((BlProductModel)productModel).isBundleProduct()) {
      BlLogger.logMessage(LOG, Level.WARN, "! Rental Price not found");
      return BigDecimal.ZERO;
    }

    long daysDiff = 0l;
    // convert String format time to LocalDate type
    if (null != arrivalDate && null != returnDate) {
      final LocalDate arrDate = arrivalDate.toInstant().atZone(ZoneId.systemDefault())
          .toLocalDate();
      final LocalDate retDate = returnDate.toInstant().atZone(ZoneId.systemDefault()).toLocalDate()
          .plusDays(BooleanUtils.isTrue(isExtendOrder) ? 1 : 0);

      daysDiff = ChronoUnit.DAYS.between(arrDate, retDate);
      BlLogger.logFormattedMessage(LOG, Level.INFO, StringUtils.EMPTY,
          "##### Arrival Date : {} Return Date : {} Rental Days : {} ########"
          , arrivalDate.toString(), returnDate.toString(), daysDiff);
    }

    if(((BlProductModel)productModel).isBundleProduct()){
      try {
        final PriceValue priceValue = commercePriceService.getDynamicBasePriceForBundle(productModel,Long.valueOf(daysDiff).intValue());
        return BigDecimal.valueOf(priceValue.getValue());
      }catch(final Exception ex) {
        BlLogger.logFormattedMessage(LOG, Level.DEBUG, StringUtils.EMPTY, ex,
            "##### Some error occur whiling calculating price for bundle product {} whiling saving order ########"
            , productModel.getCode());
        BlLogger.logFormattedMessage(LOG, Level.INFO, StringUtils.EMPTY,
            "##### Did not find any price information for bundle product {} ########"
            , productModel.getCode());
        return null;
      }
    }
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