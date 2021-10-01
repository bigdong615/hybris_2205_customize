package com.bl.core.util;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.BlStandardPricingRatioDao;
import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.model.product.ProductModel;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.europe1.model.PriceRowModel;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * BlPriceRatioUtil class is used to get product dynamic prices based on predefined price ratios
 *
 * @author Kalyan Kumar
 */

public class BlPriceRatioUtil {

  private BlStandardPricingRatioDao blStandardPricingRatioDao;
  private EnumerationService enumerationService;
  private static final Logger LOG = Logger.getLogger(BlPriceRatioUtil.class);


  /**
   * getPriceRatios method is used to prepare Map which contains no of days as key
   * and calculated price based ratio as value
   *
   * @param productModel used to get product
   * @return Map<Integer, BigDecimal> , contains calculated prices based on price ratios
   *
   */
  public Map<Integer, BigDecimal> getPriceRatios(ProductModel productModel)
  {

    Map<Integer, BigDecimal> priceList = new HashMap<>();
    // get BL Price Ratio
    Optional<PriceRowModel> basePriceRow= Optional.empty();
    if(productModel!=null && CollectionUtils.isNotEmpty(productModel.getEurope1Prices()))
    {
      basePriceRow = productModel.getEurope1Prices().stream().filter(pr->pr.getDuration()!=null && pr.getDuration().equals(getEnumerationService().getEnumerationValue(DurationEnum.class,"7"))).findAny();
    }
    List<BlStandardPricingRatioModel> priceRatio = getBlStandardPricingRatioDao().getStandardPricingRatio();
    if(basePriceRow.isPresent() && CollectionUtils.isNotEmpty(priceRatio))
    {
      // calculate Rental price rows based on number of days and save it on product price rows
      for (BlStandardPricingRatioModel pr : priceRatio)
      {
        priceList.put(Integer.parseInt(pr.getDuration().toString()), (BigDecimal.valueOf(basePriceRow.get().getPrice()).multiply(BigDecimal.valueOf(pr.getPricingRatio()))).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
      }
      return priceList;
    }
    BlLogger.logFormatMessageInfo(LOG,Level.WARN,"! priceList is not created, check product {} price rows :",productModel);
    return null;
  }

  public BlStandardPricingRatioDao getBlStandardPricingRatioDao() {
    return blStandardPricingRatioDao;
  }

  public void setBlStandardPricingRatioDao(BlStandardPricingRatioDao blStandardPricingRatioDao) {
    this.blStandardPricingRatioDao = blStandardPricingRatioDao;
  }

  public EnumerationService getEnumerationService() {
    return enumerationService;
  }

  public void setEnumerationService(EnumerationService enumerationService) {
    this.enumerationService = enumerationService;
  }

}
