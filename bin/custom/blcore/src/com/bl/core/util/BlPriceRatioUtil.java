package com.bl.core.util;

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

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.dao.BlStandardPricingRatioDao;
import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlConstrainedPricingRatioModel;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlStandardPricingRatioModel;
import com.bl.core.services.pricingratio.BlPricingRatioService;
import com.bl.logging.BlLogger;

/**
 * BlPriceRatioUtil class is used to get product dynamic prices based on predefined price ratios
 *
 * @author Kalyan Kumar
 */

public class BlPriceRatioUtil {

  private BlStandardPricingRatioDao blStandardPricingRatioDao;
  private EnumerationService enumerationService;
  private BlPricingRatioService blPricingRatioService;
  private static final Logger LOG = Logger.getLogger(BlPriceRatioUtil.class);


  /**
   * getPriceRatios method is used to prepare Map which contains no of days as key
   * and calculated price based ratio as value
   *
   * @param productModel used to get product
   * @return Map<Integer, BigDecimal> , contains calculated prices based on price ratios
   *
   */
  public Map<Integer, BigDecimal> getPriceRatios(final ProductModel productModel)
  {

    final Map<Integer, BigDecimal> priceList = new HashMap<>();
    // get BL Price Ratio
    Optional<PriceRowModel> basePriceRow= Optional.empty();
    if(productModel!=null && CollectionUtils.isNotEmpty(productModel.getEurope1Prices()))
    {
      basePriceRow = productModel.getEurope1Prices().stream().filter(pr->pr.getDuration()!=null && pr.getDuration().equals(getEnumerationService().getEnumerationValue(DurationEnum.class,"7"))).findAny();
    }

	 if (productModel instanceof BlProductModel)
	 {
		 final BlProductModel blProductModel = (BlProductModel) productModel;
		 if (blProductModel.getConstrained())
		 {
			 final List<BlConstrainedPricingRatioModel> priceRatio = getBlPricingRatioService().getConstrainedPricingRatio();
			 if (basePriceRow.isPresent() && CollectionUtils.isNotEmpty(priceRatio))
			 {
				 // calculate Rental price rows based on number of days and save it on product price rows
				 for (final BlConstrainedPricingRatioModel pr : priceRatio)
				 {
					 priceList.put(Integer.parseInt(pr.getDuration().toString()),
							 (BigDecimal.valueOf(basePriceRow.get().getPrice()).multiply(BigDecimal.valueOf(pr.getPricingRatio())))
									 .setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
				 }
				 return priceList;
			 }
		 }
		 else
		 {
			 final List<BlStandardPricingRatioModel> priceRatio = getBlStandardPricingRatioDao().getStandardPricingRatio();
			 if (basePriceRow.isPresent() && CollectionUtils.isNotEmpty(priceRatio))
			 {
				 // calculate Rental price rows based on number of days and save it on product price rows
				 for (final BlStandardPricingRatioModel pr : priceRatio)
				 {
					 priceList.put(Integer.parseInt(pr.getDuration().toString()),
							 (BigDecimal.valueOf(basePriceRow.get().getPrice()).multiply(BigDecimal.valueOf(pr.getPricingRatio())))
									 .setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
				 }
				 return priceList;
			 }
		 }
	 }
    BlLogger.logFormatMessageInfo(LOG,Level.WARN,"! priceList is not created, check product {} price rows :",productModel);
    return null;
  }

  public BlStandardPricingRatioDao getBlStandardPricingRatioDao() {
    return blStandardPricingRatioDao;
  }

  public void setBlStandardPricingRatioDao(final BlStandardPricingRatioDao blStandardPricingRatioDao) {
    this.blStandardPricingRatioDao = blStandardPricingRatioDao;
  }

  public EnumerationService getEnumerationService() {
    return enumerationService;
  }

  public void setEnumerationService(final EnumerationService enumerationService) {
    this.enumerationService = enumerationService;
  }

  /**
   * @return the blPricingRatioService
   */
  public BlPricingRatioService getBlPricingRatioService()
  {
	  return blPricingRatioService;
  }

  /**
   * @param blPricingRatioService
   *           the blPricingRatioService to set
   */
  public void setBlPricingRatioService(final BlPricingRatioService blPricingRatioService)
  {
	  this.blPricingRatioService = blPricingRatioService;
  }

}
