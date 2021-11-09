package com.bl.core.model.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.DurationEnum;
import com.bl.core.enums.ProductTypeEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.services.calculation.BlPricingService;
import com.bl.logging.BlLogger;
import de.hybris.platform.catalog.CatalogVersionService;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.europe1.model.PriceRowModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Value;

/**
 * This class is for setting the auto generated product Id on BlProduct when it is created and has
 * no productID associated to it Creating rental prices on product based on retail price
 *
 * @author Ritika
 */
public class BlProductPrepareInterceptor implements PrepareInterceptor<BlProductModel> {

  private static final Logger LOG = Logger.getLogger(BlProductPrepareInterceptor.class);

  private KeyGenerator keyGenerator;
  private EnumerationService enumerationService;
  private CatalogVersionService catalogVersionService;
  private BlPricingService blPricingService;
  @Value("${excluded.product.type.enum.list}")
  private String excludedProducts;

  @Override
  public void onPrepare(final BlProductModel blProductModel,final InterceptorContext interceptorContext) throws InterceptorException {

    Collection<BlSerialProductModel> serialProducts = blProductModel.getSerialProducts();

    if (interceptorContext.isNew(blProductModel) && StringUtils
        .isBlank(blProductModel.getProductId()) && !blProductModel.getCatalogVersion().equals(getCatalogVersionService().getCatalogVersion(BlCoreConstants.BL_PRODUCTCATALOG,BlCoreConstants.CATALOG_VERSION_NAME)))  {
      blProductModel.setProductId(getKeyGenerator().generate().toString());
    }
    createOrUpdateRentalBlProductPrice(blProductModel, interceptorContext);

    if (CollectionUtils.isNotEmpty(serialProducts)) {
      if (interceptorContext.isModified(blProductModel, BlProductModel.FORSALEBASEPRICE)) {
        calculateFinalSalePriceForSerialProducts(blProductModel, serialProducts,
            interceptorContext);
      }
      if (interceptorContext.isModified(blProductModel, BlProductModel.FORSALEDISCOUNT)) {
        calculateIncentivizedPriceForSerialProducts(blProductModel, serialProducts,
            interceptorContext);
      }
    }
  }


  /**
   * Calculate final sale base prices for all serial products
   * @param blProductModel
   * @param serialProducts
   * @param interceptorContext
   */
  private void calculateFinalSalePriceForSerialProducts(final BlProductModel blProductModel,final Collection<BlSerialProductModel> serialProducts,final InterceptorContext interceptorContext) {
    final BigDecimal forSaleBasePrice = blProductModel.getForSaleBasePrice();
    serialProducts.forEach(serialProduct-> {
       if(null != forSaleBasePrice && forSaleBasePrice.compareTo(BigDecimal.ZERO) > 0  && null != serialProduct.getConditionRatingOverallScore() && serialProduct.getConditionRatingOverallScore() > 0.0D) {
          serialProduct.setFinalSalePrice(getBlPricingService()
              .calculateFinalSalePriceForSerial(forSaleBasePrice,
                  serialProduct.getConditionRatingOverallScore()));
        }
       else{
         serialProduct.setFinalSalePrice(null);
       }
          serialProduct.setBlProduct(blProductModel);
           interceptorContext.getModelService().save(serialProduct);

      });
  }

  /**
   * Calculate incentivized price for all serial products
   * @param blProductModel
   * @param serialProducts
   * @param interceptorContext
   */
  private void calculateIncentivizedPriceForSerialProducts(final BlProductModel blProductModel,final Collection<BlSerialProductModel> serialProducts,final InterceptorContext interceptorContext) {
    final Integer forSaleDiscount = blProductModel.getForSaleDiscount();
      serialProducts.stream().forEach( serialProduct -> {
        BigDecimal calculatedIncentivizedPrice = null;
        if(null != forSaleDiscount && forSaleDiscount > 0 && null != serialProduct.getFinalSalePrice() && serialProduct.getFinalSalePrice().compareTo(BigDecimal.ZERO) > 0) {
          final BigDecimal finalSalePrice = serialProduct.getFinalSalePrice()
              .setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE);
         calculatedIncentivizedPrice = finalSalePrice.subtract(
              finalSalePrice.multiply(BigDecimal.valueOf(forSaleDiscount))
                  .divide(BigDecimal.valueOf(BlCoreConstants.DIVIDE_BY_HUNDRED))
                  .setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
          BlLogger.logFormatMessageInfo(LOG, Level.DEBUG,
              "Calculated Incentivized Price is {} for Serial Product {} with For Sale Discount {} and For Sale Final Price {}",
              calculatedIncentivizedPrice, serialProduct.getProductId(), forSaleDiscount.intValue(),
              finalSalePrice.doubleValue());
          serialProduct.setIncentivizedPrice(calculatedIncentivizedPrice);
        }
        else{
          serialProduct.setIncentivizedPrice(null);
        }
        serialProduct.setBlProduct(blProductModel);
        interceptorContext.getModelService().save(serialProduct);
      });

 }

  /**
   * Create or update rental prices for BlProduct based on the available retail price
   *
   * @param blProductModel
   */
  private void createOrUpdateRentalBlProductPrice(final BlProductModel blProductModel,
      final InterceptorContext ctx) {
    Optional<PriceRowModel> sevenDayPrice = blProductModel.getEurope1Prices().stream().filter(
        price -> getEnumerationService()
            .getEnumerationValue(DurationEnum.class, BlCoreConstants.SEVEN_DAY_PRICE)
            .equals(price.getDuration())).findAny();
    final Double retailPrice = blProductModel.getRetailPrice();
    final String[] excludedProductList = excludedProducts.split(",");
    if (retailPrice != null && retailPrice > 0.0D && !(Arrays.asList(excludedProductList).contains(blProductModel.getProductType().getCode())) ) {
      if (sevenDayPrice.isEmpty()) {
        blProductModel.setEurope1Prices(Collections.singletonList(getBlPricingService()
            .createOrUpdateSevenDayPrice(blProductModel, retailPrice, true)));
      } else if (ctx.isModified(blProductModel, BlProductModel.RETAILPRICE)) {
        blProductModel.setRetailPrice(retailPrice);
        blProductModel.setEurope1Prices(Collections.singletonList(getBlPricingService()
            .createOrUpdateSevenDayPrice(blProductModel, retailPrice, false)));
      }

    }
  }

  public KeyGenerator getKeyGenerator() {
    return keyGenerator;
  }

  public void setKeyGenerator(KeyGenerator keyGenerator) {
    this.keyGenerator = keyGenerator;
  }

  public EnumerationService getEnumerationService() {
    return enumerationService;
  }

  public void setEnumerationService(EnumerationService enumerationService) {
    this.enumerationService = enumerationService;
  }

  public BlPricingService getBlPricingService() {
    return blPricingService;
  }

  public void setBlPricingService(BlPricingService blPricingService) {
    this.blPricingService = blPricingService;
  }

  public CatalogVersionService getCatalogVersionService() {
    return catalogVersionService;
  }

  public void setCatalogVersionService(
      CatalogVersionService catalogVersionService) {
    this.catalogVersionService = catalogVersionService;
  }
}
