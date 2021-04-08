package com.bl.core.model.interceptor;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.enums.DurationEnum;
import com.bl.core.model.BlProductModel;
import com.bl.core.services.calculation.BlPricingService;
import de.hybris.platform.enumeration.EnumerationService;
import de.hybris.platform.europe1.model.PriceRowModel;
import de.hybris.platform.servicelayer.interceptor.InterceptorContext;
import de.hybris.platform.servicelayer.interceptor.InterceptorException;
import de.hybris.platform.servicelayer.interceptor.PrepareInterceptor;
import de.hybris.platform.servicelayer.keygenerator.KeyGenerator;
import java.util.Collections;
import java.util.Optional;
import org.apache.commons.lang.StringUtils;

/**
 * This class is for setting the auto generated product Id on BlProduct when it is created and has
 * no productID associated to it Creating rental prices on product based on retail price
 *
 * @author Ritika
 */
public class BlProductPrepareInterceptor implements PrepareInterceptor<BlProductModel> {

  private KeyGenerator keyGenerator;
  private EnumerationService enumerationService;
  private BlPricingService blPricingService;

  @Override
  public void onPrepare(final BlProductModel blProductModel,
      final InterceptorContext interceptorContext)
      throws InterceptorException {

    if (interceptorContext.isNew(blProductModel) && StringUtils
        .isBlank(blProductModel.getProductId())) {
      blProductModel.setProductId(getKeyGenerator().generate().toString());
    }
    createOrUpdateRentalBlProductPrice(blProductModel, interceptorContext);
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
    if (retailPrice != null && retailPrice > 0.0D) {
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

}
