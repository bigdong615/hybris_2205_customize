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
import java.util.Collection;
import java.util.HashSet;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

/**
 * This class is for setting the auto generated product Id on BlProduct
 * when it is created and has no productID associated to it
 * Creating rental prices on product based on retail price
 *
 * @author Ritika
 */
public class BlProductPrepareInterceptor implements PrepareInterceptor<BlProductModel> {

  private KeyGenerator keyGenerator;
  private EnumerationService enumerationService;
  private BlPricingService blPricingService;

  @Override
  public void onPrepare(final BlProductModel blProductModel, final InterceptorContext interceptorContext)
      throws InterceptorException {

    if( interceptorContext.isNew(blProductModel) && StringUtils.isBlank(blProductModel.getProductId())){
      blProductModel.setProductId(getKeyGenerator().generate().toString());
    }
    createRentalBlProductPrices(blProductModel);
  }


  /**
   * Create rental prices for BlProduct
   * @param blProductModel
   */
  private void createRentalBlProductPrices(final BlProductModel blProductModel) {

    Collection<PriceRowModel> prices = blProductModel.getEurope1Prices();
    Collection<PriceRowModel> newPrices = new HashSet<>(prices);
    if (CollectionUtils.isNotEmpty(prices)) {
      PriceRowModel sevenDayPriceRow;
      Optional<PriceRowModel> retailPriceRow = prices.stream().filter(priceRow -> DurationEnum.RETAIL.equals(priceRow.getDuration())).findAny();
      Optional<PriceRowModel> sevenDayPrice = prices.stream().filter(priceRow -> getEnumerationService().getEnumerationValue(DurationEnum.class,
          BlCoreConstants.SEVEN_DAY_PRICE).equals(priceRow.getDuration())).findAny();
      if (sevenDayPrice.isEmpty() && retailPriceRow.isPresent()) {
        sevenDayPriceRow = getBlPricingService()
            .createOrUpdateSevenDayPrice(blProductModel, retailPriceRow.get().getPrice(), true);
        if (null != sevenDayPriceRow) {
          newPrices.add(sevenDayPriceRow);
          newPrices = getBlPricingService()
              .createOrUpdateFixedDurationPrices(blProductModel, sevenDayPriceRow.getPrice(), true);
        }
      }
     blProductModel.setEurope1Prices(newPrices);

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
