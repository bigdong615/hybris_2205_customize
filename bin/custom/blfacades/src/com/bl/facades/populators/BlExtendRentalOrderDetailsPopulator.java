package com.bl.facades.populators;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.price.service.BlCommercePriceService;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.session.SessionService;
import java.math.BigDecimal;

public class BlExtendRentalOrderDetailsPopulator <SOURCE extends OrderModel, TARGET extends OrderData> implements
    Populator<SOURCE, TARGET> {

  private PriceDataFactory priceDataFactory;
  private BlCommercePriceService commercePriceService;


  protected SessionService sessionService;

  @Override
  public void populate(final OrderModel source, final OrderData target) throws ConversionException {

    int defaultAddedTimeForExtendRental = 1; // Default value
    target.setAddedTimeForExtendRental(defaultAddedTimeForExtendRental); // Default value which added for extend order
     PriceDataType priceType = PriceDataType.BUY;
     PriceInformation info;
     BigDecimal subTotal = BigDecimal.valueOf(0.0);

     for(AbstractOrderEntryModel entries : source.getEntries()) {
       info = getCommercePriceService().getWebPriceForExtendProduct(entries.getProduct() ,
           (long) defaultAddedTimeForExtendRental);
       if (info != null) {
         subTotal = subTotal.add(BigDecimal.valueOf(info.getPriceValue().getValue()).setScale(BlCoreConstants.DECIMAL_PRECISION, BlCoreConstants.ROUNDING_MODE));
       }

     }
     target.setTotalCostForExtendRental(getPriceDataFactory().create(priceType, subTotal , source.getCurrency().getIsocode()));
  }

  public PriceDataFactory getPriceDataFactory() {
    return priceDataFactory;
  }

  public void setPriceDataFactory(
      PriceDataFactory priceDataFactory) {
    this.priceDataFactory = priceDataFactory;
  }


  public BlCommercePriceService getCommercePriceService() {
    return commercePriceService;
  }

  public void setCommercePriceService(
      BlCommercePriceService commercePriceService) {
    this.commercePriceService = commercePriceService;
  }

  public SessionService getSessionService() {
    return sessionService;
  }

  public void setSessionService(SessionService sessionService) {
    this.sessionService = sessionService;
  }


}
