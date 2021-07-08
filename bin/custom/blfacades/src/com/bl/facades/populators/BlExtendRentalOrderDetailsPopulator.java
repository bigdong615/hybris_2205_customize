package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commerceservices.price.CommercePriceService;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.jalo.order.price.PriceInformation;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;

public class BlExtendRentalOrderDetailsPopulator <SOURCE extends OrderModel, TARGET extends OrderData> implements
    Populator<SOURCE, TARGET> {

  private PriceDataFactory priceDataFactory;

  private CommercePriceService commercePriceService;

  @Override
  public void populate(final OrderModel source, final OrderData target) throws ConversionException {
    int defaultAddedTimeForExtendRental = 1;

    target.setAddedTimeForExtendRental(defaultAddedTimeForExtendRental); // Default value which added for extend order
     PriceDataType priceType = PriceDataType.BUY;
     PriceInformation info;

     for(AbstractOrderEntryModel entries : source.getEntries()) {
       info = getCommercePriceService().getWebPriceForProduct(entries.getProduct());
       if (info != null) {
         final PriceData priceData = getPriceDataFactory().create(priceType, BigDecimal
                 .valueOf(info.getPriceValue().getValue()),
             info.getPriceValue().getCurrencyIso());
         for(OrderEntryData orderEntries : target.getEntries()){
            if(orderEntries.getProduct().getCode().equalsIgnoreCase(entries.getProduct().getCode())) {
                     orderEntries.getProduct().setPrice(priceData);
            }
         }
       }

     }
  }

  public PriceDataFactory getPriceDataFactory() {
    return priceDataFactory;
  }

  public void setPriceDataFactory(
      PriceDataFactory priceDataFactory) {
    this.priceDataFactory = priceDataFactory;
  }


  public CommercePriceService getCommercePriceService() {
    return commercePriceService;
  }

  public void setCommercePriceService(
      CommercePriceService commercePriceService) {
    this.commercePriceService = commercePriceService;
  }


}
