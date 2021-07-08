package com.bl.facades.populators;

import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commerceservices.constants.GeneratedCommerceServicesConstants.Attributes.Order;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.math.BigDecimal;
import java.util.Date;

public class BlOrderDetailsPopulator <SOURCE extends OrderModel, TARGET extends OrderData> implements
    Populator<SOURCE, TARGET> {



  private PriceDataFactory priceDataFactory;

  @Override
  public void populate(OrderModel source, OrderData target) throws ConversionException {

    target.setIsRentalCart(source.getIsRentalCart());
    if(null != source.getRentalStartDate() && null != source.getRentalEndDate()) {
      target.setRentalStartDate(
          convertDateToString(source.getRentalStartDate(), BlFacadesConstants.RENTAL_DATE_FORMAT));
      target.setRentalEndDate(
          convertDateToString(source.getRentalEndDate(), BlFacadesConstants.RENTAL_DATE_FORMAT));
      target.setRentalFormattedStartDate(convertDateToString(source.getRentalStartDate(),
          BlFacadesConstants.FORMATTED_RENTAL_DATE));
      target.setRentalFormattedEndDate(
          convertDateToString(source.getRentalEndDate(), BlFacadesConstants.FORMATTED_RENTAL_DATE));
      target.setTotalRentalDays(String.valueOf(BlDateTimeUtils
          .getDaysBetweenDates(source.getRentalStartDate(), source.getRentalEndDate()) + 1));
    }
    target.setOrderedDate(convertDateToString(source.getDate(), BlFacadesConstants.FORMATTED_RENTAL_DATE));
    target.setOrderedFormatDate(convertDateToString(source.getDate() , "MMM d , YYYY hh:mm a"));
    target.setTotalDamageWaiverCost(convertDoubleToPriceData(source.getTotalDamageWaiverCost() , source));
   target.setTaxAvalaraCalculated(convertDoubleToPriceData(source.getTotalTax() , source));
   target.setTotalPriceWithTax(convertDoubleToPriceData(source.getTotalPrice(), source));
   final Double discountAmount = source.getTotalDiscounts();
   final Double giftCartAMount = source.getGiftCardAmount();
   final Double totalDisount = discountAmount + giftCartAMount;
   target.setTotalDiscounts(convertDoubleToPriceData(totalDisount, source));
   target.setPickUpPersonEmail(source.getPickUpPersonFirstName());
   target.setPickUpPersonLastName(source.getPickUpPersonLastName());
   target.setPickUpPersonEmail(source.getPickUpPersonEmail());
   target.setPickUpPersonPhone(source.getPickUpPersonPhone());
    target.setOrderedFormatDateForExtendRental(convertDateToString(source.getDate() , "MMM d , YYYY"));
    target.setRentalEndDateForJs(convertDateToString(source.getRentalEndDate() , "MM/dd/yyyy"));
    target.setRentalStartDateForJs(convertDateToString(source.getRentalStartDate() , "yyyy ,MM, dd"));
  }

  /**
   * This Method converts rental startDate and rental endDate to String
   */
  private String convertDateToString(final Date rentalDate , final String dateFormat) {
    return BlDateTimeUtils.convertDateToStringDate(rentalDate,dateFormat);
  }

  private PriceData convertDoubleToPriceData(final Double price , OrderModel orderModel) {
    return getPriceDataFactory().create(PriceDataType.BUY ,BigDecimal.valueOf(price),orderModel.getCurrency());
  }


  public PriceDataFactory getPriceDataFactory() {
    return priceDataFactory;
  }

  public void setPriceDataFactory(
      PriceDataFactory priceDataFactory) {
    this.priceDataFactory = priceDataFactory;
  }
}
