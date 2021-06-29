package com.bl.facades.populators;

import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import java.util.Date;

public class BlOrderDetailsPopulator <SOURCE extends OrderModel, TARGET extends OrderData> implements
    Populator<SOURCE, TARGET> {


  @Override
  public void populate(OrderModel source, OrderData target) throws ConversionException {

    target.setRentalStartDate(convertDateToString(source.getRentalStartDate()));
   target.setRentalEndDate(convertDateToString(source.getRentalEndDate()));
   target.setIsRentalCart(source.getIsRentalCart());
  }

  /**
   * This Method converts rental startDate and rental endDate to String
   */
  private String convertDateToString(final Date rentalDate) {
    return BlDateTimeUtils.convertDateToStringDate(rentalDate, BlFacadesConstants.RENTAL_DATE_FORMAT);
  }
}
