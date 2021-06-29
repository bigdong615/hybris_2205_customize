package com.bl.facades.populators;

import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.constants.BlFacadesConstants;
import de.hybris.platform.commercefacades.order.converters.populator.OrderHistoryPopulator;
import de.hybris.platform.commercefacades.order.data.OrderHistoryData;
import de.hybris.platform.core.model.order.OrderModel;
import java.util.Date;

/**
 * This Populator Overridden to add Rental Dates
 * @author Manikandan
 */
public class BlOrderHistoryPopulator extends OrderHistoryPopulator {

  /**
   * This Method Populate Rental Dates to target to display on storefront
   */
  @Override
  public void populate(final OrderModel source, final OrderHistoryData target)
  {
   super.populate(source ,target);
   if(null != source.getRentalStartDate()){
    target.setRentalStartDate(convertDateToString(source.getRentalStartDate()));
  }
   if(null != source.getRentalEndDate()) {
     target.setRentalEndDate(convertDateToString(source.getRentalEndDate()));
   }
   target.setRentalCart(source.getIsRentalCart());
   target.setOrderDate(convertDateToString(source.getDate()));
  }

  /**
   * This Method converts rental startDate and rental endDate to String
   */
  private String convertDateToString(final Date rentalDate) {
    return BlDateTimeUtils.convertDateToStringDate(rentalDate, BlFacadesConstants.RENTAL_DATE_FORMAT);
  }

}
