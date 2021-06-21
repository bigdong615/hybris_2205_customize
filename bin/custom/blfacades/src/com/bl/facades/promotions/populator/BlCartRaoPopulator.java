package com.bl.facades.promotions.populator;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.ruleengineservices.rao.CartRAO;

import java.math.BigDecimal;
import java.util.Date;



public class BlCartRaoPopulator implements Populator<CartModel, CartRAO> {


  @Override
  public void populate(final CartModel source, final CartRAO target)
  {

    target.setRentalCart(source.getIsRentalCart());
    if(BlRentalDateUtils.getRentalsDuration() != null) {
      target.setRentalDurationDays(Integer.valueOf(BlRentalDateUtils.getRentalsDuration().getNumberOfDays()));
      final RentalDateDto rentalDatesFromSession = BlRentalDateUtils.getBlDatePickerService()
          .getRentalDatesFromSession();
      if(rentalDatesFromSession != null && (rentalDatesFromSession.getSelectedFromDate() != null || rentalDatesFromSession.getSelectedToDate() != null)) {
        target.setRentalArrivalDate(getFormattedDate(rentalDatesFromSession.getSelectedFromDate()));
        target.setRentalToDate(getFormattedDate(rentalDatesFromSession.getSelectedToDate()));
      }
    }
    target.setTotalIncludingDamageWaiver(
        BigDecimal.valueOf(source.getSubtotal() + source.getTotalDamageWaiverCost()));
    target.setOrderDeliveryMode(source.getDeliveryMode() != null ? source.getDeliveryMode().getCode(): null);

  }

  /**
   * Get the formatted date
   * @param selectedDate
   * @return
   */
  private Date getFormattedDate(final String selectedDate) {
    return BlDateTimeUtils.getDate(selectedDate, BlCoreConstants.DATE_FORMAT);
  }


}
