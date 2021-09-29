package com.bl.facades.promotions.populator;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import java.math.BigDecimal;
import java.util.Date;
import org.apache.commons.lang.BooleanUtils;


public class BlCartRaoPopulator implements Populator<AbstractOrderModel, CartRAO> {


  @Override
  public void populate(final AbstractOrderModel source, final CartRAO target)
  {

    target.setRentalCart(source.getIsRentalCart());
    //added for extended order
    if(BooleanUtils.isTrue(source.getIsExtendedOrder())){
        target.setRentalDurationDays(source.getTotalExtendDays());
        target.setRentalToDate(source.getExtendRentalEndDate());
        target.setRentalArrivalDate(source.getExtendRentalStartDate());
      }
      else{
      if(BlRentalDateUtils.getRentalsDuration() != null) {
        target.setRentalDurationDays(BlRentalDateUtils.getRentalsDuration().getSelectedDays()!= null ? Integer.valueOf(BlRentalDateUtils.getRentalsDuration().getSelectedDays()): 7);
        final RentalDateDto rentalDatesFromSession = BlRentalDateUtils.getBlDatePickerService()
            .getRentalDatesFromSession();
        if(rentalDatesFromSession != null && (rentalDatesFromSession.getSelectedFromDate() != null || rentalDatesFromSession.getSelectedToDate() != null)) {
          target.setRentalArrivalDate(getFormattedDate(rentalDatesFromSession.getSelectedFromDate()));
          target.setRentalToDate(getFormattedDate(rentalDatesFromSession.getSelectedToDate()));
        }
      }
    }
    target.setTotalIncludingDamageWaiver(
        BigDecimal.valueOf(source.getSubtotal() + source.getTotalDamageWaiverCost()));

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
