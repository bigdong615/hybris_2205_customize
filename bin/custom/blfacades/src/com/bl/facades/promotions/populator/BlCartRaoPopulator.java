package com.bl.facades.promotions.populator;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.servicelayer.user.UserService;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.time.temporal.Temporal;
import java.util.*;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;


public class BlCartRaoPopulator implements Populator<AbstractOrderModel, CartRAO> {

	private UserService userService;

  @Override
  public void populate(final AbstractOrderModel source, final CartRAO target)
  {

    target.setRentalCart(source.getIsRentalOrder());
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

	  if(source instanceof CartModel){
		  populateIsFirstTimeCustomer(target);
	  }else{
		  populateCustomerData(source,target);
	  }

 }

  /**
   * Get the formatted date
   * @param selectedDate
   * @return
   */
  private Date getFormattedDate(final String selectedDate) {
    return BlDateTimeUtils.getDate(selectedDate, BlCoreConstants.DATE_FORMAT);
  }
  
  /**
	 * Populate is first time customer.
	 *
	 * @param target
	 *           the target
	 */
	private void populateIsFirstTimeCustomer(final CartRAO target)
	{
		final UserModel currentUser = getUserService().getCurrentUser();
		target.setFirstTimeCustomer(
				Objects.nonNull(currentUser) && BooleanUtils.isFalse(getUserService().isAnonymousUser(currentUser))
						&& CollectionUtils.isEmpty(currentUser.getOrders()));
	}

	private void populateCustomerData(final AbstractOrderModel source, final CartRAO target)
	{
		final UserModel currentUser = source.getUser();
		List<Date> creationTime= new ArrayList<>();
		if(Objects.nonNull(currentUser) && CollectionUtils.isNotEmpty( currentUser.getOrders()))
		{
			for(OrderModel order: currentUser.getOrders()) {
				creationTime.add(order.getCreationtime());
			}
			Collections.sort(creationTime);
			if(creationTime.get(Integer.valueOf(BlCoreConstants.ZERO)).equals(source.getCreationtime())) {
				target.setFirstTimeCustomer(Boolean.TRUE);
			}
		}
		target.setRentalArrivalDate(source.getRentalStartDate());
		target.setRentalToDate(source.getRentalEndDate());
		if(target.getRentalArrivalDate() != null && target.getRentalToDate() != null) {
			DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy");
			long daysBetween = ChronoUnit.DAYS.between(BlDateTimeUtils.convertStringDateToLocalDate(dateFormat.format(target.getRentalArrivalDate()), BlCoreConstants.DATE_FORMAT),
					BlDateTimeUtils.convertStringDateToLocalDate(dateFormat.format(target.getRentalToDate()), BlCoreConstants.DATE_FORMAT));
			target.setRentalDurationDays((int) daysBetween);
		}
		else {
			target.setRentalDurationDays(7);
		}
	}
	/**
	 * @return the userService
	 */
	public UserService getUserService()
	{
		return userService;
	}

	/**
	 * @param userService
	 *           the userService to set
	 */
	public void setUserService(final UserService userService)
	{
		this.userService = userService;
	}

}
