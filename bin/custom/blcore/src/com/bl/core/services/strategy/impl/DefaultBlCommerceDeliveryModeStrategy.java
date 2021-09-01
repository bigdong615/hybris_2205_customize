package com.bl.core.services.strategy.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceDeliveryModeStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import java.util.Date;
import java.util.List;

/**
 * DefaultBlCommerceDeliveryModeStrategy for setting delivery mode and actual rental dates.
 *
 * @author Sunil
 */
public class DefaultBlCommerceDeliveryModeStrategy extends DefaultCommerceDeliveryModeStrategy {


  private BlDatePickerService blDatePickerService;

  /**
   * To set delivery mode and actual rental dates.
   * @param parameter
   * @return true
   */
  @Override
  public boolean setDeliveryMode(final CommerceCheckoutParameter parameter) {
    final ZoneDeliveryModeModel deliveryModeModel = (ZoneDeliveryModeModel) parameter
        .getDeliveryMode();
    final CartModel cartModel = parameter.getCart();

    validateParameterNotNull(cartModel, "Cart model cannot be null");
    validateParameterNotNull(deliveryModeModel, "Delivery mode model cannot be null");

    final List<Date> blackOutDates = blDatePickerService.getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
    final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
    final int numberOfDaysToSkip = deliveryModeModel.getNumberOfDaysToSkip().intValue();

    if (null != rentalDateDto) {
      final Date startDay = BlDateTimeUtils
          .subtractDaysInRentalDates(numberOfDaysToSkip, rentalDateDto.getSelectedFromDate(),
              blackOutDates);
      final Date endDay = BlDateTimeUtils
          .addDaysInRentalDates(numberOfDaysToSkip, rentalDateDto.getSelectedToDate(),
              blackOutDates);
      cartModel.setActualRentalStartDate(startDay);
      cartModel.setActualRentalEndDate(endDay);
    }

    cartModel.setDeliveryMode(deliveryModeModel);

    getModelService().save(cartModel);
    final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
    commerceCartParameter.setEnableHooks(true);
    commerceCartParameter.setCart(cartModel);
    getCommerceCartCalculationStrategy().calculateCart(commerceCartParameter);

    return true;
  }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(final BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }

}
