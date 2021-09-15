package com.bl.core.services.strategy.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceDeliveryModeStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import java.util.Date;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * DefaultBlCommerceDeliveryModeStrategy for setting delivery mode and actual rental dates.
 *
 * @author Sunil
 */
public class DefaultBlCommerceDeliveryModeStrategy extends DefaultCommerceDeliveryModeStrategy {

  private static final Logger LOG = Logger.getLogger(DefaultBlCommerceDeliveryModeStrategy.class);
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

    final int preDaysToDeduct =
        StringUtils.isNotBlank(deliveryModeModel.getPreReservedDays()) ? Integer
            .parseInt(deliveryModeModel.getPreReservedDays()) : 0;

    final int postDaysToAdd =
        StringUtils.isNotBlank(deliveryModeModel.getPostReservedDays()) ? Integer
            .parseInt(deliveryModeModel.getPostReservedDays()) : 0;

    if (null != rentalDateDto) {
      final Date startDay = BlDateTimeUtils
          .subtractDaysInRentalDates(preDaysToDeduct, rentalDateDto.getSelectedFromDate(),
              blackOutDates);
      final Date endDay = BlDateTimeUtils.getFinalEndDateConsideringPostBlackoutDates(postDaysToAdd,
          rentalDateDto.getSelectedToDate(), blackOutDates);

      cartModel.setActualRentalStartDate(startDay);
      cartModel.setActualRentalEndDate(endDay);

      BlLogger.logFormatMessageInfo(LOG, Level.INFO,
          "Actual rental start date : {} and actual rental end date : {} set in cart with id : {} for delivery mode : {}",
          startDay, endDay, cartModel.getCode(), deliveryModeModel.getCode());
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
