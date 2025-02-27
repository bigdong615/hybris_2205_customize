package com.bl.core.services.strategy.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.BlackoutDateTypeEnum;
import com.bl.core.enums.CarrierEnum;
import com.bl.core.model.ShippingOptimizationModel;
import com.bl.core.shipping.service.BlDeliveryModeService;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceDeliveryModeStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;

import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.stream.Collector;

/**
 * DefaultBlCommerceDeliveryModeStrategy for setting delivery mode and actual rental dates.
 *
 * @author Sunil
 */
public class DefaultBlCommerceDeliveryModeStrategy extends DefaultCommerceDeliveryModeStrategy {

  private static final Logger LOG = Logger.getLogger(DefaultBlCommerceDeliveryModeStrategy.class);
  private BlDeliveryModeService zoneDeliveryModeService;
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
    
    // BLS-40 starts

 	 AtomicInteger preDaysToDeduct = new AtomicInteger(0);
 	 AtomicInteger postDaysToAdd = new AtomicInteger(0);

    List<ShippingOptimizationModel> shippingOptimizationModels = getShippingOptimizeData(deliveryModeModel,cartModel.getDeliveryAddress());
    shippingOptimizationModels =   updatePreAndPostServiceDays(deliveryModeModel,shippingOptimizationModels,preDaysToDeduct,postDaysToAdd);
    
    final List<Date> blackOutDates = blDatePickerService.getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
    final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
    //    BLS-40 ends

    if (null != rentalDateDto) {
      final Date startDay = BlDateTimeUtils
          .subtractDaysInRentalDates(preDaysToDeduct.get(), rentalDateDto.getSelectedFromDate(),
              blackOutDates);
      final Date currentDate = BlDateTimeUtils.convertStringDateToDate(
              BlDateTimeUtils.getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
              BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN);
      if(startDay.compareTo(currentDate) < 0) {
          cartModel.setActualRentalStartDate(currentDate);
      }else{
          cartModel.setActualRentalStartDate(startDay);
      }
        // Commenting replacing below line to get Post Order Transit Date same as optimized shipping end date.
        // final Date endDay = BlDateTimeUtils.getFinalEndDateConsideringPostBlackoutDates(postDaysToAdd.get(), rentalDateDto.getSelectedToDate(), blackOutDates);

        final Date endDay = BlDateTimeUtils.addDaysInRentalDates(postDaysToAdd.get(), rentalDateDto.getSelectedToDate(), blackOutDates);

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

  public List<ShippingOptimizationModel>  getShippingOptimizeData(ZoneDeliveryModeModel deliveryModeModel , AddressModel addressModel){
    final int carrierId = getCarrierId((ZoneDeliveryModeModel) deliveryModeModel);
    final String addressZip = getAddressZip(addressModel);
    return (StringUtils.isNotBlank(addressZip) ? getZoneDeliveryModeService().getOptimizedShippingRecordsForCarrierAndZip(carrierId, addressZip) : Collections.EMPTY_LIST);
  }

  public List<ShippingOptimizationModel> updatePreAndPostServiceDays(
      ZoneDeliveryModeModel deliveryModeModel,
      List<ShippingOptimizationModel> shippingOptimizationModels, AtomicInteger preDaysToDeduct,
      AtomicInteger postDaysToAdd) {
    if (CollectionUtils.isNotEmpty(shippingOptimizationModels)) {
      shippingOptimizationModels = getZoneDeliveryModeService()
          .updatePreAndPostServiceDays(shippingOptimizationModels, preDaysToDeduct, postDaysToAdd);
    } else {
      preDaysToDeduct.set(StringUtils.isNotBlank(deliveryModeModel.getPreReservedDays()) ? Integer
          .parseInt(deliveryModeModel.getPreReservedDays()) : 0);

      postDaysToAdd.set(StringUtils.isNotBlank(deliveryModeModel.getPostReservedDays()) ? Integer
          .parseInt(deliveryModeModel.getPostReservedDays()) : 0);
    }
    return shippingOptimizationModels;
  }

  public void updateActualRentalDate(final AbstractOrderModel abstractOrderModel,
      String selectedFromDate, String selectedToDate, AtomicInteger preDaysToDeduct,
      AtomicInteger postDaysToAdd) {
    final List<Date> blackOutDates = blDatePickerService
        .getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
    final Date startDay = BlDateTimeUtils
        .subtractDaysInRentalDates(preDaysToDeduct.get(), selectedFromDate,
            blackOutDates);
    final Date currentDate = BlDateTimeUtils.convertStringDateToDate(
        BlDateTimeUtils
            .getCurrentDateUsingCalendar(BlDeliveryModeLoggingConstants.ZONE_PST, new Date()),
        BlDeliveryModeLoggingConstants.RENTAL_DATE_PATTERN);
    if (startDay.compareTo(currentDate) < 0) {
      abstractOrderModel.setActualRentalStartDate(currentDate);
    } else {
      abstractOrderModel.setActualRentalStartDate(startDay);
    }

    final Date endDay = BlDateTimeUtils
        .addDaysInRentalDates(postDaysToAdd.get(), selectedToDate, blackOutDates);
    abstractOrderModel.setActualRentalEndDate(endDay);

    BlLogger.logFormatMessageInfo(LOG, Level.INFO,
        "Actual rental start date : {} and actual rental end date : {} set in order with id : {}",
        startDay, endDay, abstractOrderModel.getCode());
  }


  private int getCarrierId(final ZoneDeliveryModeModel zoneDeliveryModeModel) {
     if (zoneDeliveryModeModel != null) {
         if (zoneDeliveryModeModel.getCarrier() != null) {
             return CarrierEnum.UPS.getCode().equals(zoneDeliveryModeModel.getCarrier().getCode()) ? BlInventoryScanLoggingConstants.TWO :
                     BlInventoryScanLoggingConstants.ONE;
         } else {
             return BlInventoryScanLoggingConstants.TWO;
         }
     }
     return BlInventoryScanLoggingConstants.ZERO;
  }

  private String getAddressZip(final AddressModel addressModel) {
     String newZip;
     if(addressModel != null && addressModel.getPostalcode() != null) {
         if(addressModel.getPostalcode().contains("-")) {
             newZip = addressModel.getPostalcode().split("-")[0];
         } else {
             newZip = addressModel.getPostalcode();
         }
     } else {
          newZip = StringUtils.EMPTY;
     }
     return newZip;
  }  
  
  /**
 * @return the zoneDeliveryModeService
 */
   public BlDeliveryModeService getZoneDeliveryModeService()
   {
   	return zoneDeliveryModeService;
   }
   
   /**
    * @param zoneDeliveryModeService the zoneDeliveryModeService to set
    */
   public void setZoneDeliveryModeService(BlDeliveryModeService zoneDeliveryModeService)
   {
   	this.zoneDeliveryModeService = zoneDeliveryModeService;
   }

  public BlDatePickerService getBlDatePickerService() {
    return blDatePickerService;
  }

  public void setBlDatePickerService(final BlDatePickerService blDatePickerService) {
    this.blDatePickerService = blDatePickerService;
  }

}
