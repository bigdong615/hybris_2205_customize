package com.bl.core.services.strategy.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

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
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;

import java.util.Comparator;
import java.util.Date;
import java.util.List;

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
    final int carrierId = getCarrierId((ZoneDeliveryModeModel) deliveryModeModel);
    final String addressZip = getAddressZip(cartModel.getDeliveryAddress());
 	 int preDaysToDeduct = 0;
 	 int postDaysToAdd = 0;

    List<ShippingOptimizationModel> shippingOptimizationModels = StringUtils.isNotBlank(addressZip) ? getZoneDeliveryModeService().getOptimizedShippingRecordsForCarrierAndZip(carrierId, addressZip) : Collections.EMPTY_LIST;
    
    if(CollectionUtils.isNotEmpty(shippingOptimizationModels)) {
   	 getZoneDeliveryModeService().updatePreAndPostServiceDays(shippingOptimizationModels, preDaysToDeduct, postDaysToAdd);
    }
    else
    {
        preDaysToDeduct = StringUtils.isNotBlank(deliveryModeModel.getPreReservedDays()) ? Integer
                .parseInt(deliveryModeModel.getPreReservedDays()) : 0;
    
        postDaysToAdd = StringUtils.isNotBlank(deliveryModeModel.getPostReservedDays()) ? Integer
                .parseInt(deliveryModeModel.getPostReservedDays()) : 0;   	 
    }
    
    final List<Date> blackOutDates = blDatePickerService.getAllBlackoutDatesForGivenType(BlackoutDateTypeEnum.HOLIDAY);
    final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
    //    BLS-40 ends
    
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
