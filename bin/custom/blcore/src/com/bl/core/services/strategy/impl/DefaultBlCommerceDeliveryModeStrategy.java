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

    List<ShippingOptimizationModel> shippingOptimizationModels = getZoneDeliveryModeService().getOptimizedShippingRecordsForCarrierAndZip(carrierId, addressZip);
    
 	 // Business logic to filter warehouseModel from list of warehouse model.
 	 if(StringUtils.isNotBlank(addressZip) && CollectionUtils.isNotEmpty(shippingOptimizationModels) && shippingOptimizationModels.size() > BlInventoryScanLoggingConstants.ONE) {
  		 shippingOptimizationModels = shippingOptimizationModels.stream().collect(minList(Comparator.comparing(ShippingOptimizationModel::getServiceDays)));      		
 	 }
    
    int inboundServiceDays = 0;
    int outboundServiceDays = 0;

    int preDaysToDeduct = 0;
    int postDaysToAdd = 0;
    
    if(CollectionUtils.isNotEmpty(shippingOptimizationModels) && shippingOptimizationModels.size() > BlInventoryScanLoggingConstants.ONE) {
   	 for(ShippingOptimizationModel model : shippingOptimizationModels) 
   	 {
   		 if(model.getInbound() == BlInventoryScanLoggingConstants.ONE) {
   			 inboundServiceDays =  model.getServiceDays();     	        		  
   		 }
   		 else{
   			 outboundServiceDays =  model.getServiceDays();     	        		        		  
   		 }
   	 }

       preDaysToDeduct = outboundServiceDays >= BlInventoryScanLoggingConstants.THREE ? BlInventoryScanLoggingConstants.THREE : outboundServiceDays;
       postDaysToAdd = inboundServiceDays;
   	 
    }
    else if(CollectionUtils.isNotEmpty(shippingOptimizationModels) && null != shippingOptimizationModels.get(0)) 
    {   	 
   	 inboundServiceDays = shippingOptimizationModels.get(0).getServiceDays();
   	 outboundServiceDays = shippingOptimizationModels.get(0).getServiceDays();
   	 
       preDaysToDeduct = outboundServiceDays >= BlInventoryScanLoggingConstants.THREE ? BlInventoryScanLoggingConstants.THREE : outboundServiceDays;
       postDaysToAdd = inboundServiceDays;
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

  static <T> Collector<T, ?, List<T>> minList(Comparator<? super T> comp) {
	    return Collector.of(ArrayList::new, (list, t) -> {
	        int c;
	        if (list.isEmpty() || (c = comp.compare(t, list.get(0))) == 0)
	            list.add(t);
	        else if (c < 0) {
	            /*
	             * We have found a smaller element than what we already have. Clear the list and
	             * add this smallest element to it.
	             */
	            list.clear();
	            list.add(t);
	        }
	    }, (list1, list2) -> {
	        if (comp.compare(list1.get(0), list2.get(0)) < 0)
	            return list1;
	        else if (comp.compare(list1.get(0), list2.get(0)) > 0)
	            return list2;
	        else {
	            list1.addAll(list2);
	            return list1;
	        }
	    });
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
