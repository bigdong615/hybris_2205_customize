package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.ZoneDeliveryModePopulator;
import de.hybris.platform.commercefacades.order.data.ZoneDeliveryModeData;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;

import java.util.Objects;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.bl.core.services.cart.BlCartService;


public class BlZoneDeliveryModePopulator extends ZoneDeliveryModePopulator {
	
	
	private static final String ROUND_TRIP = "Round Trip";
	
	@Autowired
	private BlCartService blCartService;

    @Override
    public void populate(final ZoneDeliveryModeModel source, final ZoneDeliveryModeData target)
    {
        super.populate(source, target);
        
       //Added condition for used Gear Delivery mode  
       final CartModel sessionCart = getBlCartService().getSessionCart();
       if(Objects.nonNull(sessionCart) && BooleanUtils.isFalse(sessionCart.getIsRentalCart()))
       {
      	 String deliveryMethodName = String.valueOf(source.getName());
          String usedGearDeliveryMode = deliveryMethodName.replace(ROUND_TRIP, "");
      	 target.setName(usedGearDeliveryMode);
       }
      
        target.setCutOffTime(source.getCutOffTime());
        target.setCarrier(String.valueOf(source.getCarrier()));
        target.setNumberOfDaysToSkip(String.valueOf(source.getNumberOfDaysToSkip()));
        target.setShippingMethodId(String.valueOf(source.getShippingMethodId()));
        target.setShippingMethodType(String.valueOf(source.getShippingMethodType()));
        target.setBusinessTypeDelivery(source.isBusinessTypeDelivery());
        target.setShippingGroup(Objects.nonNull(source.getShippingGroup()) 
      		  ? source.getShippingGroup().getCode() : StringUtils.EMPTY);
    }

	/**
	 * @return the blCartService
	 */
	public BlCartService getBlCartService()
	{
		return blCartService;
	}

	/**
	 * @param blCartService the blCartService to set
	 */
	public void setBlCartService(BlCartService blCartService)
	{
		this.blCartService = blCartService;
	}
    
    
}
