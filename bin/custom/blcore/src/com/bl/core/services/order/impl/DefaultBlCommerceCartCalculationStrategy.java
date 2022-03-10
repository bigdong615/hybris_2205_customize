package com.bl.core.services.order.impl;

import com.bl.core.services.gitfcard.BlGiftCardService;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.servicelayer.model.ModelService;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;

/**
 * It is a custom implementation of OOTB class {@link DefaultCommerceCartCalculationStrategy}
 * @author Neeraj Singh
 */
public class DefaultBlCommerceCartCalculationStrategy extends
    DefaultCommerceCartCalculationStrategy {

 
  private BlGiftCardService giftCardService;
  private ModelService modelService;

  /**
   * It does calculation on cart.
   */
  @Override
  public boolean calculateCart(final CommerceCartParameter parameter) {

    final boolean recalculate = parameter.isRecalculate();
    final CartModel order = parameter.getCart();
    if(CollectionUtils.isNotEmpty(order.getGiftCard())){
       order.setCalculated(false);
    }
    if(parameter.getGiftCardAmount()!= null)
    {
   	 setGiftCardAmount(order,parameter);
    }
    if(BooleanUtils.isTrue(parameter.getRetailGear())){
      order.setIsRetailGearOrder(Boolean.TRUE);
      getModelService().save(order);
      getModelService().refresh(order);
    }
    final boolean result = super.calculateCart(parameter);

    if (recalculate) {
      getGiftCardService().calculateGiftCard(order, order.getTotalPrice());
    }
    return result;
  }
  
  /**
   * This method save gift card amount on order 
   */
  private void setGiftCardAmount(final CartModel order, final CommerceCartParameter parameter) 
  {
	  order.setGiftCardOrder(true);
	  order.setGiftCardCost(parameter.getGiftCardAmount());
	  getModelService().save(order);
	  getModelService().refresh(order);
  }
  
  public BlGiftCardService getGiftCardService() {
    return giftCardService;
  }

  public void setGiftCardService(BlGiftCardService giftCardService) {
    this.giftCardService = giftCardService;
  }
  /**
 	 * @return the modelService
 	 */
 	public ModelService getModelService()
 	{
 		return modelService;
 	}

 	/**
 	 * @param modelService the modelService to set
 	 */
 	public void setModelService(ModelService modelService)
 	{
 		this.modelService = modelService;
 	}

}