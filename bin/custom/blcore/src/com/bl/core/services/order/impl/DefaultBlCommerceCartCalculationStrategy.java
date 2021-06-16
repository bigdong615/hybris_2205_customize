package com.bl.core.services.order.impl;

import com.bl.core.services.gitfcard.BlGiftCardService;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;
import org.apache.commons.collections4.CollectionUtils;

/**
 * It is a custom implementation of OOTB class {@link DefaultCommerceCartCalculationStrategy}
 * @author Neeraj Singh
 */
public class DefaultBlCommerceCartCalculationStrategy extends
    DefaultCommerceCartCalculationStrategy {

  private BlGiftCardService giftCardService;

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
    final boolean result = super.calculateCart(parameter);

    if (recalculate) {
      getGiftCardService().calculateGiftCard(order, order.getTotalPrice());
    }
    return result;
  }

  public BlGiftCardService getGiftCardService() {
    return giftCardService;
  }

  public void setGiftCardService(BlGiftCardService giftCardService) {
    this.giftCardService = giftCardService;
  }
}