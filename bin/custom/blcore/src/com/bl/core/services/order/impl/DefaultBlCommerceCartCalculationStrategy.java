package com.bl.core.services.order.impl;

import com.bl.core.services.gitfcard.BlGiftCardService;
import de.hybris.platform.commerceservices.order.impl.DefaultCommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;

/**
 * It is a custom implementation of OOTB class {@link DefaultCommerceCartCalculationStrategy}
 * @author Neeraj Singh
 */
public class DefaultBlCommerceCartCalculationStrategy extends
    DefaultCommerceCartCalculationStrategy {

  private BlGiftCardService giftCardService;

  @Override
  public boolean calculateCart(final CommerceCartParameter parameter) {

    final boolean recalculate = parameter.isRecalculate();
    final boolean result = super.calculateCart(parameter);
    final CartModel order = parameter.getCart();
    if (recalculate) {
      // add tax to total only if order is NET
      double totalPlusTax = order.getTotalPrice();
      if (order.getNet() != null && order.getNet()) {
        totalPlusTax += order.getTotalTax();
      }
      getGiftCardService().calculateGiftCard(order, totalPlusTax);
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

