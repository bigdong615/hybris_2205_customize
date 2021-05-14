package com.bl.core.services.order.impl;

import de.hybris.platform.commerceservices.order.impl.DefaultCommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.CartModel;

import com.bl.core.services.gitfcard.BlGiftCardService;

/**
 * @author Admin
 *
 */
public class BLCommerceCartCalculationStrategy extends DefaultCommerceCartCalculationStrategy
{
    private BlGiftCardService giftCardService;

    @Override
    public boolean calculateCart(final CommerceCartParameter parameter)
    {

        final boolean recalculate = parameter.isRecalculate();

        final boolean result = super.calculateCart(parameter);

        final CartModel order = parameter.getCart();
        if (recalculate)
        {

            // add tax to total only if order is NET
            double totalplustax = order.getTotalPrice().doubleValue();
            if (order.getNet() != null && order.getNet().booleanValue())
            {
                totalplustax += order.getTotalTax().doubleValue();
            }

            getGiftCardService().calculateGiftCard(order, totalplustax);
        }

        return result;
    }

    /**
     * @return the giftCardService
     */
    public BlGiftCardService getGiftCardService()
    {
        return giftCardService;
    }

    /**
     * @param giftCardService
     *           the giftCardService to set
     */
    public void setGiftCardService(final BlGiftCardService giftCardService)
    {
        this.giftCardService = giftCardService;
    }

}

