package com.bl.core.order.hook.impl;

import de.hybris.platform.commerceservices.order.hook.CommercePlaceOrderMethodHook;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.util.DiscountValue;

import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Required;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.gitfcard.BlGiftCardService;


/**
        * @author Admin
        *
        */
public class GiftCardOrderMethodHook implements CommercePlaceOrderMethodHook
{
    private EventService eventService;
    private ModelService modelService;

    @Resource(name = "giftCardService")
    private BlGiftCardService giftCardService;


    @Override
    public void afterPlaceOrder(final CommerceCheckoutParameter commerceCheckoutParameter,
                                final CommerceOrderResult commerceOrderResult)
    {
        final OrderModel order = commerceOrderResult.getOrder();
        getModelService().refresh(order);
        ServicesUtil.validateParameterNotNullStandardMessage("order", order);

        double discountValue = 0.00D;
        if (CollectionUtils.isNotEmpty(order.getGlobalDiscountValues()))
        {
            for (final DiscountValue orderDiscounts : order.getGlobalDiscountValues())
            {
                discountValue += orderDiscounts.getValue();
            }
        }

        // add tax to total only if order is NET
        double totalplustax = order.getTotalPrice().doubleValue();
        if (order.getNet() != null && order.getNet().booleanValue())
        {
            totalplustax += order.getTotalTax().doubleValue();
            //			for (final AbstractOrderEntryModel abstractOrderEntryModel : order.getEntries())
            //			{
            //				final Double oraclePrice = (abstractOrderEntryModel.getTotalPrice() / abstractOrderEntryModel.getQuantity());
            //				BigDecimal rounded = new BigDecimal(oraclePrice);
            //				rounded = rounded.setScale(2, RoundingMode.HALF_UP);
            //				abstractOrderEntryModel.setOraclePrice(rounded.doubleValue());
            //				getModelService().save(abstractOrderEntryModel);
            //			}
        }
        else
        {
            //			try
            //			{
            //				final double total = order.getTotalPrice().doubleValue();
            //				final double totalTax = order.getTotalTax().doubleValue();
            //				//Calculating discount tax SUS-1210
            //				final double discountTax = (discountValue * order.getTaxRate()) / 100.0;
            //				final double tax = totalTax - discountTax;
            //				double taxRate = tax / (total - tax);
            //				BigDecimal bd = new BigDecimal(taxRate);
            //				bd = bd.setScale(2, RoundingMode.HALF_UP);
            //				taxRate = bd.doubleValue() * 100;
            //				               order.setTaxRate(taxRate);
            //				order.setTotalTax(tax);

            //				for (final AbstractOrderEntryModel abstractOrderEntryModel : order.getEntries())
            //				{
            //					final Double oraclePrice = (abstractOrderEntryModel.getTotalPrice() / abstractOrderEntryModel.getQuantity())
            //							/ (1 + (order.getTaxRate().doubleValue() / 100));
            //					BigDecimal rounded = new BigDecimal(oraclePrice);
            //					rounded = rounded.setScale(2, RoundingMode.HALF_UP);
            //					abstractOrderEntryModel.setOraclePrice(rounded.doubleValue());
            //					getModelService().save(abstractOrderEntryModel);
            //				}
            //			}
            //			catch (final Exception e)
            //			{
            //				//               LOG.error("Error calculating taxrate", e);
            //			}
        }

        giftCardService.calculateGiftCard(order, totalplustax);

        final List<GiftCardModel> giftCards = order.getGiftCard();
        if (giftCards != null)
        {
            for (int i = 0; i < giftCards.size(); i++)
            {
                final GiftCardModel giftCard = giftCards.get(i);
                getModelService().refresh(giftCard);
                final List<GiftCardMovementModel> movements = giftCard.getMovements();
                for (int j = 0; j < movements.size(); j++)
                {
                    final GiftCardMovementModel giftCardMovementModel = movements.get(j);
                    if (giftCardMovementModel.getCommited() != null && !giftCardMovementModel.getCommited())
                    {
                        giftCardMovementModel.setCommited(Boolean.TRUE);
                        giftCardMovementModel.setOrder(order);
                        giftCardMovementModel.setRedeemDate(new Date());
                        getModelService().save(giftCardMovementModel);
                    }
                }
            }
        }
    }

    @Override
    public void beforePlaceOrder(final CommerceCheckoutParameter commerceCheckoutParameter)
    {
        // not implemented
    }

    @Override
    public void beforeSubmitOrder(final CommerceCheckoutParameter commerceCheckoutParameter,
                                  final CommerceOrderResult commerceOrderResult)
    {
        // not implemented
    }

    protected EventService getEventService()
    {
        return eventService;
    }

    @Required
    public void setEventService(final EventService eventService)
    {
        this.eventService = eventService;
    }

    public ModelService getModelService()
    {
        return modelService;
    }

    @Required
    public void setModelService(final ModelService modelService)
    {
        this.modelService = modelService;
    }
}

