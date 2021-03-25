package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.CartPopulator;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;

import com.bl.core.model.GiftCardModel;
import com.bl.facades.giftcard.data.BLGiftCardData;

/**
 * @author Admin
 *
 */
public class BLCartPopulator extends CartPopulator
{
    @Resource(name = "modelService")
    private ModelService modelService;

    @Override
    public void populate(final CartModel source, final CartData target)
    {

        modelService.refresh(source);

        super.populate(source, target);

        final double rawSubTotal = source.getSubtotal() != null ? source.getSubtotal().doubleValue() : 0.0d;
        final PriceData subTotalRawPriceData = createPrice(source, Double.valueOf(rawSubTotal));
        //		if (subTotalRawPriceData != null) {
        //			target.setRawSubTotal(subTotalRawPriceData);
        //		}

        final PriceDataType priceType = PriceDataType.BUY;

        if (source.getTotalPrice() != null && source.getGiftCardAmount() != null)
        {
            final PriceData grandTotal = getPriceDataFactory().create(priceType, BigDecimal.valueOf(source.getGrandTotal()),
                    source.getCurrency() != null ? source.getCurrency().getIsocode() : "");
            if (grandTotal != null)
            {
                target.setGrandTotal(grandTotal);
            }
        }

        //		if (source.getGiftCardCode() != null) {
        //			target.setGiftCode(source.getGiftCardCode());
        //		}

        if (source.getGiftCardAmount() != null)
        {

            final PriceData giftDiscount = getPriceDataFactory().create(priceType,
                    BigDecimal.valueOf(source.getGiftCardAmount().doubleValue()),
                    source.getCurrency() != null ? source.getCurrency().getIsocode() : "");
            if (giftDiscount != null)
            {
                target.setGiftCardDiscount(giftDiscount);
            }
        }

        //		if (source.getDiscountDelivery() != null) {
        //
        //			final PriceData discountDelivery = getPriceDataFactory().create(priceType,
        //					BigDecimal.valueOf(source.getDiscountDelivery().doubleValue()), source.getCurrency().getIsocode());
        //			if (discountDelivery != null) {
        //				target.setDiscountDelivery(discountDelivery);
        //			}
        //		}

        //		if (source.getTotalDeliveryWithFreeShipping() != null) {
        //
        //			final PriceData totalDeliveryWithFreeShipping = getPriceDataFactory().create(priceType,
        //					BigDecimal.valueOf(source.getTotalDeliveryWithFreeShipping().doubleValue()), source.getCurrency().getIsocode());
        //			if (totalDeliveryWithFreeShipping != null) {
        //				target.setTotalDeliveryWithFreeShipping(totalDeliveryWithFreeShipping);
        //			}
        //		}
        //
        //		if (source.getEntries() != null && !source.getEntries().isEmpty()) {
        //
        //			target.setEntries(groupEntries(target.getEntries()));
        //
        //
        //		}
        //		if (source.getDeliveryCost() != null) {
        //			final PriceData totalDeliveryCost = getPriceDataFactory().create(priceType,
        //					BigDecimal.valueOf(source.getDeliveryCost().doubleValue()), source.getCurrency().getIsocode());
        //			if (totalDeliveryCost != null)
        //			{
        //				target.setDeliveryCost(totalDeliveryCost);
        //			}
        //		}
        if (source.getGiftCard() != null && !source.getGiftCard().isEmpty())
        {
            final List<BLGiftCardData> data = new ArrayList<>();
            for (final GiftCardModel entry : source.getGiftCard())
            {
                final BLGiftCardData entrydata = new BLGiftCardData();
                entrydata.setCode(entry.getCode());
                data.add(entrydata);
            }

            target.setGiftCardData(data);

        }

    }

}

