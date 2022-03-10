package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.OrderPopulator;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.core.model.order.OrderModel;

import java.math.BigDecimal;
import org.apache.commons.lang.BooleanUtils;


/**
 * custom implementation of OOTB class {@link OrderPopulator}.
 * @author Neeraj Singh
 *
 */
public class BlOrderPopulator extends OrderPopulator
{

  /**
   * It populates data.
   * @param source
   * @param target
   */
  @Override
  public void populate(final OrderModel source, final OrderData target)
  {
    super.populate(source, target);
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
    if (source.getGiftCardAmount() != null)
    {
      final PriceData giftDiscount = getPriceDataFactory().create(priceType,
          BigDecimal.valueOf(source.getGiftCardAmount().doubleValue()), source.getCurrency().getIsocode());
      if (giftDiscount != null)
      {
        target.setGiftCardDiscount(giftDiscount);
      }
    }
    if(BooleanUtils.isTrue(source.getIsRetailGearOrder())){
      target.setIsNewGearOrder(source.getIsRetailGearOrder());
    }
    target.setOrderReturnedToWarehouse(source.isOrderReturnedToWarehouse());
  }
}