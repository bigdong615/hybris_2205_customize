package com.bl.facades.populators;

import de.hybris.platform.commercefacades.order.converters.populator.MiniCartPopulator;
import de.hybris.platform.commercefacades.order.data.AbstractOrderData;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;

public class BlMiniCartPopulator extends MiniCartPopulator {

  /**
   * Overriding to remove discounts from subtotal
   * @param source
   * @param target
   */
 @Override
  protected void addTotals(final AbstractOrderModel source, final AbstractOrderData target)
  {
    super.addTotals(source,target);
    final double subTotal = source.getSubtotal().doubleValue();
    final PriceData subTotalPriceData = createPrice(source, Double.valueOf(subTotal));
    target.setSubTotal(subTotalPriceData);

  }

}
