package com.bl.tax.populators;

import de.hybris.platform.commercefacades.order.converters.populator.CartPopulator;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.core.model.order.CartModel;

/**
 * this populator created for populating tax details on order summary
 * @author Manikandan
 */
public class BlOrderTaxPopulator extends CartPopulator {

  /**
   * this method created to populate tax details from cartModel to cartData
   * @param source cartModel
   * @param target cartData
   */
  @Override
  public void populate(final CartModel source, final CartData target)
  {
    super.populate(source ,target);
    final Double totalAvalaraTax = source.getTotalAvalaraTaxCalculated();
    target.setTaxAvalaraCalculated(createPrice(source, null != totalAvalaraTax && totalAvalaraTax > 0.0
        ? totalAvalaraTax : 0.0));
  }
}
