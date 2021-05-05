package com.bl.tax.populators;

import de.hybris.platform.commercefacades.order.converters.populator.CartPopulator;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.core.model.order.CartModel;

public class BlOrderTaxPopulator extends CartPopulator {
  @Override
  public void populate(final CartModel source, final CartData target)
  {
    super.populate(source ,target);
    target.setTaxAvalaraCalculated(createPrice(source,source.getTotalAvalaraTaxCalculated()));
  }
}
