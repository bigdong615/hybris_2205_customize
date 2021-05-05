package com.bl.tax.stratergy;

import de.hybris.platform.commerceservices.externaltax.DecideExternalTaxesStrategy;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.util.Config;

public class BlDetermineExternalTaxStrategy implements DecideExternalTaxesStrategy {

  @Override
  public boolean shouldCalculateExternalTaxes(AbstractOrderModel abstractOrder) {
    if (abstractOrder == null)
    {
      throw new IllegalStateException("Order is null. Cannot apply external tax to it.");
    }

    return abstractOrder.getDeliveryMode() != null
        && abstractOrder.getDeliveryAddress() != null && Config.getBoolean("bl.calculate.externaltax", true);
  }
}
