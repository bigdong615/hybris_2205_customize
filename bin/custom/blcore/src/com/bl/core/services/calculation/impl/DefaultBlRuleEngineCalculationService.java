package com.bl.core.services.calculation.impl;

import de.hybris.order.calculation.domain.Order;
import de.hybris.order.calculation.domain.OrderCharge;
import de.hybris.platform.ruleengineservices.calculation.impl.DefaultRuleEngineCalculationService;
import de.hybris.platform.ruleengineservices.rao.CartRAO;
import de.hybris.platform.ruleengineservices.rao.DeliveryModeRAO;
import de.hybris.platform.ruleengineservices.rao.ShipmentRAO;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.math.BigDecimal;

/**
 * @author Ritika
 */
public class DefaultBlRuleEngineCalculationService extends DefaultRuleEngineCalculationService {

  /**
   * Change selected delivery mode cost to 0
   * @param cartRao
   * @param mode
   * @return
   */
  public ShipmentRAO changeDeliveryCost(CartRAO cartRao, DeliveryModeRAO mode) {
    ServicesUtil.validateParameterNotNull(cartRao, "cart rao must not be null");
    ServicesUtil.validateParameterNotNull(mode, "mode must not be null");
    ServicesUtil.validateParameterNotNull(mode.getCost(), "mode cost must not be null");
    ServicesUtil.validateParameterNotNull(mode.getCode(), "mode code must not be null");
    Order cart = getAbstractOrderRaoToOrderConverter().convert(cartRao);
    this.removeShippingCharges(cart);
    if (BigDecimal.ZERO.compareTo(mode.getCost()) < 0) {
      OrderCharge shipping = this.createShippingCharge(cart, true, BigDecimal.valueOf(0.0));
      cart.addCharge(shipping);
    }
    recalculateTotals(cartRao, cart);
    ShipmentRAO shipmentRAO = this.createShipmentRAO(mode);
    this.getRaoUtils().addAction(cartRao, shipmentRAO);
    return shipmentRAO;
  }
}

