package com.braintree.order.refund.partial.strategy;


import de.hybris.platform.core.model.order.OrderModel;

import java.math.BigDecimal;


public interface BraintreePartialRefundOrderRecalculationStrategy
{

	/**
	 * Implement Your own startegy to make recalculation for order
	 *
	 * @param order  order that needs to be recalculated
	 * @param amount general amount that needs to be subtracted from order amount
	 * @return recalculated order
	 */
	OrderModel recalculateOrder(final OrderModel order, final BigDecimal amount);

}
