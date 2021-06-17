package com.bl.core.payment.service;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;

/**
 * This is used to create auth and capture transaction
 * @author Moumita
 */
public interface BlPaymentService
{
	/**
	 * This is for authorization of the orders
	 */
	public void authorizePaymentForOrders();

	/**
	 * This is to capture the authorized transaction
	 */
	public void capturePaymentForOrder(final OrderModel order);
}
