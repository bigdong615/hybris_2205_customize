package com.bl.core.payment.service;

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
	 * @param order model
	 * @return boolean
	 */
	boolean capturePaymentForOrder(final OrderModel order);

	/**
	 * To void 1$ authorization of orders
	 */
	void voidAuthTransaction();
}
