/*
 * [y] hybris Platform
 *
 * Copyright (c) 2018 SAP SE or an SAP affiliate company.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of SAP
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with SAP.
 *
 */
package com.bl.Ordermanagement;

import de.hybris.platform.core.model.order.OrderModel;


/**
 * Used by CheckOrderAction, this service is designed to validate the order prior to running the fulfilment process.
 */
public interface CheckOrderService
{
	/**
	 * Performs various order checks
	 *
	 * @param order
	 * 		the order {@link OrderModel} to be checked
	 * @return boolean value
	 * 		true if the order is ready for fulfillment or false if not
	 */
	boolean check(OrderModel order);
}
