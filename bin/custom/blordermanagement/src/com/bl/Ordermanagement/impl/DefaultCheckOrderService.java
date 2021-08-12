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
package com.bl.Ordermanagement.impl;

import com.bl.core.utils.BlReplaceMentOrderUtils;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import com.bl.Ordermanagement.CheckOrderService;
import java.util.Objects;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;


/**
 * default implementation to valid the order
 */
public class DefaultCheckOrderService implements CheckOrderService
{
	
	private static final String PICKUP_CODE = "pickup";

	@Override
	public boolean check(final OrderModel order)
	{
		if (!order.getCalculated().booleanValue())
		{
			// Order must be calculated
			return false;
		}
		if (order.getEntries().isEmpty())
		{
			// Order must have some lines
			return false;
		}
		else if (BooleanUtils.isFalse(BlReplaceMentOrderUtils.isReplaceMentOrder()) && Objects.isNull(order.getReplacementOrder())
				&& order.getPaymentInfo() == null && StringUtils.isBlank(order.getPoNumber()))
		{
			// Order must have some payment info to use in the process
			return false;
		}
		else
		{
			// Order delivery options must be valid
			return checkDeliveryOptions(order);
		}
	}

	protected boolean checkDeliveryOptions(final OrderModel order)
	{
		if (order.getDeliveryMode() == null)
		{
			// Order must have an overall delivery mode 
			return false;
		}

		if (order.getDeliveryAddress() == null && order.getDeliveryMode() == null)
		{
			for (final AbstractOrderEntryModel entry : order.getEntries())
			{
				if (entry.getDeliveryPointOfService() == null && entry.getDeliveryAddress() == null)
				{
					// Order and Entry have no delivery address and some entries are not for pickup
					return false;
				}
			}
			//it is a Pickup Order
			order.getDeliveryMode().setCode(PICKUP_CODE);
		}

		return true;
	}
}
