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
package com.bl.Ordermanagement.actions.order;

import com.bl.Ordermanagement.CheckOrderService;
import com.bl.core.utils.BlReplaceMentOrderUtils;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Required;


/**
 * This example action checks the order for required data in the business process. Skipping this action may result in
 * failure in one of the subsequent steps of the process. The relation between the order and the business process is
 * defined in basecommerce extension through item OrderProcess. Therefore if your business process has to access the
 * order (a typical case), it is recommended to use the OrderProcess as a parentClass instead of the plain
 * BusinessProcess.
 */
public class CheckOrderAction extends AbstractSimpleDecisionAction<OrderProcessModel>
{
	private static final Logger LOG = LoggerFactory.getLogger(CheckOrderAction.class);

	private CheckOrderService checkOrderService;

	@Override
	public Transition executeAction(final OrderProcessModel process)
	{
		LOG.info("Process: {} in step {}", process.getCode(), getClass().getSimpleName());
		final OrderModel order = process.getOrder();

		if (order == null)
		{
			LOG.info("Missing the order, exiting the process");
			return Transition.NOK;
		}

		if (getCheckOrderService().check(order) || BlReplaceMentOrderUtils.isCartForReplacement(order))
		{
			setOrderStatus(order, OrderStatus.CHECKED_VALID);
			return Transition.OK;
		}
		else
		{
			setOrderStatus(order, OrderStatus.CHECKED_INVALID);
			return Transition.NOK;
		}
	}

	protected CheckOrderService getCheckOrderService()
	{
		return checkOrderService;
	}

	@Required
	public void setCheckOrderService(final CheckOrderService checkOrderService)
	{
		this.checkOrderService = checkOrderService;
	}
}
