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

import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.esp.service.AbstractESPRestService;
import com.bl.logging.BlLogger;
import de.hybris.platform.core.enums.DeliveryStatus;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.ordersplitting.model.ConsignmentProcessModel;
import de.hybris.platform.processengine.action.AbstractAction;

import java.util.HashSet;
import java.util.Set;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Verifies whether order is cancelled completely or all the consignment processes are complete or not and updates the Order status/delivery status to
 * reflect this.
 */
public class VerifyOrderCompletionAction extends AbstractAction<OrderProcessModel>
{
	private static final org.apache.log4j.Logger LOG = Logger.getLogger(VerifyOrderCompletionAction.class);
	private DefaultBlESPEventService blEspEventService;

	@Override
	public String execute(final OrderProcessModel process)
	{

		BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Process: {} in step {}", process.getCode(), getClass().getSimpleName());

		BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Process: {} is checking for order cancellation and {} subprocess results",process.getCode(),
				process.getConsignmentProcesses().size());

		final OrderModel order = process.getOrder();

		final boolean someEntriesShipped = order.getEntries().stream()
				.anyMatch(entry -> ((OrderEntryModel) entry).getQuantityShipped().longValue() > 0);
		if (!someEntriesShipped)
		{
			order.setDeliveryStatus(DeliveryStatus.NOTSHIPPED);
		}
		else
		{
			final boolean someEntriesWaiting = order.getEntries().stream()
					.anyMatch(entry -> ((OrderEntryModel) entry).getQuantityPending().longValue() > 0);
			if (someEntriesWaiting)
			{
				order.setDeliveryStatus(DeliveryStatus.PARTSHIPPED);
			}
			else
			{
				order.setDeliveryStatus(DeliveryStatus.SHIPPED);
			}
		}
		save(order);

		final boolean isOrderCancelledCompletely = process.getOrder().getEntries().stream()
				.allMatch(entry -> entry.getQuantity().longValue() == 0);
		if (isOrderCancelledCompletely)
		{
			process.getOrder().setStatus(OrderStatus.CANCELLED);
			getModelService().save(process.getOrder());
			try {
				getBlEspEventService().sendOrderCanceledEvent(order);
			}catch(final Exception e)
			{
				BlLogger.logMessage(LOG, Level.ERROR,"Failed to trigger order canceled event" , e);
			}

			return Transition.CANCELLED.toString();
		}

		final boolean isOrderEntryNotAllocated = order.getEntries().stream()
				.anyMatch(entry -> ((OrderEntryModel) entry).getQuantityUnallocated().longValue() > 0);
		if (isOrderEntryNotAllocated)
		{
			return Transition.WAIT.toString();
		}

		for (final ConsignmentProcessModel subProcess : process.getConsignmentProcesses())
		{
			if (!subProcess.isDone())
			{
				BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Process: {} found subprocess {} incomplete -> wait again!",process.getCode(), subProcess.getCode());

				return Transition.WAIT.toString();
			}
			BlLogger.logFormatMessageInfo(LOG,Level.DEBUG,"Process: {} found subprocess {} complete ...",process.getCode(), subProcess.getCode());
		}

		order.setStatus(OrderStatus.PENDING);
		save(order);
		return Transition.OK.toString();
	}

	@Override
	public Set<String> getTransitions()
	{
		return Transition.getStringValues();
	}

	protected enum Transition
	{
		OK, CANCELLED, WAIT;

		public static Set<String> getStringValues()
		{
			final Set<String> res = new HashSet<>();

			for (final Transition transition : Transition.values())
			{
				res.add(transition.toString());
			}
			return res;
		}
	}

	public DefaultBlESPEventService getBlEspEventService() {
		return blEspEventService;
	}

	public void setBlEspEventService(final DefaultBlESPEventService blEspEventService) {
		this.blEspEventService = blEspEventService;
	}


}
