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
package com.bl.Ordermanagement.actions.order.fraudcheck;

import de.hybris.platform.orderprocessing.events.OrderPlacedEvent;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.processengine.action.AbstractProceduralAction;
import de.hybris.platform.servicelayer.event.EventService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Required;


/**
 * Send a notification that an order is being placed.
 */
public class SendOrderPlacedNotificationAction extends AbstractProceduralAction<OrderProcessModel>
{
	private static final Logger LOG = LoggerFactory.getLogger(SendOrderPlacedNotificationAction.class);

	private EventService eventService;

	@Override
	public void executeAction(final OrderProcessModel process)
	{
		LOG.info("Process: {} in step {}", process.getCode(), getClass().getSimpleName());
		getEventService().publishEvent(new OrderPlacedEvent(process));
	}

	protected EventService getEventService()
	{
		return eventService;
	}

	@Required
	public void setEventService(final EventService eventService)
	{
		this.eventService = eventService;
	}
}
