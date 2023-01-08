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
package com.bl.Ordermanagement.actions.order.payment;

import de.hybris.platform.orderprocessing.events.FraudErrorEvent;
import de.hybris.platform.orderprocessing.events.PaymentFailedEvent;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.servicelayer.event.EventService;
import com.bl.Ordermanagement.actions.order.payment.SendPaymentFailedNotificationAction;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.hamcrest.MockitoHamcrest;
import org.junit.runner.RunWith;
import static org.mockito.ArgumentMatchers.anyObject;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.ArgumentMatchers.any;
import org.junit.Rule;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.lenient;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.mockito.quality.Strictness;
import de.hybris.bootstrap.annotations.UnitTest;
import static org.mockito.hamcrest.MockitoHamcrest.argThat;
import org.mockito.Mockito;
import static org.hamcrest.MatcherAssert.assertThat;


/**
*
*/
public class SendPaymentFailedNotificationActionTest
{
	@InjectMocks
	private final SendPaymentFailedNotificationAction sendPaymentFailedNotification = new SendPaymentFailedNotificationAction();

	@Mock
	private EventService eventService;

	@Before
	public void setUp()
	{
		MockitoAnnotations.initMocks(this);
	}

	/**
	 * Test method for
	 * {@link com.bl.Ordermanagement.actions.order.SendPaymentFailedNotificationAction#executeAction(OrderProcessModel)}
	 * .
	 */
	@Test
	public void testExecuteActionOrderProcessModel()
	{
		final OrderProcessModel process = new OrderProcessModel();
		sendPaymentFailedNotification.executeAction(process);

		Mockito.verify(eventService).publishEvent(argThat(new BaseMatcher<FraudErrorEvent>()
		{

			@Override
			public boolean matches(final Object item)
			{
				if (item instanceof PaymentFailedEvent)
				{
					final PaymentFailedEvent event = (PaymentFailedEvent) item;
					if (event.getProcess().equals(process))
					{
						return true;
					}
				}
				return false;
			}

			@Override
			public void describeTo(final Description description)
			{
				//nothing to do

			}
		}));
	}
}
