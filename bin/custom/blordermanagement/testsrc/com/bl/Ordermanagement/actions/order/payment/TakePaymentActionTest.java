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

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import de.hybris.bootstrap.annotations.UnitTest;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.payment.PaymentService;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction.Transition;
import de.hybris.platform.servicelayer.model.ModelService;
import com.bl.Ordermanagement.actions.order.payment.TakePaymentAction;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;


@UnitTest
@RunWith(MockitoJUnitRunner.class)
public class TakePaymentActionTest
{
	OrderProcessModel orderProcessModel;
	OrderModel orderModel;
	PaymentTransactionModel paymentTransactionModel;
	PaymentTransactionEntryModel paymentTransactionEntryModel;

	@InjectMocks
	TakePaymentAction action = new TakePaymentAction();

	@Mock
	private PaymentService paymentService;

	@Mock
	private ModelService modelService;

	@Before
	public void setup()
	{
		paymentTransactionEntryModel = spy(new PaymentTransactionEntryModel());

		paymentTransactionModel = new PaymentTransactionModel();
		paymentTransactionModel.setPaymentProvider("PaymentProvider");
		final List<PaymentTransactionModel> paymentTransactions = new ArrayList<>();
		paymentTransactions.add(paymentTransactionModel);

		orderModel = new OrderModel();
		orderModel.setPaymentTransactions(paymentTransactions);

		orderProcessModel = new OrderProcessModel();
		orderProcessModel.setOrder(orderModel);

		when(paymentService.capture(paymentTransactionModel)).thenReturn(paymentTransactionEntryModel);
	}

	@Test
	public void shouldOKWhenPaymentCaptureIsAccepted() throws Exception
	{
		when(paymentTransactionEntryModel.getTransactionStatus()).thenReturn(TransactionStatus.ACCEPTED.name());
		final Transition transition = action.executeAction(orderProcessModel);
		assertTrue(AbstractSimpleDecisionAction.Transition.OK.toString().equals(transition.toString()));
	}

	@Test
	public void shouldNOKWhenPaymentCaptureIsRejected() throws Exception
	{
		when(paymentTransactionEntryModel.getTransactionStatus()).thenReturn(TransactionStatus.REJECTED.name());
		final Transition transition = action.executeAction(orderProcessModel);
		assertTrue(AbstractSimpleDecisionAction.Transition.NOK.toString().equals(transition.toString()));
	}

	@Test
	public void shouldNOKWhenPaymentCaptureIsReview() throws Exception
	{
		when(paymentTransactionEntryModel.getTransactionStatus()).thenReturn(TransactionStatus.REVIEW.name());
		final Transition transition = action.executeAction(orderProcessModel);
		assertTrue(AbstractSimpleDecisionAction.Transition.NOK.toString().equals(transition.toString()));
	}

	@Test
	public void shouldNOKWhenPaymentCaptureError() throws Exception
	{
		when(paymentTransactionEntryModel.getTransactionStatus()).thenReturn(TransactionStatus.ERROR.name());
		final Transition transition = action.executeAction(orderProcessModel);
		assertTrue(AbstractSimpleDecisionAction.Transition.NOK.toString().equals(transition.toString()));
	}

	@Test
	public void shouldSetOrderStatusToPaymentCapturedWhenCaptureIsAccepted() throws Exception
	{
		when(paymentTransactionEntryModel.getTransactionStatus()).thenReturn(TransactionStatus.ACCEPTED.name());

		action.execute(orderProcessModel);

		verify(modelService).save(orderModel);
		assertTrue(orderModel.getStatus().toString().equals(OrderStatus.PAYMENT_CAPTURED.toString()));
	}

	@Test
	public void shouldSetOrderStatusToPaymentNotCapturedWhenCaptureIsRejected() throws Exception
	{
		when(paymentTransactionEntryModel.getTransactionStatus()).thenReturn(TransactionStatus.REJECTED.name());

		action.execute(orderProcessModel);

		verify(modelService).save(orderModel);
		assertTrue(orderModel.getStatus().toString().equals(OrderStatus.PAYMENT_NOT_CAPTURED.toString()));
	}

	@Test
	public void shouldSetOrderStatusToPaymentNotCapturedWhenCaptureIsReview() throws Exception
	{
		when(paymentTransactionEntryModel.getTransactionStatus()).thenReturn(TransactionStatus.REVIEW.name());

		action.execute(orderProcessModel);

		verify(modelService).save(orderModel);
		assertTrue(orderModel.getStatus().toString().equals(OrderStatus.PAYMENT_NOT_CAPTURED.toString()));
	}

	@Test
	public void shouldSetOrderStatusToPaymentNotCapturedWhenCaptureIsError() throws Exception
	{
		when(paymentTransactionEntryModel.getTransactionStatus()).thenReturn(TransactionStatus.ERROR.name());

		action.execute(orderProcessModel);

		verify(modelService).save(orderModel);
		assertTrue(orderModel.getStatus().toString().equals(OrderStatus.PAYMENT_NOT_CAPTURED.toString()));
	}
}
