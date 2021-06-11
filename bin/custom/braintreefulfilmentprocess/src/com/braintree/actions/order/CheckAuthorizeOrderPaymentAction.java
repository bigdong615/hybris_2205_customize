/*
 * [y] hybris Platform
 *
 * Copyright (c) 2000-2015 hybris AG
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of hybris
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with hybris.
 *
 *
 */
package com.braintree.actions.order;

import com.braintree.constants.BraintreeConstants;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.transaction.service.BrainTreeTransactionService;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.payment.CreditCardPaymentInfoModel;
import de.hybris.platform.core.model.order.payment.InvoicePaymentInfoModel;
import de.hybris.platform.orderprocessing.model.OrderProcessModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.processengine.action.AbstractSimpleDecisionAction;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collection;
import java.util.function.Predicate;

import static com.braintree.constants.BraintreeConstants.FAKE_REQUEST_ID;


/**
 * This action implements payment authorization using {@link CreditCardPaymentInfoModel}. Any other payment model could
 * be implemented here, or in a separate action, if the process flow differs.
 */
public class CheckAuthorizeOrderPaymentAction extends AbstractSimpleDecisionAction<OrderProcessModel>
{
	private static final Logger LOG = Logger.getLogger(CheckAuthorizeOrderPaymentAction.class);

	@Autowired
	private BrainTreeTransactionService brainTreeTransactionService;

	@Autowired
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;


	@Override
	public Transition executeAction(final OrderProcessModel process)
	{
		final OrderModel order = process.getOrder();

		if (order != null)
		{
			if (order.getPaymentInfo() instanceof InvoicePaymentInfoModel)
			{
				return Transition.OK;
			}
			else
			{
				return assignStatusForOrder(order);
			}
		}
		return Transition.NOK;
	}

	/**
	 * Sets the status for given order in case on of its {@link PaymentTransactionEntryModel} matches proper
	 * {@link PaymentTransactionType} and {@link TransactionStatus}.
	 *
	 * @param order
	 *           {@link OrderModel}
	 * @return {@link Transition}
	 */
	protected Transition assignStatusForOrder(final OrderModel order)
	{

		if (order.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
		{
			BrainTreePaymentInfoModel paymentInfoModel = (BrainTreePaymentInfoModel) order.getPaymentInfo();
			String intent = paymentInfoModel.getPayPalIntent();
			LOG.error("intent: " + intent);
			String cardNumber = paymentInfoModel.getCardNumber();
			LOG.error("cardNumber: " + cardNumber);

			final boolean isNoTransactionAuthorized = isNoTransactionAuthorized(order);
			final boolean isVenmoPayment = BraintreeConstants.VENMO_CHECKOUT.equals(paymentInfoModel.getPaymentProvider());

			if ((StringUtils.isNotBlank(cardNumber) || isVenmoPayment) && isNoTransactionAuthorized)
			{
				brainTreeTransactionService.createAuthorizationTransaction(order);
				LOG.error("return result: " + Transition.OK);
				return Transition.OK;
			}

			if (BraintreeConstants.PAYPAL_INTENT_ORDER.equalsIgnoreCase(intent) && StringUtils.isBlank(cardNumber) && !isVenmoPayment)
			{
				brainTreeCheckoutFacade.handleOrderIntentViaSubscription(order);
				return Transition.OK;
			}

			if (isNoTransactionAuthorized) {
				if (BraintreeConstants.PAYPAL_INTENT_AUTHORIZE.equalsIgnoreCase(intent))
				{
					brainTreeTransactionService.createAuthorizationTransaction(order);
					return Transition.OK;
				}
				else if (BraintreeConstants.PAYPAL_INTENT_SALE.equalsIgnoreCase(intent))
				{
					brainTreeTransactionService.createAuthorizationTransaction(order);
				}
			}


		}

		for (final PaymentTransactionModel transaction : order.getPaymentTransactions())
		{
			for (final PaymentTransactionEntryModel entry : transaction.getEntries())
			{
				if (entry.getType().equals(PaymentTransactionType.AUTHORIZATION)
						&& TransactionStatus.ACCEPTED.name().equals(entry.getTransactionStatus()))
				{
					order.setStatus(OrderStatus.PAYMENT_AUTHORIZED);
					modelService.save(order);
					return Transition.OK;
				}
			}
		}
		LOG.error("Return from action with result: " + Transition.NOK);
		return Transition.NOK;
	}

	private boolean isNoTransactionAuthorized(final OrderModel order)
	{
		return order.getPaymentTransactions().stream()
				.map(PaymentTransactionModel::getEntries)
				.flatMap(Collection::stream)
				.filter(isAuthorizationTypePredicate())
				.filter(isFakeTransactionPredicate().negate())
				.map(PaymentTransactionEntryModel::getTransactionStatus)
				.noneMatch(Predicate.isEqual(TransactionStatus.ACCEPTED.name()));
	}

	private Predicate<PaymentTransactionEntryModel> isAuthorizationTypePredicate()
	{
		return entry -> PaymentTransactionType.AUTHORIZATION.equals(entry.getType());
	}

	private Predicate<PaymentTransactionEntryModel> isFakeTransactionPredicate()
	{
		return entry -> FAKE_REQUEST_ID.equals(entry.getRequestId());
	}
}
