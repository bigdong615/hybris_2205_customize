/**
 *
 */
package com.braintree.order.service.impl;

import com.braintree.constants.BraintreeConstants;
import com.braintree.order.service.OrderRetrievalService;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.search.restriction.SearchRestrictionService;
import de.hybris.platform.servicelayer.session.SessionExecutionBody;
import de.hybris.platform.servicelayer.session.SessionService;
import org.apache.log4j.Logger;


public class OrderRetrievalServiceImpl implements OrderRetrievalService
{
	private static final Logger LOG = Logger.getLogger(OrderRetrievalServiceImpl.class);

	private SessionService sessionService;

	private SearchRestrictionService searchRestrictionService;

	private BrainTreePaymentTransactionService brainTreePaymentTransactionService;

	@Override
	public PaymentTransactionModel getTransactionForId(final String transactionID)
	{
		// Create a temporary session context and disable restrictions
		final PaymentTransactionModel transaction = (PaymentTransactionModel) getSessionService()
				.executeInLocalView(new SessionExecutionBody()
				{
					@Override
					public Object execute()
					{
						searchRestrictionService.disableSearchRestrictions();
						final PaymentTransactionModel paymentTransactionModel = brainTreePaymentTransactionService
								.getTransactionByRequestIdAndPaymentProvider(transactionID, BraintreeConstants.BRAINTREE_PROVIDER_NAME);
						return paymentTransactionModel != null ? paymentTransactionModel : null;
					}
				});

		return transaction;
	}

	@Override
	public String getOrderCodeForTransaction(final String transactionID)
	{
		AbstractOrderModel order = getOrderForTransaction(transactionID);
		if (order != null)
		{
			return order.getCode();
		}
		return null;
	}

	@Override
	public AbstractOrderModel getOrderForTransaction(String transactionID)
	{
		PaymentTransactionModel transactionModel = getTransactionForId(transactionID);
		if (transactionModel != null)
		{
			return transactionModel.getOrder();
		}
		return null;
	}

	public SessionService getSessionService()
	{
		return sessionService;
	}

	public void setSessionService(final SessionService sessionService)
	{
		this.sessionService = sessionService;
	}

	public SearchRestrictionService getSearchRestrictionService()
	{
		return searchRestrictionService;
	}

	public void setSearchRestrictionService(final SearchRestrictionService searchRestrictionService)
	{
		this.searchRestrictionService = searchRestrictionService;
	}

	public BrainTreePaymentTransactionService getBrainTreePaymentTransactionService()
	{
		return brainTreePaymentTransactionService;
	}

	public void setBrainTreePaymentTransactionService(final BrainTreePaymentTransactionService brainTreePaymentTransactionService)
	{
		this.brainTreePaymentTransactionService = brainTreePaymentTransactionService;
	}

}
