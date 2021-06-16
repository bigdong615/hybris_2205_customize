package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeFindTransactionRequest;
import com.braintree.command.result.BrainTreeFindTransactionResult;
import com.braintree.commands.BrainTreeFindTransactionCommand;
import com.braintreegateway.ResourceCollection;
import com.braintreegateway.Transaction;
import com.braintreegateway.TransactionSearchRequest;
import com.braintreegateway.exceptions.NotFoundException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class FindTransactionCommandImpl extends AbstractCommand implements BrainTreeFindTransactionCommand
{
	private final static Logger LOG = Logger.getLogger(FindTransactionCommandImpl.class);

	@Override
	public BrainTreeFindTransactionResult perform(final BrainTreeFindTransactionRequest request)
	{
		validateParameterNotNullStandardMessage("Find Transaction Request", request);
		try
		{
			final TransactionSearchRequest transactionSearchRequest = translateRequest(request);
			final ResourceCollection<Transaction> transactions = getBraintreeGateway().transaction()
					.search(transactionSearchRequest);
			return new BrainTreeFindTransactionResult(transactions);
		}
		catch (final Exception exception)
		{
			if (exception instanceof NotFoundException)
			{
				LOG.error("[BT Find Transaction] Can't find BrainTree customer with id: ");
				return new BrainTreeFindTransactionResult();
			}
			else
			{
				LOG.error("[BT Find Transaction] Error during try to find Transaction: ");
				throw new IllegalArgumentException(exception.getMessage());
			}
		}
	}

	private TransactionSearchRequest translateRequest(final BrainTreeFindTransactionRequest request)
	{
		final TransactionSearchRequest transactionSearchRequest = new TransactionSearchRequest();
		if (request != null)
		{
			if (request.getStartDate() != null && request.getEndDate() != null)
			{
				transactionSearchRequest.createdAt().between(request.getStartDate(), request.getEndDate());
			}
			if (StringUtils.isNotBlank(request.getTransactionId()))
			{
				transactionSearchRequest.id().is(request.getTransactionId());
			}
			if (StringUtils.isNotBlank(request.getCustomerId()))
			{
				transactionSearchRequest.customerId().is(request.getCustomerId());
			}
			if (StringUtils.isNotBlank(request.getCustomerEmail()))
			{
				transactionSearchRequest.customerEmail().is(request.getCustomerEmail());
			}
			if (StringUtils.isNotBlank(request.getTransactionStatus()))
			{
				transactionSearchRequest.status().is(Transaction.Status.valueOf(request.getTransactionStatus()));
			}
		}
		return transactionSearchRequest;
	}
}
