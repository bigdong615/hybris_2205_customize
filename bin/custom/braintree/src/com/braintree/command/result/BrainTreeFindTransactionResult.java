package com.braintree.command.result;

import com.braintreegateway.ResourceCollection;
import com.braintreegateway.Transaction;


public class BrainTreeFindTransactionResult extends BrainTreeAbstractResult
{
	private ResourceCollection<Transaction> transactions;

	public BrainTreeFindTransactionResult()
	{
	}

	public BrainTreeFindTransactionResult(final ResourceCollection<Transaction> transactions)
	{
		this.transactions = transactions;
	}

	public ResourceCollection<Transaction> getTransactions()
	{
		return transactions;
	}
}
