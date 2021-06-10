package com.braintree.command.result;

import com.braintreegateway.Transaction;


public class BrainTreeAbstractTransactionResult extends BrainTreeAbstractResult
{
	protected String transactionId;

	private Transaction transaction;

	public String getTransactionId()
	{
		return transactionId;
	}

	public void setTransactionId(final String transactionId)
	{
		this.transactionId = transactionId;
	}

	public Transaction getTransaction()
	{
		return transaction;
	}

	public void setTransaction(final Transaction transaction)
	{
		this.transaction = transaction;
	}
}
