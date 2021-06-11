package com.braintree.command.request;

import de.hybris.platform.payment.commands.request.AbstractRequest;

import java.math.BigDecimal;


public class BrainTreeCloneTransactionRequest extends AbstractRequest
{
	private String transactionId;
	private Boolean submitForSettlement;
	private BigDecimal amount;

	public BrainTreeCloneTransactionRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	public Boolean isSubmitForSettlement()
	{
		return submitForSettlement;
	}

	public void setSubmitForSettlement(final Boolean submitForSettlement)
	{
		this.submitForSettlement = submitForSettlement;
	}

	public BigDecimal getAmount()
	{
		return amount;
	}

	public void setAmount(final BigDecimal amount)
	{
		this.amount = amount;
	}

	public String getTransactionId()
	{
		return transactionId;
	}

	public void setTransactionId(final String transactionId)
	{
		this.transactionId = transactionId;
	}
}
