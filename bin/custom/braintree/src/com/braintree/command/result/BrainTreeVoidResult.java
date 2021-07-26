package com.braintree.command.result;

import java.math.BigDecimal;


public class BrainTreeVoidResult extends BrainTreeAbstractTransactionResult
{
	private BigDecimal amount;
	private String currencyIsoCode;

	public BrainTreeVoidResult(final boolean success)
	{
		setSuccess(success);
	}

	public BigDecimal getAmount()
	{
		return amount;
	}

	public void setAmount(final BigDecimal amount)
	{
		this.amount = amount;
	}

	public String getCurrencyIsoCode()
	{
		return currencyIsoCode;
	}

	public void setCurrencyIsoCode(String currencyIsoCode)
	{
		this.currencyIsoCode = currencyIsoCode;
	}
}
