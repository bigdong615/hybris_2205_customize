package com.braintree.command.result;


import java.math.BigDecimal;


public class BrainTreeSaleTransactionResult extends BrainTreeAbstractTransactionResult
{
	private String currencyIsoCode;
	private BigDecimal amount;

	public BrainTreeSaleTransactionResult(final boolean success)
	{
		this.success = success;
	}

	@Override
	public String getTransactionId()
	{
		if (super.getTransactionId() == null && getTransaction() != null)
		{
			return getTransaction().getId();
		}
		return super.getTransactionId();
	}

	public String getCurrencyIsoCode()
	{
		return currencyIsoCode;
	}

	public void setCurrencyIsoCode(String currencyIsoCode)
	{
		this.currencyIsoCode = currencyIsoCode;
	}

	public BigDecimal getAmount()
	{
		return amount;
	}

	public void setAmount(BigDecimal amount)
	{
		this.amount = amount;
	}
}
