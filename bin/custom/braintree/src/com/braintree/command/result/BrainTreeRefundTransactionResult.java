package com.braintree.command.result;

import java.math.BigDecimal;


public class BrainTreeRefundTransactionResult extends BrainTreeAbstractTransactionResult
{
	private BigDecimal amount;
	private String currencyIsoCode;
	private String orderId;

	public BrainTreeRefundTransactionResult(final boolean success)
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

	public void setCurrencyIsoCode(final String currencyIsoCode)
	{
		this.currencyIsoCode = currencyIsoCode;
	}

	public String getOrderId()
	{
		return orderId;
	}

	public void setOrderId(final String orderId)
	{
		this.orderId = orderId;
	}

}
