package com.braintree.command.request;

import java.util.Calendar;


public class BrainTreeFindTransactionRequest extends BrainTreeAbstractRequest
{
	private Calendar startDate;
	private Calendar endDate;
	private String customerId;
	private String customerEmail;
	private String transactionId;
	private String transactionStatus;

	public BrainTreeFindTransactionRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	public Calendar getStartDate()
	{
		return startDate;
	}

	public void setStartDate(final Calendar startDate)
	{
		this.startDate = startDate;
	}

	public Calendar getEndDate()
	{
		return endDate;
	}

	public void setEndDate(final Calendar endDate)
	{
		this.endDate = endDate;
	}

	public String getCustomerId()
	{
		return customerId;
	}

	public void setCustomerId(final String customerId)
	{
		this.customerId = customerId;
	}

	public String getCustomerEmail()
	{
		return customerEmail;
	}

	public void setCustomerEmail(final String customerEmail)
	{
		this.customerEmail = customerEmail;
	}

	public String getTransactionId()
	{
		return transactionId;
	}

	public void setTransactionId(final String transactionId)
	{
		this.transactionId = transactionId;
	}

	public String getTransactionStatus()
	{
		return transactionStatus;
	}

	public void setTransactionStatus(final String transactionStatus)
	{
		this.transactionStatus = transactionStatus;
	}
}
