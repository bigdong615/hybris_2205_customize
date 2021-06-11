package com.braintree.exceptions;

public class BraintreeErrorException extends Exception
{

	private String braintreeId;

	public BraintreeErrorException(String message) {
		super(message);
	}

	public BraintreeErrorException(String message, String braintreeId)
	{
		super(message);
		this.braintreeId = braintreeId;
	}

	public String getBraintreeId()
	{
		return braintreeId;
	}

	public void setBraintreeId(String braintreeId)
	{
		this.braintreeId = braintreeId;
	}
}
