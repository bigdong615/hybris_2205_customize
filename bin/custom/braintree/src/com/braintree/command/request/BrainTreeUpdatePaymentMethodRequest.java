package com.braintree.command.request;


public class BrainTreeUpdatePaymentMethodRequest extends BrainTreeAbstractRequest
{
	private String token;
	private String newToken;
	private String cardholderName;
	private String creditCardNumber;
	private String cardExpirationDate;
	private String cvv;
	private boolean isDefault;
	private String billingAddressId;

	public BrainTreeUpdatePaymentMethodRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	public String getToken()
	{
		return token;
	}

	public void setToken(final String token)
	{
		this.token = token;
	}

	public String getCardholderName()
	{
		return cardholderName;
	}

	public void setCardholderName(final String cardholderName)
	{
		this.cardholderName = cardholderName;
	}

	public String getCreditCardNumber()
	{
		return creditCardNumber;
	}

	public void setCreditCardNumber(final String creditCardNumber)
	{
		this.creditCardNumber = creditCardNumber;
	}

	public String getCardExpirationDate()
	{
		return cardExpirationDate;
	}

	public void setCardExpirationDate(final String cardExpirationDate)
	{
		this.cardExpirationDate = cardExpirationDate;
	}

	public boolean isDefault()
	{
		return isDefault;
	}

	public void setDefault(final boolean aDefault)
	{
		isDefault = aDefault;
	}

	public String getNewToken()
	{
		return newToken;
	}

	public void setNewToken(final String newToken)
	{
		this.newToken = newToken;
	}

	public String getBillingAddressId()
	{
		return billingAddressId;
	}

	public void setBillingAddressId(final String billingAddressId)
	{
		this.billingAddressId = billingAddressId;
	}

	public String getCvv()
	{
		return cvv;
	}

	public void setCvv(String cvv)
	{
		this.cvv = cvv;
	}
}
