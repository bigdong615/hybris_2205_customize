package com.braintree.command.result;

import com.braintreegateway.Address;
import com.braintreegateway.AndroidPayDetails;
import com.braintreegateway.CreditCard;
import de.hybris.platform.payment.commands.result.AuthorizationResult;

import java.util.Calendar;


public class BrainTreeAuthorizationResult extends AuthorizationResult
{
	private boolean success;
	private Calendar authorizationExpiresAt;
	private CreditCard creditCard;
	private AndroidPayDetails androidPayDetails;
	private String paymentMethodToken;


	public boolean isSuccess()
	{
		return success;
	}

	public void setSuccess(final boolean success)
	{
		this.success = success;
	}

	public Calendar getAuthorizationExpiresAt()
	{
		return authorizationExpiresAt;
	}

	public void setAuthorizationExpiresAt(Calendar authorizationExpiresAt)
	{
		this.authorizationExpiresAt = authorizationExpiresAt;
	}

	public CreditCard getCreditCard()
	{
		return creditCard;
	}

	public void setCreditCard(CreditCard creditCard)
	{
		this.creditCard = creditCard;
	}

	public AndroidPayDetails getAndroidPayDetails()
	{
		return androidPayDetails;
	}

	public void setAndroidPayDetails(AndroidPayDetails androidPayDetails)
	{
		this.androidPayDetails = androidPayDetails;
	}

	public String getPaymentMethodToken()
	{
		return paymentMethodToken;
	}

	public void setPaymentMethodToken(String paymentMethodToken)
	{
		this.paymentMethodToken = paymentMethodToken;
	}
}
