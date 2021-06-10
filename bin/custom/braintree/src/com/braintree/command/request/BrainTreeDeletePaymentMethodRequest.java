/**
 *
 */
package com.braintree.command.request;

public class BrainTreeDeletePaymentMethodRequest extends BrainTreeAbstractRequest
{
	private final String token;

	public BrainTreeDeletePaymentMethodRequest(final String merchantTransactionCode, final String token)
	{
		super(merchantTransactionCode);
		this.token = token;
	}

	public String getToken()
	{
		return token;
	}
}
