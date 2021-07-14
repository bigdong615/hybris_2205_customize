/**
 *
 */
package com.braintree.command.request;

import com.braintreegateway.PaymentMethodRequest;


public class BrainTreeCreateCreditCardPaymentMethodRequest extends BrainTreeAbstractRequest
{
	private PaymentMethodRequest request;

	public BrainTreeCreateCreditCardPaymentMethodRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	public PaymentMethodRequest getRequest()
	{
		return request;
	}

	public void setRequest(final PaymentMethodRequest request)
	{
		this.request = request;
	}

}
