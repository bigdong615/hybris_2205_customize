/**
 *
 */
package com.braintree.command.result;

import com.braintreegateway.PaymentMethod;


public class BrainTreePaymentMethodResult extends BrainTreeCreatePaymentMethodResult
{
	private PaymentMethod paymentMethod;

	public BrainTreePaymentMethodResult()
	{
	}

	public BrainTreePaymentMethodResult(final PaymentMethod paymentMethod, final boolean success)
	{
		this.paymentMethod = paymentMethod;
		this.success = success;
	}

	public PaymentMethod getPaymentMethod()
	{
		return paymentMethod;
	}

	public void setPaymentMethod(final PaymentMethod paymentMethod)
	{
		this.paymentMethod = paymentMethod;
	}
}
