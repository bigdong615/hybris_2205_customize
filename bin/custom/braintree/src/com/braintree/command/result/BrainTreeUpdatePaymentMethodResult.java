package com.braintree.command.result;

import com.braintreegateway.PaymentMethod;


public class BrainTreeUpdatePaymentMethodResult extends BrainTreeAbstractResult
{
	PaymentMethod paymentMethod;

	public PaymentMethod getPaymentMethod()
	{
		return paymentMethod;
	}

	public void setPaymentMethod(final PaymentMethod paymentMethod)
	{
		this.paymentMethod = paymentMethod;
	}
}
