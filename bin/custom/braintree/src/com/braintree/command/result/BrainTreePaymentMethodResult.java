/**
 *
 */
package com.braintree.command.result;

import com.braintreegateway.PaymentMethod;


public class BrainTreePaymentMethodResult extends BrainTreeCreatePaymentMethodResult
{
	private PaymentMethod paymentMethod;
	private Boolean isDefault;
	
	public BrainTreePaymentMethodResult( final PaymentMethod paymentMethod, Boolean isDefault) {
		super();
		this.paymentMethod = paymentMethod;
		this.isDefault = isDefault;
	}

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
