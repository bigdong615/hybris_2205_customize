/**
 *
 */
package com.braintree.command.request;

import de.hybris.platform.payment.commands.request.AbstractRequest;


public abstract class BrainTreeAbstractRequest extends AbstractRequest
{

	private String paymentProvider;

	protected BrainTreeAbstractRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	public String getPaymentProvider()
	{
		return paymentProvider;
	}

	public void setPaymentProvider(final String paymentProvider)
	{
		this.paymentProvider = paymentProvider;
	}
}
