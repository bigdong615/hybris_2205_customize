/**
 *
 */
package com.braintree.command.request;

import com.braintreegateway.PaymentMethodRequest;


public class BrainTreeCreateCreditCardPaymentMethodRequest extends BrainTreeAbstractRequest
{
	private PaymentMethodRequest request;
	private Boolean isDefault;

	/**
	 * @return the isDefault
	 */
	public Boolean getIsDefault() {
		return isDefault;
	}

	/**
	 * @param isDefault the isDefault to set
	 */
	public void setIsDefault(Boolean isDefault) {
		this.isDefault = isDefault;
	}

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
