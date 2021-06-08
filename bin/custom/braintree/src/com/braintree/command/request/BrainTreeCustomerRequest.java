/**
 *
 */
package com.braintree.command.request;

public class BrainTreeCustomerRequest extends BrainTreeAbstractRequest
{

	private String customerId;
	private String customerEmail;

	/**
	 * @param merchantTransactionCode
	 */
	public BrainTreeCustomerRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	/**
	 * @return the customerId
	 */
	public String getCustomerId()
	{
		return customerId;
	}

	/**
	 * @param customerId
	 *           the customerId to set
	 */
	public void setCustomerId(final String customerId)
	{
		this.customerId = customerId;
	}

	/**
	 * @return the customerEmail
	 */
	public String getCustomerEmail()
	{
		return customerEmail;
	}

	/**
	 * @param customerEmail
	 *           the customerEmail to set
	 */
	public void setCustomerEmail(final String customerEmail)
	{
		this.customerEmail = customerEmail;
	}

	@Override
	public String toString()
	{
		return "BrainTreeCustomerRequest [customerId=" + customerId + ", customerEmail=" + customerEmail + "]";
	}


}
