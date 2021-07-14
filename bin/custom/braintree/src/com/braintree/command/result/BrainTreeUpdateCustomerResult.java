/**
 *
 */
package com.braintree.command.result;

import com.braintreegateway.Customer;


public class BrainTreeUpdateCustomerResult extends BrainTreeAbstractResult
{
	private Customer customer;

	public BrainTreeUpdateCustomerResult()
	{
	}

	/**
	 * @return the customer
	 */
	public Customer getCustomer()
	{
		return customer;
	}

	/**
	 * @param customer
	 *           the customer to set
	 */
	public void setCustomer(final Customer customer)
	{
		this.customer = customer;
	}

}
