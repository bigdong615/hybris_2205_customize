/**
 *
 */
package com.braintree.command.result;

import com.braintreegateway.Customer;


public class BrainTreeFindCustomerResult extends BrainTreeAbstractResult
{
	private boolean isCustomerExist;
	private Customer customer;

	public BrainTreeFindCustomerResult()
	{
		super();
	}

	public BrainTreeFindCustomerResult(final boolean isCustomerExist)
	{
		super();
		this.isCustomerExist = isCustomerExist;
	}

	public boolean isCustomerExist()
	{
		return isCustomerExist;
	}

	public void setCustomerExist(final boolean isCustomerExist)
	{
		this.isCustomerExist = isCustomerExist;
	}

	public Customer getCustomer()
	{
		return customer;
	}

	public void setCustomer(Customer customer)
	{
		this.customer = customer;
	}
}
