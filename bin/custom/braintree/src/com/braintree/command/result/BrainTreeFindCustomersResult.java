/**
 *
 */
package com.braintree.command.result;

import com.braintreegateway.Customer;
import com.braintreegateway.ResourceCollection;


public class BrainTreeFindCustomersResult extends BrainTreeAbstractResult
{
	private ResourceCollection<Customer> customers = null;

	public BrainTreeFindCustomersResult()
	{
	}

	public BrainTreeFindCustomersResult(final ResourceCollection<Customer> customers)
	{
		this.customers = customers;
	}

	public ResourceCollection<Customer> getCustomers()
	{
		return customers;
	}
}
