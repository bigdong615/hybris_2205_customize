package com.braintree.command.result;

import com.braintreegateway.Address;


public class BrainTreeAddressResult extends BrainTreeAbstractResult
{
	private Address address;

	public BrainTreeAddressResult(final Address address)
	{
		super();
		this.address = address;
	}

	public BrainTreeAddressResult()
	{
	}

	public Address getAddress()
	{
		return address;
	}

	public void setAddress(final Address address)
	{
		this.address = address;
	}
}
