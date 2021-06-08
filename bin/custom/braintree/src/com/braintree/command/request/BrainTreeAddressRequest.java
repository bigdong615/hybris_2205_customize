/**
 *
 */
package com.braintree.command.request;

public class BrainTreeAddressRequest extends BrainTreeAbstractRequest
{
	private String firstName;
	private String lastName;
	private String company;
	private String streetAddress;
	private String extendedAddress;
	private String locality;
	private String region;
	private String postalCode;
	private String countryCodeAlpha2;
	private String customerId;
	private String addressId;

	public BrainTreeAddressRequest(final String customerId)
	{
		super(customerId);
		this.customerId = customerId;
	}

	public String getAddressId()
	{
		return addressId;
	}

	public void setAddressId(final String addressId)
	{
		this.addressId = addressId;
	}

	public String getCustomerId()
	{
		return customerId;
	}

	public void setCustomerId(final String customerId)
	{
		this.customerId = customerId;
	}

	public String getFirstName()
	{
		return firstName;
	}

	public void setFirstName(final String firstName)
	{
		this.firstName = firstName;
	}

	public String getLastName()
	{
		return lastName;
	}

	public void setLastName(final String lastName)
	{
		this.lastName = lastName;
	}

	public String getCompany()
	{
		return company;
	}

	public void setCompany(final String company)
	{
		this.company = company;
	}

	public String getStreetAddress()
	{
		return streetAddress;
	}

	public void setStreetAddress(final String streetAddress)
	{
		this.streetAddress = streetAddress;
	}

	public String getExtendedAddress()
	{
		return extendedAddress;
	}

	public void setExtendedAddress(final String extendedAddress)
	{
		this.extendedAddress = extendedAddress;
	}

	public String getLocality()
	{
		return locality;
	}

	public void setLocality(final String locality)
	{
		this.locality = locality;
	}

	public String getRegion()
	{
		return region;
	}

	public void setRegion(final String region)
	{
		this.region = region;
	}

	public String getPostalCode()
	{
		return postalCode;
	}

	public void setPostalCode(final String postalCode)
	{
		this.postalCode = postalCode;
	}

	public String getCountryCodeAlpha2()
	{
		return countryCodeAlpha2;
	}

	public void setCountryCodeAlpha2(final String countryCodeAlpha2)
	{
		this.countryCodeAlpha2 = countryCodeAlpha2;
	}
}
