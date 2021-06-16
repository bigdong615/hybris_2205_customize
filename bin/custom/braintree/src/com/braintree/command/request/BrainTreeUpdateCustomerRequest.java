/**
 *
 */
package com.braintree.command.request;


public class BrainTreeUpdateCustomerRequest extends BrainTreeAbstractRequest
{
	private String id;
	private String firstName;
	private String lastName;
	private String email;
	private String company;
	private String phone;
	private String fax;
	private String website;

	public BrainTreeUpdateCustomerRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	/**
	 * @return the firstName
	 */
	public String getFirstName()
	{
		return firstName;
	}

	/**
	 * @param firstName
	 *           the firstName to set
	 */
	public void setFirstName(final String firstName)
	{
		this.firstName = firstName;
	}

	/**
	 * @return the lastName
	 */
	public String getLastName()
	{
		return lastName;
	}

	/**
	 * @param lastName
	 *           the lastName to set
	 */
	public void setLastName(final String lastName)
	{
		this.lastName = lastName;
	}

	/**
	 * @return the email
	 */
	public String getEmail()
	{
		return email;
	}

	/**
	 * @param email
	 *           the email to set
	 */
	public void setEmail(final String email)
	{
		this.email = email;
	}

	/**
	 * @return the company
	 */
	public String getCompany()
	{
		return company;
	}

	/**
	 * @param company
	 *           the company to set
	 */
	public void setCompany(final String company)
	{
		this.company = company;
	}

	/**
	 * @return the phone
	 */
	public String getPhone()
	{
		return phone;
	}

	/**
	 * @param phone
	 *           the phone to set
	 */
	public void setPhone(final String phone)
	{
		this.phone = phone;
	}

	/**
	 * @return the fax
	 */
	public String getFax()
	{
		return fax;
	}

	/**
	 * @param fax
	 *           the fax to set
	 */
	public void setFax(final String fax)
	{
		this.fax = fax;
	}

	/**
	 * @return the website
	 */
	public String getWebsite()
	{
		return website;
	}

	/**
	 * @param website
	 *           the website to set
	 */
	public void setWebsite(final String website)
	{
		this.website = website;
	}

	/**
	 * @return the id
	 */
	public String getId()
	{
		return id;
	}

	/**
	 * @param id
	 *           the id to set
	 */
	public void setId(final String id)
	{
		this.id = id;
	}


}
