/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package de.hybris.platform.acceleratorstorefrontcommons.forms;

/**
 * Form object for updating profile.
 */
public class UpdateProfileForm
{

	private String titleCode;
	private String firstName;
	private String lastName;
	private String email;
	private String fullName;


	/**
	 * @return the fullName
	 */
	public String getFullName()
	{
		return fullName;
	}

	/**
	 * @param fullName
	 *                    the fullName to set
	 */
	public void setFullName(final String fullName)
	{
		this.fullName = fullName;
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
	 *                 the email to set
	 */
	public void setEmail(final String email)
	{
		this.email = email;
	}

	/**
	 * @return the titleCode
	 */
	public String getTitleCode()
	{
		return titleCode;
	}

	/**
	 * @param titleCode
	 *           the titleCode to set
	 */
	public void setTitleCode(final String titleCode)
	{
		this.titleCode = titleCode;
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



}
