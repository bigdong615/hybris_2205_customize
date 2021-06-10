package com.bl.core.subscription.models;

import java.util.List;


/**
 * ContactRequest for subscribing the emails.
 *
 * @author Sunil Sahu
 */
public class ContactRequest{
	private String contactKey;
	private List<AttributeSet> attributeSets;

	/**
	 * @return the contactKey
	 */
	public String getContactKey()
	{
		return contactKey;
	}

	/**
	 * @param contactKey
	 *           the contactKey to set
	 */
	public void setContactKey(final String contactKey)
	{
		this.contactKey = contactKey;
	}

	/**
	 * @return the attributeSets
	 */
	public List<AttributeSet> getAttributeSets()
	{
		return attributeSets;
	}

	/**
	 * @param attributeSets
	 *           the attributeSets to set
	 */
	public void setAttributeSets(final List<AttributeSet> attributeSets)
	{
		this.attributeSets = attributeSets;
	}
}
