package com.braintree.exceptions;

import org.apache.commons.lang.StringUtils;


/**
 * Error message for custom exception or validation that have localization String
 */
public class ResourceErrorMessage
{
	private final String messageKey;
	private final String fieldKey;

	public ResourceErrorMessage(String messageKey, String fieldKey)
	{
		this.messageKey = messageKey;
		this.fieldKey = fieldKey;
	}

	public ResourceErrorMessage(String messageKey)
	{
		this.messageKey = messageKey;
		this.fieldKey = StringUtils.EMPTY;
	}

	public String getMessageKey()
	{
		return messageKey;
	}

	public String getFieldKey()
	{
		return fieldKey;
	}
}
