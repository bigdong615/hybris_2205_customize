/**
 *
 */
package com.braintree.command.result;

import de.hybris.platform.payment.commands.result.AbstractResult;


public abstract class BrainTreeAbstractResult extends AbstractResult
{
	protected boolean success;
	protected String errorCode;
	protected String errorMessage;

	public boolean isSuccess()
	{
		return success;
	}

	public void setSuccess(final boolean success)
	{
		this.success = success;
	}

	public String getErrorMessage()
	{
		return errorMessage;
	}

	public void setErrorMessage(final String errorMessage)
	{
		this.errorMessage = errorMessage;
	}

	public String getErrorCode()
	{
		return errorCode;
	}

	public void setErrorCode(final String errorCode)
	{
		this.errorCode = errorCode;
	}

	@Override
	public String toString()
	{
		return "BrainTreeAbstractResult [success=" + success + ", errorCode=" + errorCode + ", errorMessage=" + errorMessage + "]";
	}
}
