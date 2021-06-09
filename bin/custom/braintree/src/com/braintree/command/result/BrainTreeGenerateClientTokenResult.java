/**
 *
 */
package com.braintree.command.result;

public class BrainTreeGenerateClientTokenResult extends BrainTreeAbstractResult
{
	private String clientToken;

	/**
	 * @param clientToken
	 */
	public BrainTreeGenerateClientTokenResult(final String clientToken)
	{
		super();
		this.clientToken = clientToken;
	}

	/**
	 * @return the clientToken
	 */
	public String getClientToken()
	{
		return clientToken;
	}

	/**
	 * @param clientToken
	 *           the clientToken to set
	 */
	public void setClientToken(final String clientToken)
	{
		this.clientToken = clientToken;
	}
}
