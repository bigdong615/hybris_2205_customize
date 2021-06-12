/**
 *
 */
package com.braintree.command.request;

public class BrainTreeGenerateClientTokenRequest extends BrainTreeAbstractRequest
{

	private String merchantAccount;

	public BrainTreeGenerateClientTokenRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	/**
	 * @return the merchantAccount
	 */
	public String getMerchantAccount()
	{
		return merchantAccount;
	}

	/**
	 * @param merchantAccount
	 *           the merchantAccount to set
	 */
	public void setMerchantAccount(final String merchantAccount)
	{
		this.merchantAccount = merchantAccount;
	}

}
