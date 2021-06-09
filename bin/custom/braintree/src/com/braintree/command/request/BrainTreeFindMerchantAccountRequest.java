/**
 *
 */
package com.braintree.command.request;

public class BrainTreeFindMerchantAccountRequest extends BrainTreeAbstractRequest
{
	private String merchantAccount;

	public BrainTreeFindMerchantAccountRequest(final String merchantTransactionCode)
	{
		super(merchantTransactionCode);
	}

	public String getMerchantAccount()
	{
		return merchantAccount;
	}

	public void setMerchantAccount(final String merchantAccount)
	{
		this.merchantAccount = merchantAccount;
	}

}
