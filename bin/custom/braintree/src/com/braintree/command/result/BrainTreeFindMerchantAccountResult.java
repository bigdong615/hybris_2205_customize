/**
 *
 */
package com.braintree.command.result;


public class BrainTreeFindMerchantAccountResult extends BrainTreeAbstractResult
{
	private boolean isMerchantAccountExist;

	public BrainTreeFindMerchantAccountResult()
	{
		super();
	}

	public BrainTreeFindMerchantAccountResult(final boolean isMerchanAccountExist)
	{
		super();
		this.isMerchantAccountExist = isMerchanAccountExist;
	}

	public boolean isMerchantAccountExist()
	{
		return isMerchantAccountExist;
	}

	public void setMerchantAccountExist(final boolean isMerchantAccountExist)
	{
		this.isMerchantAccountExist = isMerchantAccountExist;
	}
}
