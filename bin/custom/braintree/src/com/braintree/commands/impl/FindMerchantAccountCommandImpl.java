package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeFindMerchantAccountRequest;
import com.braintree.command.result.BrainTreeFindMerchantAccountResult;
import com.braintree.commands.BrainTreeFindMerchantAccountCommand;
import com.braintreegateway.MerchantAccount;
import com.braintreegateway.exceptions.NotFoundException;
import org.apache.log4j.Logger;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class FindMerchantAccountCommandImpl extends
		AbstractCommand<BrainTreeFindMerchantAccountRequest, BrainTreeFindMerchantAccountResult> implements
		BrainTreeFindMerchantAccountCommand
{
	private final static Logger LOG = Logger.getLogger(FindMerchantAccountCommandImpl.class);

	@Override
	public BrainTreeFindMerchantAccountResult perform(final BrainTreeFindMerchantAccountRequest request)
	{
		validateParameterNotNullStandardMessage("Find Merchant Request", request);
		final String merchantAccountId = request.getMerchantAccount();
		validateParameterNotNullStandardMessage("merchantAccount", merchantAccountId);
		LOG.info("merchantAccountId: " + merchantAccountId);
		try
		{
			final MerchantAccount merchantAccount = getBraintreeGateway().merchantAccount().find(merchantAccountId);
			boolean isMerchantAccountExist = false;
			if (merchantAccount != null)
			{
				isMerchantAccountExist = true;
			}
			return new BrainTreeFindMerchantAccountResult(isMerchantAccountExist);
		}
		catch (final Exception exception)
		{
			LOG.error("Exception occurred, message: " + exception.getMessage(), exception);
			if (exception instanceof NotFoundException)
			{
				LOG.error("Can't find Braintree Merchant, merchantAccountId: " + merchantAccountId, exception);
				return new BrainTreeFindMerchantAccountResult(false);
			}
			else
			{
				LOG.error("Error while search for Merchant, merchantAccountId: " + merchantAccountId, exception);
				throw new IllegalArgumentException(exception.getMessage());
			}
		}
	}

}
