/**
 *
 */
package com.braintree.commands;

import com.braintree.command.request.BrainTreeFindMerchantAccountRequest;
import com.braintree.command.result.BrainTreeFindMerchantAccountResult;
import de.hybris.platform.payment.commands.Command;


public interface BrainTreeFindMerchantAccountCommand extends
		Command<BrainTreeFindMerchantAccountRequest, BrainTreeFindMerchantAccountResult>
{
	@Override
	BrainTreeFindMerchantAccountResult perform(final BrainTreeFindMerchantAccountRequest request);
}
