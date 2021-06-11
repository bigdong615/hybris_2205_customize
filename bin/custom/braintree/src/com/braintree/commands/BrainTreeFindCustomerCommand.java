/**
 *
 */
package com.braintree.commands;

import com.braintree.command.request.BrainTreeCustomerRequest;
import com.braintree.command.result.BrainTreeFindCustomerResult;
import com.braintree.command.result.BrainTreeFindCustomersResult;
import de.hybris.platform.payment.commands.Command;


public interface BrainTreeFindCustomerCommand extends Command<BrainTreeCustomerRequest, BrainTreeFindCustomerResult>
{
	BrainTreeFindCustomersResult process(final BrainTreeCustomerRequest request);
}
