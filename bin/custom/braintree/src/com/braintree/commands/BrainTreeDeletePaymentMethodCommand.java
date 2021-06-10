/**
 *
 */
package com.braintree.commands;

import com.braintree.command.request.BrainTreeDeletePaymentMethodRequest;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import de.hybris.platform.payment.commands.Command;


public interface BrainTreeDeletePaymentMethodCommand extends
		Command<BrainTreeDeletePaymentMethodRequest, BrainTreePaymentMethodResult>
{
	//interface
}
