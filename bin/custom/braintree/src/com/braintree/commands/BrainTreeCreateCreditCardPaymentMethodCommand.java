/**
 *
 */
package com.braintree.commands;

import com.braintree.command.request.BrainTreeCreateCreditCardPaymentMethodRequest;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import de.hybris.platform.payment.commands.Command;


public interface BrainTreeCreateCreditCardPaymentMethodCommand extends
		Command<BrainTreeCreateCreditCardPaymentMethodRequest, BrainTreePaymentMethodResult>
{
	//interface
}
