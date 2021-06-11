package com.braintree.commands;

import com.braintree.command.request.BrainTreeSaleTransactionRequest;
import com.braintree.command.result.BrainTreeSaleTransactionResult;
import de.hybris.platform.payment.commands.Command;


public interface BrainTreePartialCaptureCommand extends Command<BrainTreeSaleTransactionRequest, BrainTreeSaleTransactionResult>
{
	//interface
}
