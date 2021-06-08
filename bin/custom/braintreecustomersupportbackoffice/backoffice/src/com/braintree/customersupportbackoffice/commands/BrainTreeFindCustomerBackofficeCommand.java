package com.braintree.customersupportbackoffice.commands;

import com.braintree.command.result.BrainTreeFindCustomerResult;
import com.braintree.command.result.BrainTreeFindCustomersResult;
import com.braintree.customersupportbackoffice.commands.request.BrainTreeCustomerBackofficeRequest;
import de.hybris.platform.payment.commands.Command;

public interface BrainTreeFindCustomerBackofficeCommand extends Command<BrainTreeCustomerBackofficeRequest, BrainTreeFindCustomerResult> {
    public BrainTreeFindCustomersResult process(final BrainTreeCustomerBackofficeRequest request);
}
