package com.braintree.customersupportbackoffice.services.impl;

import com.braintree.command.result.BrainTreeFindCustomersResult;
import com.braintree.customersupportbackoffice.commands.BrainTreeFindCustomerBackofficeCommand;
import com.braintree.customersupportbackoffice.commands.request.BrainTreeCustomerBackofficeRequest;
import com.braintree.customersupportbackoffice.services.CustomerSearchService;
import de.hybris.platform.payment.AdapterException;
import org.springframework.beans.factory.annotation.Required;


public class BraintreeCustomerSearchServiceImpl implements CustomerSearchService {
    private BrainTreeFindCustomerBackofficeCommand brainTreeFindCustomerBackofficeCommand;

    @Override
    public BrainTreeFindCustomersResult findCustomers(BrainTreeCustomerBackofficeRequest findCustomerRequest) throws AdapterException {
        return brainTreeFindCustomerBackofficeCommand.process(findCustomerRequest);
    }

    @Required
    @SuppressWarnings("unused")
    public void setBrainTreeFindCustomerBackofficeCommand(BrainTreeFindCustomerBackofficeCommand command) {
        this.brainTreeFindCustomerBackofficeCommand = command;
    }
}
