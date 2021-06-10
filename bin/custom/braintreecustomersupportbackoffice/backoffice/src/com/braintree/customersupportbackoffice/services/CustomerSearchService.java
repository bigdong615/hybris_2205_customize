package com.braintree.customersupportbackoffice.services;

import com.braintree.command.result.BrainTreeFindCustomersResult;
import com.braintree.customersupportbackoffice.commands.request.BrainTreeCustomerBackofficeRequest;
import de.hybris.platform.payment.AdapterException;


public interface CustomerSearchService {
    BrainTreeFindCustomersResult findCustomers(final BrainTreeCustomerBackofficeRequest findCustomerRequest) throws AdapterException;

}
