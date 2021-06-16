package com.braintree.customersupportbackoffice.services.impl;

import com.braintree.command.result.BrainTreeFindTransactionResult;
import com.braintree.customersupportbackoffice.commands.BrainTreeFindTransactionBackofficeCommand;
import com.braintree.customersupportbackoffice.commands.request.BrainTreeFindTransactionBackofficeRequest;
import com.braintree.customersupportbackoffice.services.TransactionSearchService;
import de.hybris.platform.payment.AdapterException;
import org.springframework.beans.factory.annotation.Required;

public class BraintreeTransactionSearchServiceImpl implements TransactionSearchService {
    private BrainTreeFindTransactionBackofficeCommand brainTreeFindTransactionBackofficeCommand;

    @Override
    public BrainTreeFindTransactionResult findTransactions(BrainTreeFindTransactionBackofficeRequest findTransactionRequest) throws AdapterException {
        return brainTreeFindTransactionBackofficeCommand.perform(findTransactionRequest);
    }

    @Required
    @SuppressWarnings("unused")
    public void setBrainTreeFindTransactionBackofficeCommand(BrainTreeFindTransactionBackofficeCommand brainTreeFindTransactionBackofficeCommand) {
        this.brainTreeFindTransactionBackofficeCommand = brainTreeFindTransactionBackofficeCommand;
    }
}
