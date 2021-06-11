package com.braintree.customersupportbackoffice.services;

import com.braintree.command.result.BrainTreeFindTransactionResult;
import com.braintree.customersupportbackoffice.commands.request.BrainTreeFindTransactionBackofficeRequest;
import de.hybris.platform.payment.AdapterException;

public interface TransactionSearchService {
    BrainTreeFindTransactionResult findTransactions(BrainTreeFindTransactionBackofficeRequest request) throws AdapterException;
}
