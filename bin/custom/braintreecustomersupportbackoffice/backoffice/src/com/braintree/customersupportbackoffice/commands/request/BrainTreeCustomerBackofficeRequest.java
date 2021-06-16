package com.braintree.customersupportbackoffice.commands.request;

import com.braintree.command.request.BrainTreeCustomerRequest;
import com.hybris.cockpitng.search.data.ValueComparisonOperator;

public class BrainTreeCustomerBackofficeRequest extends BrainTreeCustomerRequest {
    private ValueComparisonOperator customerEmailOperator;

    /**
     * @param merchantTransactionCode
     */
    public BrainTreeCustomerBackofficeRequest(String merchantTransactionCode) {
        super(merchantTransactionCode);
    }

    public ValueComparisonOperator getCustomerEmailOperator() {
        return customerEmailOperator;
    }

    public void setCustomerEmailOperator(ValueComparisonOperator customerEmailOperator) {
        this.customerEmailOperator = customerEmailOperator;
    }
}
