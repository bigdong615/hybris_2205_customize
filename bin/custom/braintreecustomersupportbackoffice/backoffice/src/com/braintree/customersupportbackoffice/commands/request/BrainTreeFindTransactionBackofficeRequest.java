package com.braintree.customersupportbackoffice.commands.request;

import com.braintree.command.request.BrainTreeFindTransactionRequest;
import com.hybris.cockpitng.search.data.ValueComparisonOperator;

public class BrainTreeFindTransactionBackofficeRequest extends BrainTreeFindTransactionRequest {
    private ValueComparisonOperator customerEmailOperator;
    private String riskDecision;

    public BrainTreeFindTransactionBackofficeRequest(String merchantTransactionCode) {
        super(merchantTransactionCode);
    }

    public ValueComparisonOperator getCustomerEmailOperator() {
        return customerEmailOperator;
    }

    public void setCustomerEmailOperator(ValueComparisonOperator customerEmailOperator) {
        this.customerEmailOperator = customerEmailOperator;
    }

    public String getRiskDecision() {
        return riskDecision;
    }

    public void setRiskDecision(String riskDecision) {
        this.riskDecision = riskDecision;
    }
}
