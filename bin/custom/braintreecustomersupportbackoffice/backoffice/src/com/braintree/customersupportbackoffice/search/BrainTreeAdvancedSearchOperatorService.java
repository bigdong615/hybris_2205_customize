package com.braintree.customersupportbackoffice.search;

import com.google.common.collect.Lists;
import com.hybris.backoffice.cockpitng.search.DefaultAdvancedSearchOperatorService;
import com.hybris.cockpitng.dataaccess.facades.type.DataAttribute;
import com.hybris.cockpitng.search.data.ValueComparisonOperator;

import java.util.Collection;
import java.util.List;

public class BrainTreeAdvancedSearchOperatorService extends DefaultAdvancedSearchOperatorService {
    @Override
    public Collection<ValueComparisonOperator> getAvailableOperators(DataAttribute dataAttribute) {
        List<ValueComparisonOperator> operators = Lists.newArrayList();
        switch (dataAttribute.getQualifier()) {
            case "braintreeTransactionCustomerID":
            case "braintreeTransactionDetailID":
            case "braintreeTransactionDetailRiskDecision":
            case "braintreeTransactionDetailDateFrom":
            case "braintreeTransactionDetailDateTo":
            case "braintreeTransactionDetailStatus":
                operators.add(ValueComparisonOperator.EQUALS);
                break;
            case "braintreeTransactionCustomerEmail":
                operators.add(ValueComparisonOperator.CONTAINS);
                operators.add(ValueComparisonOperator.EQUALS);
                operators.add(ValueComparisonOperator.STARTS_WITH);
                operators.add(ValueComparisonOperator.ENDS_WITH);
                break;
            default:
                operators.addAll(super.getAvailableOperators(dataAttribute));
                break;
        }
        return operators;
    }
}
