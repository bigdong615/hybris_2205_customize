package com.braintree.customersupportbackoffice.editor;

import com.hybris.cockpitng.editor.defaultenum.DefaultEnumEditor;

import java.util.List;

public class TransactionRiskDecisionEditor extends DefaultEnumEditor {
    @Override
    protected List<Object> getAllValues(String valueType, Object initialValue) {
        return super.getAllValues("java.lang.Enum(BraintreeTransactionRiskDecision)", initialValue);
    }
}
