package com.braintree.customersupportbackoffice.services;

import com.braintree.model.BrainTreeTransactionDetailModel;

public interface BrainTreeTransactionManagementActionService {
    boolean isVoidPossible(BrainTreeTransactionDetailModel transaction);

    boolean isClonePossible(BrainTreeTransactionDetailModel transaction);

    boolean isRefundPossible(BrainTreeTransactionDetailModel transaction);

    boolean isSubmitForSettlementPossible(BrainTreeTransactionDetailModel transaction);

}
