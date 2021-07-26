package com.braintree.customersupportbackoffice.services.impl;

import com.braintree.constants.BraintreeConstants;
import com.braintree.converters.utils.BraintreeTransactionConverterUtils;
import com.braintree.customersupportbackoffice.services.BrainTreeTransactionManagementActionService;
import com.braintree.model.BrainTreeTransactionDetailModel;
import com.braintreegateway.Transaction;

public class BrainTreeTransactionManagementActionServiceImpl implements BrainTreeTransactionManagementActionService {

    @Override
    public boolean isVoidPossible(BrainTreeTransactionDetailModel transaction)
    {
        if (transaction != null)
        {
            final String status = transaction.getStatus();
            return Transaction.Status.AUTHORIZED.toString().equals(status);
        }
        return false;
    }

    @Override
    public boolean isClonePossible(BrainTreeTransactionDetailModel transaction)
    {
        if (transaction != null)
        {
            final String status = transaction.getStatus();
            if (BraintreeConstants.CREDIT_CARD_PAYMENT_TYPE.equals(transaction.getPaymentType()))
            {
                return Transaction.Status.AUTHORIZED.toString().equals(status)
                        || Transaction.Status.AUTHORIZATION_EXPIRED.toString().equals(status)
                        || Transaction.Status.VOIDED.toString().equals(status) || Transaction.Status.SETTLED.toString().equals(status)
                        || Transaction.Status.SUBMITTED_FOR_SETTLEMENT.toString().equals(status);
            }
            return false;
        }
        return false;
    }

    @Override
    public boolean isRefundPossible(BrainTreeTransactionDetailModel transaction)
    {
        if (transaction != null)
        {
            final String status = transaction.getStatus();

            if (BraintreeConstants.PAYPAL_PAYMENT_TYPE_NAME.equals(transaction.getPaymentType()))
            {
                return (Transaction.Status.SETTLED.toString().equals(status) || Transaction.Status.SETTLING.toString().equals(status)
                        || Transaction.Status.SETTLEMENT_PENDING.toString().equals(status))
                        && !BraintreeTransactionConverterUtils.TRANSACTION_REFUND_SIGN.equals(transaction.getRefund());
            }
            return (Transaction.Status.SETTLED.toString().equals(status) || Transaction.Status.SETTLING.toString().equals(status))
                    && !BraintreeTransactionConverterUtils.TRANSACTION_REFUND_SIGN.equals(transaction.getRefund());
        }
        return false;
    }

    @Override
    public boolean isSubmitForSettlementPossible(BrainTreeTransactionDetailModel transaction)
    {
        if (transaction != null)
        {
            final String status = transaction.getStatus();
            return Transaction.Status.AUTHORIZED.toString().equals(status);
        }
        return false;
    }
}
