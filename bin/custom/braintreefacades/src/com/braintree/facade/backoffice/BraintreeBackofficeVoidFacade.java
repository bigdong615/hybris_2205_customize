package com.braintree.facade.backoffice;

import java.util.List;

import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.exceptions.BraintreeErrorException;

import com.braintree.hybris.data.BrainTreeResponseResultData;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;

public interface BraintreeBackofficeVoidFacade {

    boolean isVoidPossible(final OrderModel order);

    List<PaymentTransactionEntryModel> getVoidableTransactions(final OrderModel order);

    void executeVoid(final PaymentTransactionEntryModel transactionId) throws BraintreeErrorException;

    boolean isVoidAuthorizationPossible(final OrderModel order);

    public BrainTreePaymentMethodResult deletePaymentMethod(final BrainTreePaymentInfoModel paymentInfo);

    public BrainTreeResponseResultData deletePaymentMethod(BrainTreePaymentInfoModel paymentInfoModel, PaymentTransactionEntryModel paymentTransactionEntryModel);

}
