package com.braintree.facade.backoffice;

import java.util.List;

import com.braintree.exceptions.BraintreeErrorException;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;

public interface BraintreeBackofficeMultiCaptureFacade {

    boolean isMultiCapturePossible(final OrderModel order);

    List<PaymentTransactionEntryModel> getMultiCaptureableTransactions(final OrderModel order);

    void executeMultiCapture(final PaymentTransactionEntryModel transactionId) throws BraintreeErrorException;
}
