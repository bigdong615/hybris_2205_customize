package com.braintree.facade.backoffice;

import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.hybris.data.BrainTreeResponseResultData;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;

import java.math.BigDecimal;

public interface BraintreeBackofficePartialRefundFacade {

    boolean isPartialRefundPossible(final OrderModel order);

    BrainTreeResponseResultData partialRefundTransaction(final OrderModel order, final PaymentTransactionEntryModel paymentTransactionEntry,
                                                         final BigDecimal amount) throws BraintreeErrorException;

}
