package com.braintree.order.refund.partial.services;

import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.exceptions.BraintreeErrorException;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;

import java.math.BigDecimal;


public interface BraintreePartialRefundService
{

	BrainTreeRefundTransactionResult partialRefundTransaction(final OrderModel order,
			final PaymentTransactionEntryModel paymentTransactionEntry,
			final BigDecimal amountString) throws BraintreeErrorException;

	boolean eligibleForPartialRefund(PaymentTransactionEntryModel paymentEntry);

	boolean isValidTransactionId(OrderModel orderModel, String transactionId);

}
