package com.braintree.order.capture.partial.strategy;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;

import java.math.BigDecimal;
import java.util.Iterator;
import java.util.List;


public class BraintreePartialOrderCalculationStrategyImpl implements BraintreePartialOrderCalculationStrategy
{

	@Override
	public BigDecimal calculateCaptureAmount(OrderModel orderModel)
	{
		List<PaymentTransactionModel> paymentTransactions = orderModel.getPaymentTransactions();
		Iterator<PaymentTransactionModel> iterator = paymentTransactions.iterator();
		if (iterator.hasNext())
		{
			PaymentTransactionModel paymentTransactionModel = iterator.next();
			if (paymentTransactionModel != null)
			{
				return calculateTransaction(paymentTransactionModel, orderModel.getTotalPrice());
			}
		}
		return BigDecimal.ZERO;
	}

	@Override
	public BigDecimal calculateTransaction(PaymentTransactionModel paymentTransactionModel, Double totalAmount)
	{
		BigDecimal amount = BigDecimal.ZERO;
		List<PaymentTransactionEntryModel> entries = paymentTransactionModel.getEntries();
		for (PaymentTransactionEntryModel entryModel : entries)
		{
			if (PaymentTransactionType.PARTIAL_CAPTURE.equals(entryModel.getType())
					&& TransactionStatus.ACCEPTED.name().equals(entryModel.getTransactionStatus())
					&& TransactionStatusDetails.SUCCESFULL.name().equals(entryModel.getTransactionStatusDetails()))
			{
				totalAmount = paymentTransactionModel.getOrder().getTotalPrice();
				amount = amount.add(entryModel.getAmount());
			}
		}
		return BigDecimal.valueOf(totalAmount).subtract(amount);
	}
}
