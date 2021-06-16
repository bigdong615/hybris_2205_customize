package com.braintree.order.refund;

import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.refund.impl.DefaultRefundService;
import de.hybris.platform.returns.model.ReturnRequestModel;
import org.apache.commons.collections.CollectionUtils;
import org.apache.log4j.Logger;
import org.springframework.util.Assert;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class DefaultBraintreeRefundService extends DefaultRefundService implements BraintreeRefundService
{
	private static final Logger LOG = Logger.getLogger(DefaultBraintreeRefundService.class);

	private BrainTreePaymentService brainTreePaymentService;
	private BrainTreeTransactionService brainTreeTransactionService;

	@Override
	public void applyBraintreeRefund(final OrderModel refundOrder, final OrderModel originalOrder,
			final ReturnRequestModel returnRequestModel)
	{
		final Double orderAmountBeforeRefund = originalOrder.getTotalPrice();

		braintreeRefundProcess(orderAmountBeforeRefund, refundOrder);

		getModelService().save(originalOrder);
		getModelService().refresh(originalOrder);
	}

	private void braintreeRefundProcess(final Double orderAmountBeforeRefund, final OrderModel refundOrder)
	{
		final Double orderAmountAfterRefund = refundOrder.getTotalPrice();

		if (!orderAmountAfterRefund.equals(orderAmountBeforeRefund))
		{
			final BigDecimal refundAmount = BigDecimal.valueOf(orderAmountBeforeRefund)
					.subtract(BigDecimal.valueOf(orderAmountAfterRefund))
					.setScale(getCurrencyDigit(refundOrder), RoundingMode.HALF_EVEN);

			processRefund(refundOrder, refundAmount);
		}
	}

	private void processRefund(OrderModel refundOrder, BigDecimal refundAmount)
	{
		final List<PaymentTransactionModel> paymentTransactions = refundOrder.getPaymentTransactions();
		if (CollectionUtils.isNotEmpty(paymentTransactions))
		{
			final PaymentTransactionModel transactionModel = paymentTransactions.iterator().next();
			final PaymentTransactionEntryModel refundTransactionEntry = processBrainTreeRefund(refundAmount,
					refundOrder.getCode(), transactionModel);
			storeOrderTransactionEntryFromHistory(refundOrder, refundTransactionEntry);
		}
	}

	private PaymentTransactionEntryModel processBrainTreeRefund(final BigDecimal refundAmount, final String orderId,
			final PaymentTransactionModel transaction)
	{
		validateParameterNotNullStandardMessage("transaction", transaction);
		LOG.info(
				"processBrainTreeRefund, orderId: " + orderId + ", transaction:" + transaction + ", transaction.reqId:" + transaction
						.getRequestId() + ", refundAmount: " + refundAmount);

		final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(transaction.getRequestId());
		request.setTransactionId(transaction.getRequestId());
		request.setAmount(refundAmount);
		request.setOrderId(orderId);

		final BrainTreeRefundTransactionResult brainTreeRefundTransactionResult = getBrainTreePaymentService()
				.refundTransaction(request);

		if (brainTreeRefundTransactionResult.isSuccess())
		{
			return getBrainTreeTransactionService().createRefundTransaction(transaction, brainTreeRefundTransactionResult);
		}
		else
		{
			LOG.error("Error: " + brainTreeRefundTransactionResult.getErrorMessage());
			throw new AdapterException(brainTreeRefundTransactionResult.getErrorMessage());
		}
	}

	private int getCurrencyDigit(final OrderModel order)
	{
		final CurrencyModel currency = order.getCurrency();
		Assert.notNull(currency, "Order " + order.getCode() + " has got a null currency. Cannot calculate refund");
		final Integer digits = currency.getDigits();
		Assert.notNull(digits, "Order " + order.getCode()
				+ " has got a currency without decimal digits defined. Cannot calculate refund");
		return digits.intValue();
	}

	private void storeOrderTransactionEntryFromHistory(OrderModel refundOrder, PaymentTransactionEntryModel refundTransactionEntry)
	{
		OrderModel originalOrder = refundOrder.getOriginalVersion();

		if (originalOrder != null && refundTransactionEntry != null)
		{
			List<PaymentTransactionModel> paymentTransactions = originalOrder.getPaymentTransactions();
			if (CollectionUtils.isNotEmpty(paymentTransactions))
			{
				saveRefundTransaction(originalOrder, refundTransactionEntry, paymentTransactions);
			}
		}
	}

	private void saveRefundTransaction(OrderModel originalOrder, PaymentTransactionEntryModel refundBrainTreeTransaction,
			List<PaymentTransactionModel> paymentTransactions)
	{
		PaymentTransactionModel originalTransaction = paymentTransactions.iterator().next();
		List<PaymentTransactionEntryModel> entries = originalTransaction.getEntries();
		if (CollectionUtils.isNotEmpty(entries))
		{
			PaymentTransactionEntryModel cloneEntry = getModelService().clone(refundBrainTreeTransaction,
					PaymentTransactionEntryModel.class);
			cloneEntry.setPaymentTransaction(originalTransaction);
			getModelService().saveAll(cloneEntry, originalTransaction, originalOrder);
			getModelService().refresh(originalOrder);
		}
	}

	@Override
	public void applyBraintreeRefund(ReturnRequestModel returnRequestModel, Double refundAmount)
	{
		OrderModel orderModel = returnRequestModel.getOrder();
		BigDecimal refAmount = BigDecimal.valueOf(refundAmount);

		processRefund(orderModel, refAmount);

		getModelService().save(orderModel);
		getModelService().refresh(orderModel);
	}

	public BrainTreePaymentService getBrainTreePaymentService()
	{
		return brainTreePaymentService;
	}

	public void setBrainTreePaymentService(final BrainTreePaymentService brainTreePaymentService)
	{
		this.brainTreePaymentService = brainTreePaymentService;
	}


	public BrainTreeTransactionService getBrainTreeTransactionService()
	{
		return brainTreeTransactionService;
	}

	public void setBrainTreeTransactionService(final BrainTreeTransactionService brainTreeTransactionService)
	{
		this.brainTreeTransactionService = brainTreeTransactionService;
	}

}
