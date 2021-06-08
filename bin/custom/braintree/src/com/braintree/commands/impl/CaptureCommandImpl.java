package com.braintree.commands.impl;

import com.braintree.constants.BraintreeConstants;
import com.braintree.jalo.BrainTreePaymentInfo;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.order.service.OrderRetrievalService;
import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.braintreegateway.TransactionRequest;
import com.braintreegateway.ValidationError;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.payment.commands.CaptureCommand;
import de.hybris.platform.payment.commands.request.CaptureRequest;
import de.hybris.platform.payment.commands.result.CaptureResult;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import org.apache.log4j.Logger;

import java.math.BigDecimal;
import java.util.Currency;
import java.util.List;


public class CaptureCommandImpl extends AbstractCommand implements CaptureCommand
{
	private final static Logger LOG = Logger.getLogger(CaptureCommandImpl.class);

	private CaptureRequest request;

	private OrderRetrievalService orderRetrievalService;

	@Override
	public CaptureResult perform(final CaptureRequest request)
	{
		this.request = request;

		final CaptureResult result;
		final String transactionID = request.getRequestId();

		AbstractOrderModel order = orderRetrievalService.getOrderForTransaction(transactionID);

		if (isAvailableMultiCapture(order))
		{
			return createReviewNeededResponse();
		}
		else
		{
			if (isIntentSale(order) || isAvailableSubmitForSettlement(order))
			{
				result = createSettledResponse(request.getRequestId());
			}
			else
			{
				getLoggingHandler().getLogger().info(String.format("[CAPTURING FOR TRANSACTION CODE: %s]", transactionID));
				final TransactionRequest transactionRequest = new TransactionRequest();

				final String orderId = orderRetrievalService.getOrderCodeForTransaction(transactionID);

				if (orderId != null)
				{
					getLoggingHandler().getLogger().info(String.format("[ORDER CODE: %s]", orderId));
					transactionRequest.orderId(orderId);
				}

				BigDecimal amount = getSubmitForSettlementAmountForOrder(order);
				if (amount != null && amount.compareTo(BigDecimal.ZERO) > 0)
				{
					transactionRequest.amount(amount);
				}

				final Result<Transaction> braintreeReply = getBraintreeGateway().transaction().submitForSettlement(transactionID,
						transactionRequest);
				result = translateResponse(braintreeReply);
			}
		}

		return result;
	}

	private CaptureResult createReviewNeededResponse()
	{
		final CaptureResult captureResult = new CaptureResult();
		captureResult.setTransactionStatus(TransactionStatus.REVIEW);
		captureResult.setTransactionStatusDetails(TransactionStatusDetails.REVIEW_NEEDED);
		captureResult.setCurrency(request.getCurrency());
		captureResult.setRequestId(request.getRequestId());
		captureResult.setRequestToken(request.getRequestToken());
		captureResult.setMerchantTransactionCode(request.getMerchantTransactionCode());
		captureResult.setTotalAmount(BigDecimal.ZERO);
		return captureResult;
	}

	private CaptureResult createSettledResponse(final String transactionID)
	{
		getLoggingHandler().getLogger().info(String
				.format("[SETTLEMENT FOR TRANSACTION ID: %s WILL BE SETTLED IMMEDIATELY BY BRAINTREE] See transaction details.",
						transactionID));
		LOG.error("WILL BE SETTLED IMMEDIATELY BY BRAINTREE See transaction transactionID: " + transactionID);
		final CaptureResult result = new CaptureResult();

		result.setCurrency(request.getCurrency());
		result.setMerchantTransactionCode(request.getMerchantTransactionCode());
		result.setRequestId(request.getRequestId());
		result.setRequestToken(request.getRequestToken());
		result.setTransactionStatus(TransactionStatus.ACCEPTED);
		result.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
		result.setTotalAmount(request.getTotalAmount());

		return result;
	}

	private CaptureResult translateResponse(final Result<Transaction> braintreeReply)
	{
		List<ValidationError> errors = null;
		Transaction transaction = null;

		final CaptureResult result = new CaptureResult();
		result.setTransactionStatus(TransactionStatus.REJECTED);
		result.setTransactionStatusDetails(TransactionStatusDetails.BANK_DECLINE);
		if (braintreeReply != null)
		{
			transaction = braintreeReply.getTarget();

			if (braintreeReply.isSuccess())
			{
				if (transaction != null)
				{

					if (transaction.getAmount() != null)
					{
						result.setTotalAmount(transaction.getAmount());
					}

					result.setCurrency(Currency.getInstance(transaction.getCurrencyIsoCode()));
					result.setMerchantTransactionCode(transaction.getMerchantAccountId());
					result.setRequestId(transaction.getId());
					result.setRequestToken(transaction.getAuthorizedTransactionId());
					result.setTransactionStatus(TransactionStatus.ACCEPTED);
					result.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
				}
			}
			else if (braintreeReply.getErrors() != null)
			{

				if (braintreeReply.getErrors().getAllDeepValidationErrors() != null
						&& braintreeReply.getErrors().getAllDeepValidationErrors().size() > 0)
				{

					result.setTransactionStatusDetails(getCodeTranslator().translateReasonCode(
							braintreeReply.getErrors().getAllDeepValidationErrors().get(0).getCode().code));

					errors = braintreeReply.getErrors().getAllDeepValidationErrors();
					final String errorMessage = getLoggingHandler().handleErrors(errors);
				}
			}
			getLoggingHandler().handleResult("[CAPTURE TRANSACTION] ", transaction);
		}

		return result;
	}

	private BigDecimal getSubmitForSettlementAmountForOrder(AbstractOrderModel orderModel)
	{
		BigDecimal amount = null;
		List<PaymentTransactionModel> paymentTransactionModels = orderModel.getPaymentTransactions();
		for (PaymentTransactionModel transaction : paymentTransactionModels)
		{
			if (amount == null)
			{
				for (PaymentTransactionEntryModel transactionEntry : transaction.getEntries())
				{
					if (PaymentTransactionType.AUTHORIZATION.equals(transactionEntry.getType()))
					{
						amount = transactionEntry.getSubmittedForSettlementAmount();
						break;
					}
				}
			}
		}
		return amount;
	}

	private boolean isIntentSale(AbstractOrderModel order)
	{
		if (order != null && order.getPaymentInfo() != null && order.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
		{
			if (isNotPayPalPayment(order))
			{
				return false;
			}
			String intent = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPayPalIntent();
			return BraintreeConstants.PAYPAL_INTENT_SALE.equalsIgnoreCase(intent);
		}
		return false;
	}

	public boolean isIntentOrder(AbstractOrderModel order)
	{
		if (order != null && order.getPaymentInfo() != null && order.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
		{
			String intent = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPayPalIntent();
			if (BraintreeConstants.PAYPAL_INTENT_ORDER.equalsIgnoreCase(intent))
			{
				return true;
			}
		}
		return false;
	}

	private boolean isNotPayPalPayment(AbstractOrderModel order)
	{
		String paymentProvider = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider();
		return BraintreeConstants.BRAINTREE_PAYMENT.equals(paymentProvider) || BraintreeConstants.APPLE_PAY_PAYMENT
				.equals(paymentProvider);
	}

	private boolean isAvailableMultiCapture(AbstractOrderModel order)
	{
		String paymentProvider = ((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider();

		return (BraintreeConstants.BRAINTREE_PAYMENT.equals(paymentProvider) || isIntentOrder(order))
				&& !getBrainTreeConfigService().getSettlementConfigParameter()
				&& getBrainTreeConfigService().getMultiCaptureEnabled();
	}

	public OrderRetrievalService getOrderRetrievalService()
	{
		return orderRetrievalService;
	}

	public void setOrderRetrievalService(final OrderRetrievalService orderRetrievalService)
	{
		this.orderRetrievalService = orderRetrievalService;
	}

	private boolean isApplePay(AbstractOrderModel order)
	{
		return BraintreeConstants.APPLE_PAY_PAYMENT
				.equalsIgnoreCase(((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider());
	}

	private boolean isVenmoPayment(AbstractOrderModel order)
	{
		return BraintreeConstants.VENMO_CHECKOUT
				.equalsIgnoreCase(((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider());
	}

	private boolean isCreditCardPayment(AbstractOrderModel order)
	{
		return BraintreeConstants.BRAINTREE_CREDITCARD_PAYMENT
				.equalsIgnoreCase(((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider());
	}

	private boolean isGooglePayPayment(AbstractOrderModel order)
	{
		return BraintreeConstants.ANDROID_PAY_CARD
				.equalsIgnoreCase(((BrainTreePaymentInfoModel) order.getPaymentInfo()).getPaymentProvider());
	}


	private boolean isAvailableSubmitForSettlement(AbstractOrderModel order)
	{
		boolean isSubmitForSettlement = getBrainTreeConfigService().getSettlementConfigParameter();
		return (isApplePay(order) || isCreditCardPayment(order) || isGooglePayPayment(order) || isVenmoPayment(order)) && isSubmitForSettlement;
	}

}
