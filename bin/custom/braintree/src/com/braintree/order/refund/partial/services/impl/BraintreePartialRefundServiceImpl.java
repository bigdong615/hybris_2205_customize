package com.braintree.order.refund.partial.services.impl;

import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.constants.BraintreeConstants;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.order.refund.partial.services.BraintreePartialRefundService;
import com.braintree.order.refund.partial.strategy.BraintreePartialRefundOrderRecalculationStrategy;
import com.braintree.transaction.service.BrainTreeTransactionService;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import org.apache.log4j.Logger;

import java.math.BigDecimal;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;


public class BraintreePartialRefundServiceImpl implements BraintreePartialRefundService
{

	private static final Logger LOG = Logger.getLogger(BraintreePartialRefundServiceImpl.class);
	private ModelService modelService;
	private BrainTreePaymentService brainTreePaymentService;
	private BrainTreeTransactionService brainTreeTransactionService;
	private UserService userService;
	private BraintreePartialRefundOrderRecalculationStrategy recalculationStrategy;

	@Override
	public BrainTreeRefundTransactionResult partialRefundTransaction(OrderModel order,
			PaymentTransactionEntryModel paymentTransactionEntry, BigDecimal amount) throws BraintreeErrorException
	{
		validateParameterNotNullStandardMessage("order", order);
		validateParameterNotNullStandardMessage("braintreeTransactionID", paymentTransactionEntry);
		validateParameterNotNullStandardMessage("amountString", amount);

		if (eligibleForPartialRefund(paymentTransactionEntry))
		{
			if (!isValidTransactionId(order, paymentTransactionEntry.getRequestId()))
			{
				throw new BraintreeErrorException(getLocalizedString(BraintreeConstants.TRANSACTION_ID_INVALID),
						paymentTransactionEntry.getRequestId());
			}
			return createPartialRefundTransaction(paymentTransactionEntry, amount, order.getCode());
		}
		else
		{
			throw new BraintreeErrorException(getLocalizedString(BraintreeConstants.TRANSACTION_NOT_REFUNDABLE),
					paymentTransactionEntry.getRequestId());
		}
	}

	public boolean eligibleForPartialRefund(PaymentTransactionEntryModel paymentEntry)
	{
		return (PaymentTransactionType.CAPTURE.equals(paymentEntry.getType()) || PaymentTransactionType.PARTIAL_CAPTURE
				.equals(paymentEntry.getType())) && TransactionStatus.ACCEPTED.name().equals(paymentEntry.getTransactionStatus())
				&& paymentEntry.getTransactionStatusDetails().startsWith(TransactionStatusDetails.SUCCESFULL.name());
	}

	protected BrainTreeRefundTransactionResult createPartialRefundTransaction(final PaymentTransactionEntryModel paymentEntry,
			final BigDecimal amount, final String orderId) throws BraintreeErrorException
	{
		final String merchantTransactionCode = getMerchantCode();
		if (merchantTransactionCode != null)
		{
			final BrainTreeRefundTransactionRequest request = new BrainTreeRefundTransactionRequest(merchantTransactionCode);
			request.setTransactionId(paymentEntry.getRequestId());

			request.setAmount(amount);
			//			request.setOrderId(orderId);
			final BrainTreeRefundTransactionResult result = getBrainTreePaymentService().refundTransaction(request);
			if (result.isSuccess())
			{
				updateTransactionRefundedAmount(paymentEntry, amount);
				getBrainTreeTransactionService().createPartialRefundTransaction(paymentEntry.getPaymentTransaction(), result);
			}
			else
			{
				throw new BraintreeErrorException(result.getErrorMessage(), result.getRequestId());
			}
			return result;
		}

		return new BrainTreeRefundTransactionResult(false);
	}

	protected void updateTransactionRefundedAmount(PaymentTransactionEntryModel paymentEntry, BigDecimal amount)
	{
		BigDecimal refundedAmount = paymentEntry.getRefundedAmount();
		if (refundedAmount == null)
		{
			paymentEntry.setRefundedAmount(amount);
		}
		else
		{
			paymentEntry.setRefundedAmount(refundedAmount.add(amount));
		}
		getModelService().save(paymentEntry);
	}

	@Override
	public boolean isValidTransactionId(final OrderModel orderModel, final String transactionId)
	{
		boolean result = false;

		for (final PaymentTransactionModel paymentTransactionModel : orderModel.getPaymentTransactions())
		{
			for (final PaymentTransactionEntryModel paymentTransactionEntryModel : paymentTransactionModel.getEntries())
			{
				if ((paymentTransactionEntryModel.getType() == PaymentTransactionType.CAPTURE ||
						paymentTransactionEntryModel.getType() == PaymentTransactionType.PARTIAL_CAPTURE)
						&& TransactionStatus.ACCEPTED.name().equals(paymentTransactionEntryModel.getTransactionStatus())
						&& paymentTransactionEntryModel.getRequestId().equals(transactionId))
				{
					result = true;
					break;
				}
			}
		}

		return result;
	}

	protected String getMerchantCode()
	{
		return getUserService().getCurrentUser().getUid();
	}

	public ModelService getModelService()
	{
		return modelService;
	}

	public void setModelService(ModelService modelService)
	{
		this.modelService = modelService;
	}

	public BrainTreePaymentService getBrainTreePaymentService()
	{
		return brainTreePaymentService;
	}

	public void setBrainTreePaymentService(BrainTreePaymentService brainTreePaymentService)
	{
		this.brainTreePaymentService = brainTreePaymentService;
	}

	public BrainTreeTransactionService getBrainTreeTransactionService()
	{
		return brainTreeTransactionService;
	}

	public void setBrainTreeTransactionService(BrainTreeTransactionService brainTreeTransactionService)
	{
		this.brainTreeTransactionService = brainTreeTransactionService;
	}

	public UserService getUserService()
	{
		return userService;
	}

	public void setUserService(UserService userService)
	{
		this.userService = userService;
	}

	public BraintreePartialRefundOrderRecalculationStrategy getRecalculationStrategy()
	{
		return recalculationStrategy;
	}

	public void setRecalculationStrategy(BraintreePartialRefundOrderRecalculationStrategy recalculationStrategy)
	{
		this.recalculationStrategy = recalculationStrategy;
	}
}
