package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.commands.BrainTreeRefundCommand;
import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.braintreegateway.TransactionRefundRequest;
import com.braintreegateway.ValidationError;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import org.apache.log4j.Logger;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class RefundTransactionCommandImpl extends AbstractCommand implements BrainTreeRefundCommand
{
	private static final Logger LOG = Logger.getLogger(RefundTransactionCommandImpl.class);

	@Override
	public BrainTreeRefundTransactionResult perform(final BrainTreeRefundTransactionRequest request)
	{
		validateParameterNotNullStandardMessage("Refund transaction Request", request);
		try
		{
			final TransactionRefundRequest transactionRequest = new TransactionRefundRequest();
			transactionRequest.amount(request.getAmount());
			transactionRequest.orderId(request.getOrderId());

			LOG.info("transactionRequest.query: " + transactionRequest.toQueryString());
			LOG.info("transactionRequest.xml: " + transactionRequest.toXML());

			final Result<Transaction> result = getBraintreeGateway().transaction().refund(request.getTransactionId(),
					transactionRequest);
			LOG.info("Message: " + result.getMessage());
			LOG.info("Parameters: " + result.getParameters());
			if (result.isSuccess())
			{
				return translateResponse(result.getTarget(), result.isSuccess());
			}
			else
			{
				return translateErrorResponse(request.getTransactionId(), result);
			}
		}
		catch (final Exception exception)
		{
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	private BrainTreeRefundTransactionResult translateErrorResponse(final String transactionId, final Result<Transaction> result)
	{
		final BrainTreeRefundTransactionResult response = new BrainTreeRefundTransactionResult(result.isSuccess());
		if (result.getErrors() != null)
		{

			final List<ValidationError> allDeepValidationErrors = result.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT transaction id(%s) refund with error: %s %s", transactionId, validationError.getCode(),
								validationError.getMessage()));

				if (validationError.getCode() != null)
				{
					response.setErrorCode(validationError.getCode().toString());
				}
				response.setErrorMessage(validationError.getMessage());
			}
			else
			{
				response.setErrorMessage(result.getMessage());
			}
		}
		else
		{
			response.setErrorMessage(result.getMessage());
		}
		return response;
	}

	private BrainTreeRefundTransactionResult translateResponse(final Transaction target, final boolean success)
	{
		final BrainTreeRefundTransactionResult result = new BrainTreeRefundTransactionResult(success);
		if (target != null)
		{
			result.setAmount(target.getAmount());
			result.setCurrencyIsoCode(target.getCurrencyIsoCode());
			result.setTransactionId(target.getId());
			result.setTransaction(target);
			result.setOrderId(target.getOrderId());
			if (success)
			{
				result.setTransactionStatus(TransactionStatus.ACCEPTED);
				result.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
			}
		}
		return result;
	}
}
