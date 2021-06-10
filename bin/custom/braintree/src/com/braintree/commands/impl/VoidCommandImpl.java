package com.braintree.commands.impl;

import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.commands.BrainTreeVoidCommand;
import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.braintreegateway.ValidationError;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class VoidCommandImpl extends AbstractCommand implements BrainTreeVoidCommand
{
	@Override
	public BrainTreeVoidResult perform(final VoidRequest voidRequest)
	{
		validateParameterNotNullStandardMessage("Void transaction Request", voidRequest);
		try
		{
			final Result<Transaction> result = getBraintreeGateway().transaction().voidTransaction(voidRequest.getRequestId());
			if (result.isSuccess())
			{
				return translateResponse(result.getTarget(), voidRequest, result.isSuccess());
			}
			else
			{
				return translateErrorResponse(voidRequest.getRequestId(), result);
			}
		}
		catch (final Exception exception)
		{
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	private BrainTreeVoidResult translateErrorResponse(final String transactionId, final Result<Transaction> result)
	{
		final BrainTreeVoidResult voidResult = new BrainTreeVoidResult(result.isSuccess());
		if (result.getErrors() != null)
		{

			final List<ValidationError> allDeepValidationErrors = result.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT transaction id(%s) void with error: %s %s", transactionId, validationError.getCode(),
								validationError.getMessage()));

				if (validationError.getCode() != null)
				{
					voidResult.setErrorCode(validationError.getCode().toString());
				}
				voidResult.setErrorMessage(validationError.getMessage());
				voidResult.setTransactionStatus(TransactionStatus.ERROR);
				voidResult.setRequestId(transactionId);
			}
		}
		return voidResult;
	}

	private BrainTreeVoidResult translateResponse(final Transaction transaction, final VoidRequest request, final boolean success)
	{
		final BrainTreeVoidResult result = new BrainTreeVoidResult(success);
		result.setMerchantTransactionCode(request.getMerchantTransactionCode());
		result.setRequestId(request.getRequestId());
		result.setRequestToken(request.getRequestToken());
		result.setTransactionStatus(TransactionStatus.ACCEPTED);
		result.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
		result.setAmount(transaction.getAmount());
		result.setCurrencyIsoCode(transaction.getCurrencyIsoCode());
		result.setMerchantTransactionCode(request.getMerchantTransactionCode());
		result.setTransactionId(transaction.getId());
		getLoggingHandler().handleResult("[CLONE TRANSACTION] ", transaction);
		return result;
	}
}
