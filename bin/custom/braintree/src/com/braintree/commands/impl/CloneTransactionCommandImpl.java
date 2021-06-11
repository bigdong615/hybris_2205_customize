package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeCloneTransactionRequest;
import com.braintree.command.result.BrainTreeCloneTransactionResult;
import com.braintree.commands.BrainTreeCloneCommand;
import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.braintreegateway.TransactionCloneRequest;
import com.braintreegateway.ValidationError;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class CloneTransactionCommandImpl extends AbstractCommand implements BrainTreeCloneCommand
{
	@Override
	public BrainTreeCloneTransactionResult perform(final BrainTreeCloneTransactionRequest request)
	{
		validateParameterNotNullStandardMessage("Clone transaction Request", request);
		try
		{
			final TransactionCloneRequest transactionCloneRequest = translateRequest(request);
			final Result<Transaction> result = getBraintreeGateway().transaction().cloneTransaction(request.getTransactionId(),
					transactionCloneRequest);
			if (result.isSuccess())
			{
				getLoggingHandler().getLogger().info("BT transaction id(" + request.getTransactionId() + ") clone successfully");
				return translateResponse(result);
			}
			else
			{
				return translateResponse(request.getTransactionId(), result);
			}
		}
		catch (final Exception exception)
		{
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	private BrainTreeCloneTransactionResult translateResponse(final Result<Transaction> result)
	{
		final BrainTreeCloneTransactionResult brainTreeCloneTransactionResult = new BrainTreeCloneTransactionResult(
				result.isSuccess());
		final Transaction transaction = result.getTarget();
		if (transaction != null)
		{
			brainTreeCloneTransactionResult.setTransactionId(transaction.getId());
			brainTreeCloneTransactionResult.setTransaction(transaction);
		}
		if (result.isSuccess())
		{
			brainTreeCloneTransactionResult.setTransactionStatus(TransactionStatus.ACCEPTED);
			brainTreeCloneTransactionResult.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
		}
		return brainTreeCloneTransactionResult;
	}

	private BrainTreeCloneTransactionResult translateResponse(final String transactionId, final Result<Transaction> result)
	{
		final BrainTreeCloneTransactionResult brainTreeCloneTransactionResult = new BrainTreeCloneTransactionResult(
				result.isSuccess());
		if (result.getErrors() != null)
		{
			final List<ValidationError> allDeepValidationErrors = result.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT transaction id(%s) clone with error: %s %s", transactionId, validationError.getCode(),
								validationError.getMessage()));

				if (validationError.getCode() != null)
				{
					brainTreeCloneTransactionResult.setErrorCode(validationError.getCode().toString());
				}
				brainTreeCloneTransactionResult.setErrorMessage(validationError.getMessage());
			}
		}
		return brainTreeCloneTransactionResult;
	}

	private TransactionCloneRequest translateRequest(final BrainTreeCloneTransactionRequest request)
	{
		final TransactionCloneRequest transactionCloneRequest = new TransactionCloneRequest();

		if (request.getAmount() != null)
		{
			transactionCloneRequest.amount(request.getAmount());
		}
		if (request.isSubmitForSettlement() != null)
		{
			transactionCloneRequest.options().submitForSettlement(request.isSubmitForSettlement()).done();
		}
		else
		{
			transactionCloneRequest.options().submitForSettlement(Boolean.FALSE).done();
		}
		return transactionCloneRequest;
	}
}
