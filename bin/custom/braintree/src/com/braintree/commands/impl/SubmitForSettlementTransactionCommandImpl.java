package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeSubmitForSettlementTransactionRequest;
import com.braintree.command.result.BrainTreeSubmitForSettlementTransactionResult;
import com.braintree.commands.BrainTreeSubmitForSettlementCommand;
import com.braintree.order.service.OrderRetrievalService;
import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.braintreegateway.TransactionRequest;
import com.braintreegateway.ValidationError;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class SubmitForSettlementTransactionCommandImpl extends AbstractCommand implements BrainTreeSubmitForSettlementCommand
{
	private OrderRetrievalService orderRetrievalService;

	@Override
	public BrainTreeSubmitForSettlementTransactionResult perform(final BrainTreeSubmitForSettlementTransactionRequest request)
	{
		validateParameterNotNullStandardMessage("Submit For Settlement transaction Request", request);
		try
		{
			final Result<Transaction> result = submitForSettlement(request);

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

	private Result<Transaction> submitForSettlement(final BrainTreeSubmitForSettlementTransactionRequest request)
	{
		final TransactionRequest transactionRequest = new TransactionRequest();
		final String orderId = orderRetrievalService.getOrderCodeForTransaction(request.getTransactionId());
		if (orderId != null)
		{
			transactionRequest.orderId(orderId);
		}
		if (request.getAmount() != null)
		{
			transactionRequest.amount(request.getAmount());
		}
		return getBraintreeGateway().transaction().submitForSettlement(request.getTransactionId(), transactionRequest);
	}

	private BrainTreeSubmitForSettlementTransactionResult translateErrorResponse(final String transactionId,
			final Result<Transaction> result)
	{
		final BrainTreeSubmitForSettlementTransactionResult response = new BrainTreeSubmitForSettlementTransactionResult(
				result.isSuccess());

		if (result.getMessage() != null)
		{
			response.setErrorMessage(result.getMessage());
		}

		if (result.getErrors() != null)
		{

			final List<ValidationError> allDeepValidationErrors = result.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT transaction id(%s) submit for settlement with error: %s %s", transactionId,
								validationError.getCode(), validationError.getMessage()));

				if (validationError.getCode() != null)
				{
					response.setErrorCode(validationError.getCode().toString());
				}
				response.setErrorMessage(validationError.getMessage());
			}
			getLoggingHandler().handleResult("[SUBMIT FOR SETTLEMENT TRANSACTION] ", result.getTarget());
		}
		return response;
	}

	private BrainTreeSubmitForSettlementTransactionResult translateResponse(final Transaction target, final boolean success)
	{
		final BrainTreeSubmitForSettlementTransactionResult result = new BrainTreeSubmitForSettlementTransactionResult(success);
		if (target != null)
		{
			result.setTransactionId(target.getId());
			result.setTransaction(target);
			if (success)
			{
				result.setTransactionStatus(TransactionStatus.ACCEPTED);
				result.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
			}
			getLoggingHandler().handleResult("[SUBMIT FOR SETTLEMENT TRANSACTION] ", target);
		}
		return result;
	}

	public OrderRetrievalService getOrderRetrievalService()
	{
		return orderRetrievalService;
	}

	public void setOrderRetrievalService(final OrderRetrievalService orderRetrievalService)
	{
		this.orderRetrievalService = orderRetrievalService;
	}
}
