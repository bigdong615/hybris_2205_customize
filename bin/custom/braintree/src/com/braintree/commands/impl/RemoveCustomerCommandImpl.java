package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeCustomerRequest;
import com.braintree.command.result.BrainTreeCustomerResult;
import com.braintree.commands.BrainTreeRemoveCustomerCommand;
import com.braintreegateway.Customer;
import com.braintreegateway.Result;
import com.braintreegateway.ValidationError;
import com.braintreegateway.exceptions.NotFoundException;
import de.hybris.platform.payment.AdapterException;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class RemoveCustomerCommandImpl extends AbstractCommand implements BrainTreeRemoveCustomerCommand
{
	@Override
	public BrainTreeCustomerResult perform(final BrainTreeCustomerRequest request)
	{
		validateParameterNotNullStandardMessage("Remove customer request", request);
		try
		{
			final Result<Customer> deleteResponse = getBraintreeGateway().customer().delete(request.getCustomerId());
			if (deleteResponse.isSuccess())
			{
				return new BrainTreeCustomerResult(deleteResponse.isSuccess());
			}
			else
			{
				return translateErrorResponse(request.getCustomerId(), deleteResponse);
			}
		}
		catch (final NotFoundException notFoundException)
		{
			return translateNotFoundResponse(request, notFoundException);
		}
		catch (final Exception exception)
		{
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	private BrainTreeCustomerResult translateErrorResponse(final String customerId, final Result<Customer> deleteResponse)
	{
		final BrainTreeCustomerResult response = new BrainTreeCustomerResult(deleteResponse.isSuccess());
		if (deleteResponse.getErrors() != null)
		{

			final List<ValidationError> allDeepValidationErrors = deleteResponse.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT customer id(%s) can not be delete error: %s %s", customerId, validationError.getCode(),
								validationError.getMessage()));

				if (validationError.getCode() != null)
				{
					response.setErrorCode(validationError.getCode().toString());
				}
				response.setErrorMessage(validationError.getMessage());
			}
		}
		return response;
	}

	private BrainTreeCustomerResult translateNotFoundResponse(final BrainTreeCustomerRequest request,
			final NotFoundException notFoundException)
	{
		getLoggingHandler().getLogger().info(
				String.format("Customer with id=%s not Found! Error %s", request.getCustomerId(), notFoundException.getMessage()));

		final BrainTreeCustomerResult brainTreeCustomerResult = new BrainTreeCustomerResult(Boolean.FALSE.booleanValue());
		brainTreeCustomerResult.setErrorMessage(String.format("Customer with id=%s not Found!", request.getCustomerId()));
		return brainTreeCustomerResult;
	}
}
