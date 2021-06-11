/**
 *
 */
package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeDeletePaymentMethodRequest;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.commands.BrainTreeDeletePaymentMethodCommand;
import com.braintreegateway.PaymentMethod;
import com.braintreegateway.Result;
import com.braintreegateway.ValidationError;
import com.braintreegateway.exceptions.NotFoundException;
import de.hybris.platform.payment.AdapterException;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class DeletePaymentMethodCommandImpl extends
		AbstractCommand<BrainTreeDeletePaymentMethodRequest, BrainTreePaymentMethodResult> implements
		BrainTreeDeletePaymentMethodCommand
{

	@Override
	public BrainTreePaymentMethodResult perform(final BrainTreeDeletePaymentMethodRequest request)
	{
		validateParameterNotNullStandardMessage("Delete Payment Method request", request);
		try
		{
			final Result<? extends PaymentMethod> deleteResult = getBraintreeGateway().paymentMethod().delete(request.getToken());
			if (deleteResult.isSuccess())
			{
				return new BrainTreePaymentMethodResult(deleteResult.getTarget(), deleteResult.isSuccess());
			}
			else
			{
				return translateErrorResponse(request.getToken(), deleteResult);
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

	private BrainTreePaymentMethodResult translateErrorResponse(final String token, final Result<? extends PaymentMethod> result)
	{
		final BrainTreePaymentMethodResult response = new BrainTreePaymentMethodResult();
		response.setSuccess(result.isSuccess());
		if (result.getErrors() != null)
		{

			final List<ValidationError> allDeepValidationErrors = result.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT payment method token(%s) deleting with error: %s %s", token, validationError.getCode(),
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

	private BrainTreePaymentMethodResult translateNotFoundResponse(final BrainTreeDeletePaymentMethodRequest request,
			final NotFoundException notFoundException)
	{
		getLoggingHandler().getLogger()
				.info(String.format("Payment Method with token=%s not Found! Error %s", request.getToken(),
						notFoundException.getMessage()));

		final BrainTreePaymentMethodResult brainTreeCustomerResult = new BrainTreePaymentMethodResult();
		brainTreeCustomerResult.setSuccess(Boolean.FALSE.booleanValue());
		brainTreeCustomerResult.setErrorMessage(String.format("Payment Method with token id=%s not Found!", request.getToken()));
		return brainTreeCustomerResult;
	}

}
