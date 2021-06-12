/**
 *
 */
package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeAddressRequest;
import com.braintree.command.result.BrainTreeAddressResult;
import com.braintree.commands.BrainTreeRemoveAddressCommand;
import com.braintreegateway.Address;
import com.braintreegateway.Result;
import com.braintreegateway.ValidationError;

import java.util.List;


public class RemoveAddressCommandImpl extends AbstractCommand implements BrainTreeRemoveAddressCommand
{
	@Override
	public BrainTreeAddressResult perform(final BrainTreeAddressRequest addressRequest)
	{
		getLoggingHandler().handleAddressRequest("[REMOVE BRAINTREE ADDRESS REQUEST]", addressRequest);
		final Result<Address> result = getBraintreeGateway().address().delete(addressRequest.getCustomerId(),
				addressRequest.getAddressId());

		return translateResult(result);
	}

	private BrainTreeAddressResult translateResult(final Result<Address> result)
	{
		if (result.getErrors() != null)
		{
			getLoggingHandler().handleErrors(result.getErrors().getAllDeepValidationErrors());
			getLoggingHandler().handleErrors(result.getErrors().getAllValidationErrors());
			return translateErrorResponse(result);
		}
		final Address address = result.getTarget();
		getLoggingHandler().handleResult("[REMOVE BRAINTREE ADDRESS RESULT]", address);

		final BrainTreeAddressResult brainTreeAddressResult = new BrainTreeAddressResult(address);
		brainTreeAddressResult.setSuccess(result.isSuccess());
		return brainTreeAddressResult;
	}

	private BrainTreeAddressResult translateErrorResponse(final Result<Address> result)
	{
		final BrainTreeAddressResult response = new BrainTreeAddressResult(result.getTarget());
		response.setSuccess(result.isSuccess());
		if (result.getErrors() != null)
		{
			final List<ValidationError> allDeepValidationErrors = result.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				if (validationError.getCode() != null)
				{
					response.setErrorCode(validationError.getCode().toString());
				}
				response.setErrorMessage(validationError.getMessage());
			}
		}
		return response;
	}
}
