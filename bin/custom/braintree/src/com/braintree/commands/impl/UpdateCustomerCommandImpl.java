/**
 *
 */
package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeUpdateCustomerRequest;
import com.braintree.command.result.BrainTreeUpdateCustomerResult;
import com.braintree.commands.BrainTreeUpdateCustomerCommand;
import com.braintreegateway.Customer;
import com.braintreegateway.CustomerRequest;
import com.braintreegateway.Result;
import com.braintreegateway.ValidationError;
import com.braintreegateway.exceptions.NotFoundException;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import org.apache.commons.lang.StringUtils;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class UpdateCustomerCommandImpl extends AbstractCommand implements BrainTreeUpdateCustomerCommand
{

	@Override
	public BrainTreeUpdateCustomerResult perform(final BrainTreeUpdateCustomerRequest paramRequest)
	{
		validateParameterNotNullStandardMessage("Update Customer Request", paramRequest);
		try
		{
			final CustomerRequest request = translateRequest(paramRequest);
			final Result<Customer> result = getBraintreeGateway().customer().update(paramRequest.getId(), request);
			if (result.isSuccess())
			{
				return translateResponse(result.getTarget(), result.isSuccess());
			}
			else
			{
				return translateErrorResponse(paramRequest.getId(), result);
			}
		}
		catch (final NotFoundException notFoundException)
		{
			return translateNotFoundResponse(paramRequest, notFoundException);
		}
		catch (final Exception exception)
		{
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	private BrainTreeUpdateCustomerResult translateResponse(final Customer target, final boolean success)
	{
		final BrainTreeUpdateCustomerResult updateCustomerResult = new BrainTreeUpdateCustomerResult();
		if (target != null)
		{
			updateCustomerResult.setCustomer(target);
			updateCustomerResult.setSuccess(success);
			if (success)
			{
				updateCustomerResult.setTransactionStatus(TransactionStatus.ACCEPTED);
				updateCustomerResult.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
			}
		}
		return updateCustomerResult;
	}

	private BrainTreeUpdateCustomerResult translateErrorResponse(final String customerId, final Result<Customer> result)
	{
		final BrainTreeUpdateCustomerResult response = new BrainTreeUpdateCustomerResult();
		response.setSuccess(result.isSuccess());
		if (result.getErrors() != null)
		{

			final List<ValidationError> allDeepValidationErrors = result.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT customer id(%s) updated with error: %s %s", customerId, validationError.getCode(),
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

	private CustomerRequest translateRequest(final BrainTreeUpdateCustomerRequest paramRequest)
	{
		final CustomerRequest customerRequest = new CustomerRequest();
		customerRequest.company(StringUtils.trim(paramRequest.getCompany()));
		customerRequest.email(StringUtils.trim(paramRequest.getEmail()));
		customerRequest.fax(StringUtils.trim(paramRequest.getFax()));
		customerRequest.firstName(StringUtils.trim(paramRequest.getFirstName()));
		customerRequest.lastName(StringUtils.trim(paramRequest.getLastName()));
		customerRequest.phone(StringUtils.trim(paramRequest.getPhone()));
		customerRequest.website(StringUtils.trim(paramRequest.getWebsite()));
		return customerRequest;
	}

	private BrainTreeUpdateCustomerResult translateNotFoundResponse(final BrainTreeUpdateCustomerRequest request,
			final NotFoundException notFoundException)
	{
		getLoggingHandler().getLogger().info(
				String.format("Customer with id=%s not Found! Error %s", request.getId(), notFoundException.getMessage()));

		final BrainTreeUpdateCustomerResult brainTreeCustomerResult = new BrainTreeUpdateCustomerResult();
		brainTreeCustomerResult.setSuccess(Boolean.FALSE.booleanValue());
		brainTreeCustomerResult.setErrorMessage(String.format("Customer with id=%s not Found!", request.getId()));
		return brainTreeCustomerResult;
	}
}
