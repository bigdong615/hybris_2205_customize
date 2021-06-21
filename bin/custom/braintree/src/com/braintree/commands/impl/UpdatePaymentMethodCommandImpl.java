package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeUpdatePaymentMethodRequest;
import com.braintree.command.result.BrainTreeUpdatePaymentMethodResult;
import com.braintree.commands.BrainTreeUpdatePaymentMethodCommand;
import com.braintree.constants.BraintreeConstants;
import com.braintreegateway.CreditCardVerification;
import com.braintreegateway.PaymentMethod;
import com.braintreegateway.PaymentMethodRequest;
import com.braintreegateway.Result;
import com.braintreegateway.ValidationError;
import com.braintreegateway.exceptions.NotFoundException;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import org.apache.commons.lang.StringUtils;

import java.util.List;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class UpdatePaymentMethodCommandImpl extends AbstractCommand implements BrainTreeUpdatePaymentMethodCommand
{
	@Override
	public BrainTreeUpdatePaymentMethodResult perform(final BrainTreeUpdatePaymentMethodRequest request)
	{
		validateParameterNotNullStandardMessage("Update Payment Method Request", request);
		try
		{
			final PaymentMethodRequest paymentMethodRequest = translateRequest(request);
			final Result<? extends PaymentMethod> result = getBraintreeGateway().paymentMethod().update(request.getToken(),
					paymentMethodRequest);
			if (result.isSuccess())
			{
				return translateResponse(result.getTarget(), result.isSuccess());
			}
			else
			{
				return translateErrorResponse(request.getToken(), result);
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

	private PaymentMethodRequest translateRequest(final BrainTreeUpdatePaymentMethodRequest request)
	{
		final PaymentMethodRequest paymentMethodRequest = new PaymentMethodRequest();
		paymentMethodRequest.cardholderName(request.getCardholderName());
		paymentMethodRequest.expirationDate(request.getCardExpirationDate());
		if (StringUtils.isNotBlank(request.getCvv()))
		{
			paymentMethodRequest.cvv(request.getCvv());
		}
		if (request.isDefault())
		{
			paymentMethodRequest.options().makeDefault(Boolean.valueOf(request.isDefault())).done();
		}
		if (!request.getToken().equals(request.getNewToken()))
		{
			paymentMethodRequest.token(request.getNewToken());
		}
		if (StringUtils.isNotBlank(request.getBillingAddressId()))
		{
			paymentMethodRequest.billingAddressId(request.getBillingAddressId());
		}
		paymentMethodRequest.options().verifyCard(getBrainTreeConfigService().getVerifyCard());
		return paymentMethodRequest;
	}

	private BrainTreeUpdatePaymentMethodResult translateResponse(final PaymentMethod target, final boolean success)
	{
		final BrainTreeUpdatePaymentMethodResult updateCustomerResult = new BrainTreeUpdatePaymentMethodResult();
		if (target != null)
		{
			updateCustomerResult.setPaymentMethod(target);
			updateCustomerResult.setSuccess(success);
			if (success)
			{
				updateCustomerResult.setTransactionStatus(TransactionStatus.ACCEPTED);
				updateCustomerResult.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
			}
		}
		return updateCustomerResult;
	}

	private BrainTreeUpdatePaymentMethodResult translateErrorResponse(final String token,
			final Result<? extends PaymentMethod> result)
	{
		final BrainTreeUpdatePaymentMethodResult response = new BrainTreeUpdatePaymentMethodResult();
		response.setSuccess(result.isSuccess());
		if (result.getErrors() != null)
		{
			final List<ValidationError> allDeepValidationErrors = result.getErrors().getAllDeepValidationErrors();
			if (allDeepValidationErrors != null && allDeepValidationErrors.size() > 0)
			{
				final ValidationError validationError = allDeepValidationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT payment method token(%s) updated with error: %s %s", token, validationError.getCode(),
								validationError.getMessage()));

				if (validationError.getCode() != null)
				{
					response.setErrorCode(validationError.getCode().toString());
				}
				response.setErrorMessage(validationError.getMessage());
			}
			CreditCardVerification creditCardVerification = result.getCreditCardVerification();
			if (creditCardVerification != null)
			{
				response.setErrorCode(creditCardVerification.getProcessorResponseCode());
				response.setErrorMessage(getLocalizedErrorMessage(BraintreeConstants.GENERAL_VALIDATION_ERROR_MESSAGE));
				getLoggingHandler().handleCardVerificationError(creditCardVerification);
			}

			getLoggingHandler().handleErrors(result.getErrors().getAllDeepValidationErrors());
			getLoggingHandler().handleErrors(result.getErrors().getAllValidationErrors());
		}
		return response;
	}


	private BrainTreeUpdatePaymentMethodResult translateNotFoundResponse(final BrainTreeUpdatePaymentMethodRequest request,
			final NotFoundException notFoundException)
	{
		getLoggingHandler().getLogger()
				.info(String.format("Payment Method with token=%s not Found! Error %s", request.getToken(),
						notFoundException.getMessage()));
		final BrainTreeUpdatePaymentMethodResult brainTreeCustomerResult = new BrainTreeUpdatePaymentMethodResult();
		brainTreeCustomerResult.setSuccess(Boolean.FALSE.booleanValue());
		brainTreeCustomerResult.setErrorMessage(String.format("Customer with token=%s not Found!", request.getToken()));
		return brainTreeCustomerResult;
	}
}
