/**
 *
 */
package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeCreateCreditCardPaymentMethodRequest;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.commands.BrainTreeCreateCreditCardPaymentMethodCommand;
import com.braintree.constants.BraintreeConstants;
import com.braintreegateway.CreditCard;
import com.braintreegateway.CreditCardVerification;
import com.braintreegateway.PayPalAccount;
import com.braintreegateway.PaymentMethod;
import com.braintreegateway.PaymentMethodRequest;
import com.braintreegateway.Result;
import com.braintreegateway.ValidationError;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.List;


public class CreateCreditCardPaymentMethodCommandImpl extends
		AbstractCommand<BrainTreeCreateCreditCardPaymentMethodRequest, BrainTreePaymentMethodResult> implements
		BrainTreeCreateCreditCardPaymentMethodCommand
{
	private final static Logger LOG = Logger.getLogger(CreateCreditCardPaymentMethodCommandImpl.class);

	@Override
	public BrainTreePaymentMethodResult perform(final BrainTreeCreateCreditCardPaymentMethodRequest request)
	{
		PaymentMethodRequest braintreeRequest = request.getRequest();
		braintreeRequest.options().verifyCard(getBrainTreeConfigService().getVerifyCard());

		LOG.info("braintreeRequest: " + braintreeRequest);
		LOG.info("braintreeRequest.xml: " + braintreeRequest.toXML()); // toQueryString

		final Result<? extends PaymentMethod> result = getBraintreeGateway().paymentMethod().create(braintreeRequest);

		LOG.info("result: " + result);
		LOG.info("result.message: " + result.getMessage());
		LOG.info("result.errors: " + result.getErrors());
		LOG.info("result.parameters: " + result.getParameters());

		if (result.isSuccess())
		{
			return translateResponse(result);
		}
		else
		{
			return translateErrorResponse(braintreeRequest, result);
		}
	}

	private BrainTreePaymentMethodResult translateResponse(Result<? extends PaymentMethod> btResult)
	{
		final BrainTreePaymentMethodResult result = new BrainTreePaymentMethodResult();
		if (btResult.getTarget() != null)
		{
			result.setPaymentMethod(btResult.getTarget());
			result.setSuccess(btResult.isSuccess());
			if (btResult.isSuccess())
			{
				result.setTransactionStatus(TransactionStatus.ACCEPTED);
				result.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL);
				if (btResult.getTarget() instanceof PayPalAccount)
				{
					PayPalAccount payPal = (PayPalAccount) btResult.getTarget();
					result.setPaymentMethodToken(payPal.getToken());
					result.setImageSource(payPal.getImageUrl());
					result.setEmail(payPal.getEmail());
					result.setCardType(StringUtils.EMPTY);
					result.setExpirationMonth(StringUtils.EMPTY);
					result.setExpirationYear(StringUtils.EMPTY);
					result.setCardNumber(StringUtils.EMPTY);
					result.setCardholderName(StringUtils.EMPTY);
					getLoggingHandler().handleResult("[PAYPAL ACCOUNT]", payPal);
				}
				else if (btResult.getTarget() instanceof CreditCard)
				{
					CreditCard card = (CreditCard) btResult.getTarget();
					result.setPaymentMethodToken(card.getToken());
					result.setCardType(card.getCardType());
					result.setExpirationMonth(card.getExpirationMonth());
					result.setExpirationYear(card.getExpirationYear());
					result.setCardNumber(card.getMaskedNumber());
					result.setImageSource(card.getImageUrl());
					result.setCardholderName(card.getCardholderName());
					getLoggingHandler().handleResult("[PAYMENT METHOD]", card);
				}
			}
		}
		return result;
	}

	private BrainTreePaymentMethodResult translateErrorResponse(final PaymentMethodRequest request,
			final Result<? extends PaymentMethod> result)
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
						String.format("Cannot create Payment method for user(%s) with error: %s %s", request.getCustomerId(),
								validationError.getCode(), validationError.getMessage()));

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

			getLoggingHandler().handleCardVerificationError(creditCardVerification);
			getLoggingHandler().handleErrors(result.getErrors().getAllDeepValidationErrors());
			getLoggingHandler().handleErrors(result.getErrors().getAllValidationErrors());
		}
		return response;
	}

}
