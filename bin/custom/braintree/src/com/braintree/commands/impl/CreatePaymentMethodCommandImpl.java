package com.braintree.commands.impl;

import com.braintree.command.request.BrainTreeCreatePaymentMethodRequest;
import com.braintree.command.result.BrainTreeCreatePaymentMethodResult;
import com.braintree.commands.BrainTreeCreatePaymentMethodCommand;
import com.braintree.constants.BraintreeConstants;
import com.braintreegateway.AndroidPayCard;
import com.braintreegateway.CreditCard;
import com.braintreegateway.CreditCardVerification;
import com.braintreegateway.PayPalAccount;
import com.braintreegateway.PaymentMethodRequest;
import com.braintreegateway.Result;
import com.braintreegateway.VenmoAccount;
import de.hybris.platform.payment.dto.BillingInfo;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.math.BigDecimal;


public class CreatePaymentMethodCommandImpl extends
		AbstractCommand<BrainTreeCreatePaymentMethodRequest, BrainTreeCreatePaymentMethodResult> implements
		BrainTreeCreatePaymentMethodCommand
{
	private final static Logger LOG = Logger.getLogger(CreatePaymentMethodCommandImpl.class);

	@Override
	public BrainTreeCreatePaymentMethodResult perform(final BrainTreeCreatePaymentMethodRequest request)
	{
		final PaymentMethodRequest translateRequest = translateRequest(request);

		final Result brainTreeResult = getBraintreeGateway().paymentMethod().create(translateRequest);

		final BrainTreeCreatePaymentMethodResult result = translateResult(brainTreeResult);

		return result;
	}

	private BrainTreeCreatePaymentMethodResult translateResult(final Result brainTreeResult)
	{
		PayPalAccount payPal = null;
		CreditCard card = null;
		VenmoAccount venmo = null;
		AndroidPayCard googlePay = null;
		final BrainTreeCreatePaymentMethodResult result = new BrainTreeCreatePaymentMethodResult();
		result.setSuccess(brainTreeResult.isSuccess());

		if (brainTreeResult.getErrors() != null)
		{
			result.setErrorMessage(brainTreeResult.getMessage());

			CreditCardVerification creditCardVerification = brainTreeResult.getCreditCardVerification();
			if (creditCardVerification != null)
			{
				result.setErrorCode(creditCardVerification.getProcessorResponseCode());
				result.setErrorMessage(getLocalizedErrorMessage(BraintreeConstants.GENERAL_VALIDATION_ERROR_MESSAGE));
				result.setCvvValidationCode(creditCardVerification.getCvvResponseCode());
				getLoggingHandler().handleCardVerificationError(creditCardVerification);
			}
			getLoggingHandler().handleErrors(brainTreeResult.getErrors().getAllDeepValidationErrors());
			getLoggingHandler().handleErrors(brainTreeResult.getErrors().getAllValidationErrors());
		}

		if (brainTreeResult.getTarget() instanceof PayPalAccount)
		{
			payPal = (PayPalAccount) brainTreeResult.getTarget();
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
		else if (brainTreeResult.getTarget() instanceof CreditCard)
		{
			card = (CreditCard) brainTreeResult.getTarget();
			result.setPaymentMethodToken(card.getToken());
			result.setCardType(card.getCardType());
			result.setExpirationMonth(card.getExpirationMonth());
			result.setExpirationYear(card.getExpirationYear());
			result.setCardNumber(card.getMaskedNumber());
			result.setCardholderName(card.getCardholderName());
			result.setImageSource(card.getImageUrl());
            result.setBraintreeAddressId(card.getBillingAddress().getId());
            result.setIsDefault(card.isDefault());
			getLoggingHandler().handleResult("[PAYMENT METHOD]", card);
		}
		else if (brainTreeResult.getTarget() instanceof VenmoAccount)
		{
			venmo = (VenmoAccount) brainTreeResult.getTarget();
			result.setPaymentMethodToken(venmo.getToken());
			result.setImageSource(venmo.getImageUrl());
			result.setCardType(StringUtils.EMPTY);
			result.setExpirationMonth(StringUtils.EMPTY);
			result.setExpirationYear(StringUtils.EMPTY);
			result.setCardNumber(StringUtils.EMPTY);
			result.setCardholderName(StringUtils.EMPTY);
			getLoggingHandler().handleResult("[VENMO PAYMENT METHOD]", venmo);
		}

		else if (brainTreeResult.getTarget() instanceof AndroidPayCard)
		{
			googlePay = (AndroidPayCard) brainTreeResult.getTarget();
			result.setPaymentMethodToken(googlePay.getToken());
			result.setImageSource(googlePay.getImageUrl());
			result.setCardType(googlePay.getSourceCardType());
			result.setExpirationMonth(googlePay.getExpirationMonth());
			result.setExpirationYear(googlePay.getExpirationYear());
			result.setCardNumber(String.format(BraintreeConstants.CARD_NUMBER_MASK, googlePay.getSourceCardLast4()));
			getLoggingHandler().handleResult("[GOOGLE PAY PAYMENT METHOD]", googlePay);
		}

		return result;
	}

	private PaymentMethodRequest translateRequest(final BrainTreeCreatePaymentMethodRequest request)
	{
		getLoggingHandler().handleCreatePaymentMethodRequest(request);
		final PaymentMethodRequest braintreeRequest = new PaymentMethodRequest().customerId(request.getCustomerId())
				.paymentMethodNonce(request.getMethodNonce()).cardholderName(request.getCardholderName());

		if (StringUtils.isNotEmpty(request.getBillingAddressId()))
		{
			braintreeRequest.billingAddressId(request.getBillingAddressId());
		}
		else
		{
			final BillingInfo billingInfo = request.getBillingInfo();
			braintreeRequest.billingAddress().countryCodeAlpha2(billingInfo.getCountry()).region(billingInfo.getState())
					.firstName(billingInfo.getFirstName()).lastName(billingInfo.getLastName()).streetAddress(billingInfo.getStreet1())
					.extendedAddress(billingInfo.getStreet2()).locality(billingInfo.getCity()).postalCode(billingInfo.getPostalCode());
		}

		braintreeRequest.options().verifyCard(getBrainTreeConfigService().getVerifyCard());
		if (request.getAmount() != null)
		{
			braintreeRequest.options().paypal().amount(new BigDecimal(request.getAmount()));
		}
		return braintreeRequest;
	}
}
