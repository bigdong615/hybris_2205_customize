package com.braintree.commands.impl;

import java.util.List;

import org.apache.commons.collections4.CollectionUtils;

import com.braintree.commands.BrainTreeCreatePaymentMethodNonceCommand;
import com.braintreegateway.PaymentMethodNonce;
import com.braintreegateway.Result;
import com.braintreegateway.ValidationError;
import com.braintreegateway.ValidationErrors;


public class CreatePaymentMethodNonceCommandImpl extends AbstractCommand implements BrainTreeCreatePaymentMethodNonceCommand
{
	@Override
	public String perform(final String request)
	{
		final Result<PaymentMethodNonce> brainTreeReply = getBraintreeGateway().paymentMethodNonce().create(request);

		validateErrors(request, brainTreeReply);

		return brainTreeReply.getTarget().getNonce();
	}

	private void validateErrors(String request, Result<PaymentMethodNonce> brainTreeReply)
	{
		final ValidationErrors errors = brainTreeReply.getErrors();
		if (errors != null)
		{
			final List<ValidationError> validationErrors = errors.getAllDeepValidationErrors();
			if (CollectionUtils.isNotEmpty(validationErrors))
			{
				final ValidationError validationError = validationErrors.get(0);
				getLoggingHandler().getLogger().info(
						String.format("BT payment method nonce for token(%s) is creating with error: %s %s", request,
								validationError.getCode(),
								validationError.getMessage()));
			}
		}
	}

}
