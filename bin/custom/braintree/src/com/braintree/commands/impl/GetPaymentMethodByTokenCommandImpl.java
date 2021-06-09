package com.braintree.commands.impl;

import com.braintree.commands.GetPaymentMethodByTokenCommand;
import com.braintreegateway.PayPalAccount;
import org.apache.log4j.Logger;


public class GetPaymentMethodByTokenCommandImpl extends AbstractCommand<String, PayPalAccount> implements
		GetPaymentMethodByTokenCommand
{
	private final static Logger LOG = Logger.getLogger(GetPaymentMethodByTokenCommandImpl.class);

	@Override public PayPalAccount perform(final String paymentMethodToken)
	{
		final PayPalAccount paymentMethod = (PayPalAccount) getBraintreeGateway().paymentMethod().find(paymentMethodToken);

		LOG.info("Got paymentMethod info " + paymentMethod);

		return paymentMethod;
	}
}
