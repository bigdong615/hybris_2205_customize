package com.braintree.commands;

import com.braintreegateway.PayPalAccount;
import com.braintreegateway.PaymentMethod;
import de.hybris.platform.payment.commands.Command;


public interface GetPaymentMethodByTokenCommand extends Command<String, PayPalAccount>
{
}
