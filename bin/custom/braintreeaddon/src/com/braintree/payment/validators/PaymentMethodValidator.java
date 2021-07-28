package com.braintree.payment.validators;

import com.braintree.exceptions.ResourceErrorMessage;


public interface PaymentMethodValidator
{
	/**
	 * validate fields: cardholder,expirationDate and cvv
	 * 
	 * @return validation message
	 */
    ResourceErrorMessage validate(final String expirationDate, final String cvv);
}
