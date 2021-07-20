package com.braintree.payment.validators;

import com.braintree.exceptions.ResourceErrorMessage;


public interface PaymentMethodValidator
{
	/**
	 * validate fields: cardholder,expirationDate and cvv
	 * 
	 * @return validation message
	 */
    ResourceErrorMessage validate(String expirationDate, String cvv);
}
