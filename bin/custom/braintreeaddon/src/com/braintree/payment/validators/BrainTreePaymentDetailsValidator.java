/**
 *
 */
package com.braintree.payment.validators;

import com.google.common.collect.Lists;
import de.hybris.platform.acceleratorservices.payment.data.PaymentErrorField;
import de.hybris.platform.commercefacades.user.data.AddressData;
import org.apache.commons.lang.StringUtils;
import org.springframework.validation.BindingResult;

import java.util.List;


public class BrainTreePaymentDetailsValidator
{

	public List<PaymentErrorField> validatePaymentDetails(final AddressData addressData, final BindingResult bindingResult)
	{
		final List<PaymentErrorField> errorFields = Lists.newArrayList();

		if (StringUtils.isBlank(addressData.getCountry().getIsocode()))
		{
			errorFields.add(createError("billTo_country"));
		}

		if (StringUtils.isBlank(addressData.getFirstName()))
		{
			errorFields.add(createError("billTo_firstName"));
		}
		if (StringUtils.isBlank(addressData.getLastName()))
		{
			errorFields.add(createError("billTo_lastName"));
		}
		if (StringUtils.isBlank(addressData.getLine1()))
		{
			errorFields.add(createError("billTo_street1"));
		}

		if (StringUtils.isBlank(addressData.getTown()))
		{
			errorFields.add(createError("billTo_city"));
		}

		if (StringUtils.isBlank(addressData.getPostalCode()))
		{
			errorFields.add(createError("billTo_postalCode"));
		}

		fillPaymentErrors(bindingResult, errorFields);

		return errorFields;
	}

	private void fillPaymentErrors(final BindingResult bindingResult, final List<PaymentErrorField> errorFields)
	{
		// Add in specific errors for invalid fields
		for (final PaymentErrorField paymentErrorField : errorFields)
		{
			if (paymentErrorField.isMissing())
			{
				bindingResult.rejectValue(paymentErrorField.getName(),
						"checkout.error.paymentethod.formentry.sop.missing." + paymentErrorField.getName(),
						"Please enter a value for this field");
			}
			if (paymentErrorField.isInvalid())
			{
				bindingResult.rejectValue(paymentErrorField.getName(),
						"checkout.error.paymentethod.formentry.sop.invalid." + paymentErrorField.getName(),
						"This value is invalid for this field");
			}

		}
	}

	private PaymentErrorField createError(final String name)
	{
		final PaymentErrorField paymentErrorFieldCountry = new PaymentErrorField();
		paymentErrorFieldCountry.setMissing(true);
		paymentErrorFieldCountry.setName(name);
		return paymentErrorFieldCountry;
	}

}
