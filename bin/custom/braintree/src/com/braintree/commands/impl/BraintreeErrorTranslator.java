/**
 *
 */
package com.braintree.commands.impl;

import com.braintreegateway.ValidationError;
import com.braintreegateway.ValidationErrorCode;
import de.hybris.platform.util.localization.Localization;
import org.apache.commons.lang.WordUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.EnumMap;


public class BraintreeErrorTranslator
{
	public static final String DEFAULT_MESSAGE_KEY = "braintree.error.default";
	private static final Logger LOG = LoggerFactory.getLogger(BraintreeErrorTranslator.class);
	private static final EnumMap<ValidationErrorCode, String> ERROR_MAP = new EnumMap(ValidationErrorCode.class)
	{
		{
			put(ValidationErrorCode.ADDRESS_POSTAL_CODE_INVALID_CHARACTERS, DEFAULT_MESSAGE_KEY);
			put(ValidationErrorCode.ADDRESS_POSTAL_CODE_IS_REQUIRED, DEFAULT_MESSAGE_KEY);
			put(ValidationErrorCode.ADDRESS_POSTAL_CODE_IS_TOO_LONG, DEFAULT_MESSAGE_KEY);
			put(ValidationErrorCode.CREDIT_CARD_CREDIT_CARD_TYPE_IS_NOT_ACCEPTED, DEFAULT_MESSAGE_KEY);
			put(ValidationErrorCode.CREDIT_CARD_CVV_IS_INVALID, "braintree.error.invalid.cvv");
			put(ValidationErrorCode.CREDIT_CARD_CVV_IS_REQUIRED, "braintree.error.invalid.cvv");
			put(ValidationErrorCode.CREDIT_CARD_NUMBER_HAS_INVALID_LENGTH, "braintree.error.invalid.ccn");
			put(ValidationErrorCode.CREDIT_CARD_NUMBER_IS_INVALID, "braintree.error.invalid.ccn");
			put(ValidationErrorCode.CREDIT_CARD_NUMBER_MUST_BE_TEST_NUMBER, "braintree.error.invalid.ccn");
			put(ValidationErrorCode.CUSTOMER_EMAIL_IS_INVALID, DEFAULT_MESSAGE_KEY);
		}
	};

	public String getMessage(final ValidationError error)
	{
		try
		{
			if (ERROR_MAP.containsKey(error.getCode()))
			{
				return Localization.getLocalizedString(ERROR_MAP.get(error.getCode()), new Object[]
						{ obfuscateErrorCode(error) });
			}
			else
			{
				LOG.warn("No error mapping found [validationError.code={}, validationError.message={}]", error.getCode(),
						error.getMessage());

				return obfuscateErrorCodeNoMapping(error);
			}
		}
		catch (final Exception e)
		{
			LOG.error("Failed to create message for Braintree validation error: [code={}, message={}]", new Object[]
					{ error.getCode(), error.getMessage(), e });
			return "Please enter a valid credit card number.";
		}
	}

	private String obfuscateErrorCodeNoMapping(final ValidationError error)
	{
		final StringBuilder result = new StringBuilder();
		result.append(error.getMessage());
		result.append("(");
		result.append(error.getCode());
		result.append(")");
		return result.toString();
	}

	private String obfuscateErrorCode(final ValidationError error)
	{
		//@formatter:off
		final String transformedError = WordUtils.capitalizeFully(error.getCode().toString(), new char[]
		{ '_' }).replaceAll("_", "");
		return String.format("010-%s", transformedError);
		//@formatter:on
	}
}
