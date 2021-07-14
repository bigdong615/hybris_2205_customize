/**
 *
 */
package com.braintree.commands.impl;

import de.hybris.platform.payment.dto.TransactionStatusDetails;


public class BraintreeCodeTranslator
{

	private static final Object[][] REASON_CODE_MAPPING =
			{
					{ "100", TransactionStatusDetails.SUCCESFULL },
					{ "101", TransactionStatusDetails.INVALID_REQUEST },
					{ "150", TransactionStatusDetails.GENERAL_SYSTEM_ERROR },
					{ "151", TransactionStatusDetails.TIMEOUT },
					{ "152", TransactionStatusDetails.TIMEOUT },
					{ "200", TransactionStatusDetails.AUTHORIZATION_REJECTED_BY_PSP },
					{ "201", TransactionStatusDetails.REVIEW_NEEDED },
					{ "81710", TransactionStatusDetails.INVALID_CARD_EXPIRATION_DATE },
					{ "203", TransactionStatusDetails.BANK_DECLINE },
					{ "204", TransactionStatusDetails.INSUFFICIENT_FUNDS },
					{ "205", TransactionStatusDetails.STOLEN_OR_LOST_CARD },
					{ "207", TransactionStatusDetails.COMMUNICATION_PROBLEM },
					{ "91538", TransactionStatusDetails.INACTIVE_OR_INVALID_CARD },
					{ "209", TransactionStatusDetails.INVALID_CVN },
					{ "210", TransactionStatusDetails.CREDIT_LIMIT_REACHED },
					{ "211", TransactionStatusDetails.INVALID_CVN },
					{ "91514", TransactionStatusDetails.CUSTOMER_BLACKLISTED },
					{ "230", TransactionStatusDetails.AUTHORIZATION_REJECTED_BY_PSP },
					{ "231", TransactionStatusDetails.INVALID_ACCOUNT_NUMBER },
					{ "81509", TransactionStatusDetails.INVALID_CARD_TYPE },
					{ "91517", TransactionStatusDetails.INVALID_CARD_TYPE },
					{ "233", TransactionStatusDetails.PROCESSOR_DECLINE },
					{ "234", TransactionStatusDetails.PSP_CONFIGURATION_PROBLEM },
					{ "91522", TransactionStatusDetails.AUTHORIZED_AMOUNT_EXCEEDED },
					{ "236", TransactionStatusDetails.COMMUNICATION_PROBLEM },
					{ "237", TransactionStatusDetails.AUTHORIZATION_ALREADY_REVERSED },
					{ "238", TransactionStatusDetails.AUTHORIZATION_ALREADY_SETTLED },
					{ "111", TransactionStatusDetails.AMOUNTS_MUST_MATCH },
					{ "91508", TransactionStatusDetails.INCORRECT_CARD_NUMBER_OR_TYPE },
					{ "91528", TransactionStatusDetails.INVALID_REQUEST_ID },
					{ "91507", TransactionStatusDetails.NO_AUTHORIZATION_FOR_SETTLEMENT },
					{ "91512", TransactionStatusDetails.TRANSACTION_ALREADY_SETTLED_OR_REVERSED },
					{ "91504", TransactionStatusDetails.NOT_VOIDABLE },
					{ "91505", TransactionStatusDetails.CREDIT_FOR_VOIDED_CAPTURE },
					{ "91506", TransactionStatusDetails.CREDIT_FOR_VOIDED_CAPTURE },
					{ "250", TransactionStatusDetails.TIMEOUT },
					{ "475", TransactionStatusDetails.THREE_D_SECURE_AUTHENTICATION_REQUIRED } };

	public TransactionStatusDetails translateReasonCode(final String reasonCode)
	{
		if (reasonCode == null)
		{
			return null;
		}

		for (final Object[] entry : REASON_CODE_MAPPING)
		{
			final String code = String.valueOf(entry[0]);
			if (code.equals(reasonCode))
			{
				return ((TransactionStatusDetails) entry[1]);
			}

		}
		return TransactionStatusDetails.UNKNOWN_CODE;
	}

}
