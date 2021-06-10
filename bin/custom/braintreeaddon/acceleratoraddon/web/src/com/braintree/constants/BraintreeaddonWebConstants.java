/*
 * Copyright (c) 2020 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.braintree.constants;

/**
 * Global class for all Braintreeaddon web constants. You can add global constants for your extension into this class.
 */
public final class BraintreeaddonWebConstants
{

	private BraintreeaddonWebConstants()
	{
		//empty to avoid instantiating this constant class
	}

	// implement here constants used by this extension


	public static final String REQUEST_MODEL_ATTRIBUTE_NAME = "request";

	public static final String ANONYMOUS_CHECKOUT = "anonymous_checkout";

	public static final String PAY_PAL_RESPONSE = "payPalData";

	public static final String PAY_PAL_CARD_DETAILS = "No Card details";

	public static final String PAY_PAL_STANDARD_ENABLE = "payPalStandardEnabled";

	public static final String APPLE_PAY_ENABLE = "applePayEnabled";

	public static final String VENMO_ENABLE = "venmoEnabled";

	public static final String LOCAL_PAYMENTS_ENABLED = "localPaymentsEnabled";

	public static final String HOSTED_FIELDS_ENABLE = "hostedFieldsEnable";

	public static final String PAY_PAL_EXPRESS_ENABLE = "payPalExpressEnabled";

	public static final String ERROR_MESSAGE_WITH_REASON = "braintree.error.default.with.reason";

	public static final String PAYMENT_INFOS = "braintreePaymentInfos";

	public static final String ACCEPTED_PAYMENTS_METHODS_IMAGES_URL = "paymentsImagesURL";

	public static final String CART_URL = "/cart";

	public static final String MULTI_CHECKOUT_SUMMARY_CMS_PAGE_LABEL = "multiStepCheckoutSummary";

	public static final String BRAINTREE_LOCALE = "braintreeLocale";

	public static final String BILLING_AGREEMENT_DESCRIPTION = "billingAgreementDescription";

	public static final String CURRENCY_MERCHANT_ACCOUNT = "currencyMerchantAccount";

	public static final String DISABLE_MARK_FUNDING = "disableMarkFunding";

	public static final String GOOGLE_PAY_MERCHANT_ID = "googleMerchantId";

	public static final String GOOGLE_PAY_ENABLE = "googlePayEnable";

	public static final String GOOGLE_PAY_COUNTRY_CODE = "googlePayCountryCode";

	public static final String IS_CREDIT_MESSAGE_ENABLED = "isCreditMessagesEnabled";

	public static final String CREDIT_MESSAGE_COMPONENT = "creditMessageComponent";

	public static final String CREDIT_ENABLED = "isCreditEnabled";

}
