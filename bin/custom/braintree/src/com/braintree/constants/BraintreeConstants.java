/*
 * [y] hybris Platform
 *
 * Copyright (c) 2000-2013 hybris AG
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of hybris
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with hybris.
 *
 *
 */
package com.braintree.constants;

/**
 * Global class for all Braintree constants. You can add global constants for your extension into this class.
 */
public final class BraintreeConstants extends GeneratedBraintreeConstants
{
	public static final String EXTENSIONNAME = "braintree";

	public static final String TRANSACTION_SEARCH_DATE_FORMAT = "MM/dd/yy h:mm a";
	public static final String PAYPAL_PAYMENT_INSTRUMENT_TYPE = "paypal_account";
	public static final String CREDIT_CARD_PAYMENT_INSTRUMENT_TYPE = "credit_card";
	public static final String PAYPAL_PAYMENT_TYPE_NAME = "PayPal Account";
	public static final String CREDIT_CARD_PAYMENT_TYPE = "Credit Card";
	public static final String BRAINTREE_PRIVATE_KEY = "braintree.private_key";
	public static final String BRAINTREE_PUBLIC_KEY = "braintree.public_key";
	public static final String BRAINTREE_MERCHANT_ID = "braintree.merchant_id";
	public static final String SINGLE_USE_PARAMETER = "braintree.single.use";
	public static final String BRAINTREE_SUBMIT_FOR_SETTLEMENT = "braintree.submit.for.settlement";
	public static final String PAYPAL_DEFAULT_ADDRESS = "defaultAddressGroup";
	public static final String LOG_ALL_ENABLE = "braintree.log.all.enable";
	public static final String LOG_PACKAGE_PATH = "com.braintree";
	public static final String BRAINTREE_PROVIDER_NAME = "BrainTree";
	public static final String CARD_NUMBER_MASK = "**************%s";
	public static final String BRAINTREE_ADVANCED_FRAUD_TOOLS_ENABLED = "braintree.advanced.fraud.tools.enabled";
	public static final String BRAINTREE_ENVIRONMENT = "braintree.environment.type";
	public static final String ENVIRONMENT_SANDBOX = "sandbox";
	public static final String ENVIRONMENT_PRODUCTION = "production";
	public static final String BRAINTREE_3D_SECURE = "braintree.3d.secure";
	public static final String BRAINTREE_PAYMENT = "CreditCard";
	public static final String BRAINTREE_CREDITCARD_PAYMENT = "CreditCard";
	public static final String PAYPAL_PAYMENT = "PayPalAccount";
	public static final String PAY_PAL_EXPRESS_CHECKOUT = "BrainTreePayPalExpress";
	public static final String VENMO_CHECKOUT = "VenmoAccount";
	public static final String APPLE_PAY_CARD = "ApplePayCard";
	public static final String ANDROID_PAY_CARD = "AndroidPayCard";
	public static final String HOSTED_FIELDS_ENABLED = "braintree.hosted.fields.enabled";
	public static final String PAYPAL_EXPRESS_ENABLED = "braintree.express.paypal.enabled";
	public static final String PAYPAL_STANDARD_ENABLED = "braintree.standard.paypal.enabled";
	public static final String IS_SKIP_3D_SECURE_LIABILITY_RESULT = "braintree.skip.3dsecure.liability.result";
	public static final String BRAINTREE_CREDIT_CARD_STATEMENT_NAME = "braintree.credit.card.statement.name";
	public static final String BRAINTREE_CHANNEL_NAME = "braintree.channel";
	public static final String BRAINTREE_KEY = "braintree.channel.source.key";
	public static final String BRAINTREE_ACCEPTED_PAYMENT_METHODS = "braintree.accepted.payment.methods";
	public static final String STORE_IN_VAULT = "braintree.store.in.vault";
	public static final String BRAINTREE_ACCEPTED_PAYMENT_METHODS_DELIMETER = ";";
	public static final String BRAINTREE_IMAGES_PREFIX = "braintree.image.";
	public static final String BRAINTREE_REPLENISHMENT = "REPLENISHMENT";
	public static final String BRAINTREE_MERCHANT_ACCOUNT_PREFIX = "braintree.merchant.account.";
	public static final String CONFIGURATION_PROPERTY_DELIMETER = ".";
	public static final String IS_PAYPAL_CHECKOUT = "isPayPalCheckout";
	public static final String IS_BRAINTREE_CHECKOUT = "isBrainTreeCheckout";
	public static final String VERIFY_CARD = "braintree.verify.card";
	public static final String VERIFY_CARD_ON_VAULTING = "braintree.verify.card.on.vaulting";
	public static final String MULTICAPTURE_ENABLED = "braintree.multicapture";
	public static final String GENERAL_VALIDATION_ERROR_MESSAGE = "braintree.verify.card.general.error.msg";
	public static final String ORDER_PROCESS_NAME = "order-process";
	public static final String ORDER_PROCESS_RESTART_NODE = "braintree.order.process.restart.node";
	public static final String DEFAULT_ORDER_PROCESS_RESTART_NODE = "splitOrder";
	public static final String BRAINTREE_AUTHENTICATION_TOKEN = "braintree.authentication.token";
	public static final String BRAINTREE_ECVZ_ACEESS_TOKEN = "braintree.ecvz.access.token";
	public static final String BRAINTRE_CUSTOM_FIELD_GENERAL_KEY = "braintree.custom.field";
	public static final String BRAINTREE_USER_ACTION = "braintree.user.action";
	public static final String BRAINTREE_PAYPAL_INTENT = "braintree.paypal.intent";
	public static final String BRAINTREE_LOCALE = "braintree.locale";
	public static final String PAYPAL_INTENT_SALE = "sale";
	public static final String PAYPAL_INTENT_AUTHORIZE = "authorize";
	public static final String PAYPAL_INTENT_ORDER = "order";
	public static final String PAYPAL_INTENT_CAPTURE = "capture";
	public static final String HYBRIS_BUILD_API_VERSION = "build.version.api";
	public static final String B2C_FLOW = "braintree.b2c";
	public static final String BRAINTREE_APPLE_PAY_ENABLE = "braintree.applePay.enable";
	public static final String BRAINTREE_VENMO_ENABLE = "braintree.venmo.enable";
	public static final String BRAINTREE_LOCAL_PAYMENTS_ENABLE = "braintree.local.payments.enable";
	public static final String APPLE_PAY_PAYMENT = "ApplePayCard";
	public static final String BRAINTREE_CREDIT_ENABLE = "braintree.credit.enable";
	public static final String BRAINTREE_VENMO_PROFILE_ID = "braintree.venmo.profile.id";
	public static final String BILLING_AGREEMENT_DESCRIPTION_KEY = "braintree.billing.agreement.text";
	public static final String TRANSACTION_ID_INVALID = "message.braintree.order.refund.partial.transaction.id.invalid";
	public static final String TRANSACTION_NOT_REFUNDABLE = "message.braintree.order.refund.partial.transaction.not.refundable";
	public static final String ALL_TRANSACTIONS_ALREADY_REFUNDED = "message.braintree.order.refund.partial.transactions.already.refunded";
	public static final String EXCEEDED_CAPTURE_AMOUNT = "message.braintree.order.capture.exceeded.capture.amount";
	public static final String FAKE_REQUEST_ID = "FAKE";
	public static final String GUEST_USER_TYPE = "GUEST";
	public static final String PROPERTY_LEVEL2_LEVEL3 = "braintree.enable.level2.level3.data";
	public static final String CURRENCY_MERCHANT_ACCOUNT_ID = "braintree.currency.merchant.account.id";
	public static final String LOCAL_PAYMENT = "LocalPayment";
	public static final String B2C_ENDPOINT_URL_PROPERTY = "website.electronics.https";
	public static final String B2B_ENDPOINT_URL_PROPERTY = "website.powertools.http";
	public static final String PAYPAL_DISABLE_FUNDING_MARK_PAGE = "braintree.disable.funding.mark.page";
	public static final String BRAINTREE_GOOGLE_PAY_MERCHANT_ID = "braintree.googlePay.merchant_id";
	public static final String BRAINTREE_GOOGLE_PAY_ENABLE = "braintree.googlePay.enable";
	public static final String BRAINTREE_GOOGLE_PAY_COUNTRY_CODE = "braintree.googlepay.seller.country.code";
	public static final String AUTH_AMOUNT_TO_VERIFY_CARD = "braintree.authAmount.verify.card";
	public static final String IS_ENABLED = "isEnabled";

	public static final String EMPTY_STRING = "";

	public static final String NAME = "BorrowLenses*Rental";
	public static final String URL = "borrowlenses.com";
	public static final String PHONE_NUMBER = "8448536737";

	private BraintreeConstants()
	{
		//empty to avoid instantiating this constant class
	}

}
