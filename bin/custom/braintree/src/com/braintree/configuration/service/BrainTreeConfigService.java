package com.braintree.configuration.service;

import com.braintree.constants.BraintreeConstants;
import com.braintreegateway.Environment;
import com.braintreegateway.util.BraintreeCrypto;
import de.hybris.platform.servicelayer.config.ConfigurationService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.site.BaseSiteService;
import java.math.BigDecimal;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.braintree.constants.BraintreeConstants.B2B_ENDPOINT_URL_PROPERTY;
import static com.braintree.constants.BraintreeConstants.B2C_ENDPOINT_URL_PROPERTY;
import static com.braintree.constants.BraintreeConstants.B2C_FLOW;
import static com.braintree.constants.BraintreeConstants.BILLING_AGREEMENT_DESCRIPTION_KEY;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_3D_SECURE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_ACCEPTED_PAYMENT_METHODS;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_ACCEPTED_PAYMENT_METHODS_DELIMETER;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_ADVANCED_FRAUD_TOOLS_ENABLED;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_APPLE_PAY_ENABLE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_CREDIT_ENABLE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_CHANNEL_NAME;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_CREDIT_CARD_STATEMENT_NAME;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_ENVIRONMENT;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_GOOGLE_PAY_COUNTRY_CODE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_GOOGLE_PAY_ENABLE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_GOOGLE_PAY_MERCHANT_ID;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_IMAGES_PREFIX;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_KEY;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_LOCALE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_LOCAL_PAYMENTS_ENABLE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_MERCHANT_ACCOUNT_PREFIX;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PAYPAL_INTENT;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_SUBMIT_FOR_SETTLEMENT;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_VENMO_ENABLE;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_VENMO_PROFILE_ID;
import static com.braintree.constants.BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER;
import static com.braintree.constants.BraintreeConstants.CURRENCY_MERCHANT_ACCOUNT_ID;
import static com.braintree.constants.BraintreeConstants.ENVIRONMENT_PRODUCTION;
import static com.braintree.constants.BraintreeConstants.ENVIRONMENT_SANDBOX;
import static com.braintree.constants.BraintreeConstants.HOSTED_FIELDS_ENABLED;
import static com.braintree.constants.BraintreeConstants.HYBRIS_BUILD_API_VERSION;
import static com.braintree.constants.BraintreeConstants.IS_SKIP_3D_SECURE_LIABILITY_RESULT;
import static com.braintree.constants.BraintreeConstants.MULTICAPTURE_ENABLED;
import static com.braintree.constants.BraintreeConstants.PAYPAL_DISABLE_FUNDING_MARK_PAGE;
import static com.braintree.constants.BraintreeConstants.PAYPAL_EXPRESS_ENABLED;
import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_ORDER;
import static com.braintree.constants.BraintreeConstants.PAYPAL_STANDARD_ENABLED;
import static com.braintree.constants.BraintreeConstants.SINGLE_USE_PARAMETER;
import static com.braintree.constants.BraintreeConstants.STORE_IN_VAULT;
import static com.braintree.constants.BraintreeConstants.VERIFY_CARD;
import static com.braintree.constants.BraintreeConstants.VERIFY_CARD_ON_VAULTING;

public class BrainTreeConfigService
{
	private final static Logger LOG = Logger.getLogger(BrainTreeConfigService.class);
	private final Map<String, Environment> ENV_TYPE_MAP = new HashMap()
	{
		{
			put(ENVIRONMENT_SANDBOX, Environment.SANDBOX);
			put(ENVIRONMENT_PRODUCTION, Environment.PRODUCTION);
		}
	};
	private BaseSiteService baseSiteService;
	private CommonI18NService commonI18NService;
	private ConfigurationService configurationService;
	private UserService userService;
	@Resource(name = "sessionService")
	private SessionService sessionService;

	public Environment getEnvironmentType()
	{
		if (ENV_TYPE_MAP.containsKey(getConfiguration().getString(BRAINTREE_ENVIRONMENT)))
		{
			return ENV_TYPE_MAP.get(getConfiguration().getString(BRAINTREE_ENVIRONMENT));
		}
		LOG.error("Configuration environment property name is incorrect! Use environment: " + Environment.DEVELOPMENT);
		return Environment.DEVELOPMENT;
	}

	/**
	 * @return Configuration
	 */
	private Configuration getConfiguration()
	{
		return getConfigurationService().getConfiguration();
	}

	public Boolean getSettlementConfigParameter()
	{
		boolean settlementConfigParameter = false;
		String settlementValue = getConfiguration().getString(BRAINTREE_SUBMIT_FOR_SETTLEMENT);
		if (settlementValue.equalsIgnoreCase(Boolean.TRUE.toString()))
		{
			settlementConfigParameter = true;
		}
		else if (!settlementValue.equalsIgnoreCase(Boolean.FALSE.toString()))
		{
			LOG.warn("'braintree.submit.for.settlement' value is invalid. Set default 'false' value");
		}
		return settlementConfigParameter;
	}

	public String getEnvironmentTypeName()
	{
		return getEnvironmentType().getEnvironmentName();
	}

	public Boolean get3dSecureConfiguration()
	{
		return Boolean.valueOf(getConfiguration().getBoolean(BRAINTREE_3D_SECURE, false));
	}

	public Boolean getAdvancedFraudTools()
	{
		return Boolean.valueOf(getConfiguration().getBoolean(BRAINTREE_ADVANCED_FRAUD_TOOLS_ENABLED, true));
	}

	public Boolean getSingleUse()
	{
		return Boolean.valueOf(getConfiguration().getBoolean(SINGLE_USE_PARAMETER, true));
	}

	public Boolean getHostedFieldEnabled()
	{
		return Boolean.valueOf(getConfiguration().getBoolean(HOSTED_FIELDS_ENABLED, true));
	}

	public Boolean getPayPalExpressEnabled()
	{
		return getPayPalStandardEnabled() ?
				Boolean.valueOf(getConfiguration().getBoolean(PAYPAL_EXPRESS_ENABLED, true)) :
				Boolean.FALSE;
	}

	public Boolean getPayPalStandardEnabled()
	{
		return Boolean.valueOf(getConfiguration().getBoolean(PAYPAL_STANDARD_ENABLED, true));
	}

	public Boolean getIsSkip3dSecureLiabilityResult()
	{
		return Boolean.valueOf(getConfiguration().getBoolean(IS_SKIP_3D_SECURE_LIABILITY_RESULT, false));
	}

	public String getCreditCardStatementName()
	{
		return getConfiguration().getString(BRAINTREE_CREDIT_CARD_STATEMENT_NAME);
	}

	public String getBrainTreeChannel()
	{
		return BraintreeCrypto
				.decrypt(getConfiguration().getString(BRAINTREE_KEY), getConfiguration().getString(BRAINTREE_CHANNEL_NAME));
	}

	public String getMerchantAccountIdForCurrentSiteAndCurrency()
	{
		return getMerchantAccountIdByCurrentSiteNameAndCurrency(getBaseSiteService().getCurrentBaseSite().getUid(),
				getCommonI18NService().getCurrentCurrency().getIsocode().toLowerCase());
	}

	public String getMerchantAccountIdForCurrentSiteAndCurrencyIsoCode(final String currencyIsoCode)
	{
		return getMerchantAccountIdByCurrentSiteNameAndCurrency(getBaseSiteService().getCurrentBaseSite().getUid(),
				currencyIsoCode.toLowerCase());
	}

	public String getMerchantAccountIdForSiteAndCurrencyIsoCode(final String baseSiteUid, final String currencyIsoCode)
	{
		return getMerchantAccountIdByCurrentSiteNameAndCurrency(baseSiteUid, currencyIsoCode.toLowerCase());
	}

	public String getAcceptedPaymentMethods()
	{
		return getConfiguration().getString(BRAINTREE_ACCEPTED_PAYMENT_METHODS);
	}

	public String getStoreInVault()
	{
		return getConfiguration().getString(STORE_IN_VAULT);
	}
	//if false - will not allow vault flow for guest user
	public boolean getStoreInVaultIgnoringIntent()
	{
		return Boolean.parseBoolean(getStoreInVault());
	}

	public String getStoreInVaultForCurrentUser()
	{
		if (userService.isAnonymousUser(userService.getCurrentUser()) || PAYPAL_INTENT_ORDER.equals(getIntent()))
		{
			return Boolean.FALSE.toString();
		}
		return getStoreInVault();
	}
	//if false - will not allow to create customer on braintree with vault flow for guest user
	public boolean getVaultingForCurrentUser(String paymentProvider)
	{
		boolean isPayPalCheckout = BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT.equals(paymentProvider)
				|| BraintreeConstants.PAYPAL_PAYMENT.equals(paymentProvider);

		if (PAYPAL_INTENT_ORDER.equals(getIntent()) && isPayPalCheckout)
		{
			return false;
		}
		return Boolean.parseBoolean(getStoreInVault());
	}

	public String getStoreInVaultForCardVaulting(String braintreeCustomerId)
	{

		boolean isVault = !(StringUtils.isEmpty(braintreeCustomerId));
		return isVault ? getStoreInVault() : Boolean.FALSE.toString();
	}

	public String getStoreInVaultForCurrentUser(final String braintreeCustomerId)
	{
		boolean isVault = !(StringUtils.isEmpty(braintreeCustomerId) || PAYPAL_INTENT_ORDER.equals(getIntent()));
		return isVault ? getStoreInVault() : Boolean.FALSE.toString();
	}

	public Map<String, String> getAcceptedPaymentMethodImages()
	{
		final Map<String, String> acceptedPaymentMethodImages = new HashMap<>();
		final String acceptedPaymentMethods = getAcceptedPaymentMethods();
		if (StringUtils.isNotBlank(acceptedPaymentMethods))
		{
			final String paymentMethods = StringUtils.deleteWhitespace(acceptedPaymentMethods);

			final List<String> paymentMethodList = Arrays
					.asList(StringUtils.split(paymentMethods, BRAINTREE_ACCEPTED_PAYMENT_METHODS_DELIMETER));

			for (final String paymentMethod : paymentMethodList)
			{
				final String imageLink = getConfiguration().getString(BRAINTREE_IMAGES_PREFIX + paymentMethod);
				if (StringUtils.isNotBlank(imageLink))
				{
					acceptedPaymentMethodImages.put(paymentMethod, imageLink);
				}
			}
		}

		return acceptedPaymentMethodImages;
	}

	public String getMerchantAccountIdByCurrentSiteNameAndCurrency(final String currentSiteName, final String currency)
	{
		final String merchantAccountId = getConfiguration()
				.getString(BRAINTREE_MERCHANT_ACCOUNT_PREFIX + currentSiteName + CONFIGURATION_PROPERTY_DELIMETER + currency);
		return merchantAccountId;
	}

	public boolean getVerifyCard()
	{
		return getConfiguration().getBoolean(VERIFY_CARD);
	}

	public boolean getVerifyCardOnVaulting()
	{
		return getConfiguration().getBoolean(VERIFY_CARD_ON_VAULTING);
	}

	public boolean getMultiCaptureEnabled()
	{
		return getConfiguration().getBoolean(MULTICAPTURE_ENABLED, true);
	}

	public boolean getB2CFlowEnabled()
	{
		return getConfiguration().getBoolean(B2C_FLOW, true);
	}

	public String getCurrencyMerchantAccountId()
	{
		return getConfiguration().getString(CURRENCY_MERCHANT_ACCOUNT_ID);
	}

	public String getEndpointURL()
	{
		return getConfiguration().getString(B2C_ENDPOINT_URL_PROPERTY);
	}

	public String getB2BEndpointURL()
	{
		return getConfiguration().getString(B2B_ENDPOINT_URL_PROPERTY);
	}

	public String getDisableFunding()
	{
		return getConfiguration().getString(PAYPAL_DISABLE_FUNDING_MARK_PAGE);
	}

	/**
	 * @return the baseSiteService
	 */
	public BaseSiteService getBaseSiteService()
	{
		return baseSiteService;
	}

	/**
	 * @param baseSiteService the baseSiteService to set
	 */
	public void setBaseSiteService(final BaseSiteService baseSiteService)
	{
		this.baseSiteService = baseSiteService;
	}

	/**
	 * @return the commonI18NService
	 */
	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	/**
	 * @param commonI18NService the commonI18NService to set
	 */
	public void setCommonI18NService(final CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
	}

	/**
	 * @return the configurationService
	 */
	public ConfigurationService getConfigurationService()
	{
		return configurationService;
	}

	/**
	 * @param configurationService the configurationService to set
	 */
	public void setConfigurationService(final ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}

	public UserService getUserService()
	{
		return userService;
	}

	public void setUserService(UserService userService)
	{
		this.userService = userService;
	}

	public String getIntent()
	{
		return getConfiguration().getString(BRAINTREE_PAYPAL_INTENT).toLowerCase();
	}

	public String getBrainTreeLocale()
	{
		return getConfiguration().getString(BRAINTREE_LOCALE);
	}

	public Integer getHybrisBuildApiVersion()
	{
		String apiVersion = getConfiguration().getString(HYBRIS_BUILD_API_VERSION);
		if (StringUtils.isNotEmpty(apiVersion))
		{
			String[] version = apiVersion.split("\\.");
			if (version.length > 0 && StringUtils.isNumeric(version[0]))
			{
				return Integer.valueOf(version[0]);
			}
		}
		return null;
	}

	public String getVenmoProfileId()
	{
		return getConfiguration().getString(BRAINTREE_VENMO_PROFILE_ID, "");
	}

	public boolean getVenmoEnabled()
	{
		return getConfiguration().getBoolean(BRAINTREE_VENMO_ENABLE, false);
	}

	public boolean getApplePayEnabled()
	{
		return getConfiguration().getBoolean(BRAINTREE_APPLE_PAY_ENABLE, false);
	}

	public boolean getLocalPaymentsEnabled()
	{
		return getConfiguration().getBoolean(BRAINTREE_LOCAL_PAYMENTS_ENABLE, true);
	}

	public boolean getCreditEnabled()
	{
		return getConfiguration().getBoolean(BRAINTREE_CREDIT_ENABLE, false);
	}

	public Object getBillingAgreementDescription()
	{
		return getConfiguration().getString(BILLING_AGREEMENT_DESCRIPTION_KEY, "");
	}

	public String getGooglePayMerchantId()
	{
		return getConfiguration().getString(BRAINTREE_GOOGLE_PAY_MERCHANT_ID);
	}

	public boolean getGooglePayEnabled()
	{
		return getConfiguration().getBoolean(BRAINTREE_GOOGLE_PAY_ENABLE, false);
	}

	public String getGooglePayCountryCode()
	{
		return getConfiguration().getString(BRAINTREE_GOOGLE_PAY_COUNTRY_CODE);
	}

	public boolean isOneOfPaymentMethodsEnabled()
	{
		return getPayPalStandardEnabled() || getHostedFieldEnabled() || getVenmoEnabled() || getLocalPaymentsEnabled()
				|| getApplePayEnabled() || getGooglePayEnabled();
	}

	public BigDecimal getAuthAMountToVerifyCard()
	{
		return getConfiguration().getBigDecimal(AUTH_AMOUNT_TO_VERIFY_CARD);
	}
}
