package com.braintree.facade.impl;

import de.hybris.platform.servicelayer.i18n.I18NService;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.facade.BrainTreeAccountFacade;
import com.braintree.hybris.data.PayPalConfigurationData;



public class BrainTreeAccountFacadeImpl implements BrainTreeAccountFacade
{
	private BrainTreeConfigService brainTreeConfigService;
	private I18NService i18NService;

	@Override
	public PayPalConfigurationData getPayPalConfigurationData()
	{
		final PayPalConfigurationData payPalConfigurationData = new PayPalConfigurationData();
		payPalConfigurationData.setEnvironment(getBrainTreeConfigService().getEnvironmentTypeName());
		payPalConfigurationData.setSecure3d(getBrainTreeConfigService().get3dSecureConfiguration());
		payPalConfigurationData.setSkip3dSecureLiabilityResult(getBrainTreeConfigService().getIsSkip3dSecureLiabilityResult());
		payPalConfigurationData.setAdvancedFraudTools(getBrainTreeConfigService().getAdvancedFraudTools());
		payPalConfigurationData.setDbaName(getBrainTreeConfigService().getCreditCardStatementName());
		payPalConfigurationData.setStoreInVault(String.valueOf(getBrainTreeConfigService().getStoreInVaultIgnoringIntent()));
		payPalConfigurationData.setLocale(getI18NService().getCurrentLocale().toString());
		payPalConfigurationData.setCurrency(getI18NService().getCurrentJavaCurrency().toString());
		payPalConfigurationData.setIntent(getBrainTreeConfigService().getIntent());
		return payPalConfigurationData;
	}

	public BrainTreeConfigService getBrainTreeConfigService()
	{
		return brainTreeConfigService;
	}

	public void setBrainTreeConfigService(final BrainTreeConfigService brainTreeConfigService)
	{
		this.brainTreeConfigService = brainTreeConfigService;
	}

	public I18NService getI18NService()
	{
		return i18NService;
	}

	public void setI18NService(final I18NService i18NService)
	{
		this.i18NService = i18NService;
	}
}
