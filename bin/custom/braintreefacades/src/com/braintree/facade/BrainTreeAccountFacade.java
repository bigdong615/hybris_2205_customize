package com.braintree.facade;

import com.braintree.hybris.data.PayPalConfigurationData;


public interface BrainTreeAccountFacade
{

	/**
	 * Get base data configuration for UI
	 * 
	 * @return PayPalConfigurationData
	 */
	PayPalConfigurationData getPayPalConfigurationData();
}
