package com.braintree.customfield.service.impl;

import com.braintree.constants.BraintreeConstants;
import com.braintree.customfield.service.CustomFieldsService;
import de.hybris.platform.servicelayer.config.ConfigurationService;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;


public class CustomFieldsServiceImpl implements CustomFieldsService
{

	private ConfigurationService configurationService;

	@Override
	public Map<String, String> getDefaultCustomFieldsMap()
	{
		final Iterator<String> customFieldsNames = configurationService.getConfiguration()
				.getKeys(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY);
		final Map<String, String> customFields = new HashMap<>();

		while (customFieldsNames.hasNext())
		{
			final String customFieldName = customFieldsNames.next();
			final String customFieldValue = configurationService.getConfiguration().getString(customFieldName);
			if (!"".equals(customFieldValue))
			{
				customFields.put(customFieldName.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", ""),
						customFieldValue);
			}
		}

		return customFields;
	}

	public ConfigurationService getConfigurationService()
	{
		return configurationService;
	}

	public void setConfigurationService(final ConfigurationService configurationService)
	{
		this.configurationService = configurationService;
	}

}
