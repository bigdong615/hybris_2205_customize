package com.braintree.customfield.service.impl;

import com.braintree.constants.BraintreeConstants;
import com.braintree.customfield.service.CustomFieldsService;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.servicelayer.config.ConfigurationService;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;


public class CustomFieldsServiceImpl implements CustomFieldsService
{

	private ConfigurationService configurationService;
	@Autowired
	private CartService cartService;

	@Override
	public Map<String, String> getDefaultCustomFieldsMap()
	{
		final Iterator<String> customFieldsNames = configurationService.getConfiguration()
				.getKeys(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY);
		final Map<String, String> customFields = new HashMap<>();

		while (customFieldsNames.hasNext())
		{
			final CartModel cartModel = cartService.getSessionCart();
			final String customFieldName = customFieldsNames.next();
			String customFieldValue = "";
			if(StringUtils.contains(customFieldName, "field_1")) {
				customFieldValue = String.valueOf(cartModel.getTotalPrice());
			} else {
				customFieldValue = cartModel.getCode();
			}
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
