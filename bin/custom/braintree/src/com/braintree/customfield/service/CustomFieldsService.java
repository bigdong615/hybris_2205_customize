package com.braintree.customfield.service;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import java.util.Map;

/**
 * It sets the custom fields value
 */
public interface CustomFieldsService
{
	Map<String, String> getDefaultCustomFieldsMap();

  /**
   * It gets the custom fields value
   * @param order the order model
   * @return the custom fields
   */
  Map<String, String> getDefaultCustomFieldsMap(final AbstractOrderModel order);
}
