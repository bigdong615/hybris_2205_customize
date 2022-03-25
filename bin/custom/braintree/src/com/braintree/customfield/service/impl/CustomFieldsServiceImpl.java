package com.braintree.customfield.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.braintree.constants.BraintreeConstants;
import com.braintree.customfield.service.CustomFieldsService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.config.ConfigurationService;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.apache.commons.lang3.BooleanUtils;


public class CustomFieldsServiceImpl implements CustomFieldsService
{

	private ConfigurationService configurationService;
	private static final String HYPHEN = "-";
	private static final String SPACE = " ";

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

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, String> getDefaultCustomFieldsMap(final AbstractOrderModel order) {
		final Iterator<String> customFieldsNames = configurationService.getConfiguration()
				.getKeys(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY);
		final Map<String, String> customFields = new HashMap<>();
		while (customFieldsNames.hasNext())
		{
			final String customFieldName = customFieldsNames.next();
			setCustomFields(order, customFieldName, customFields);
		}
		return customFields;
	}

	/**
	 * It sets the custom fields value
	 * @param order the order model
	 * @param customFieldName the custom field name
	 * @param customFields the custom fields
	 */
	private void setCustomFields(final AbstractOrderModel order, String customFieldName, final
			Map<String, String> customFields) {
		final CustomerModel customer = (CustomerModel) order.getUser();
		customFieldName = customFieldName
				.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", "");
		switch (customFieldName) {
			case "field_1":
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", ""),
						String.valueOf(customer.getOrderCount()));
				break;
			case "field_2":
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", ""),
						String.valueOf(customer.getInprocessOrderCount()));
				break;
			case "field_3":
				StringBuilder outstandingBill = new StringBuilder();
				customer.getOutstandingBills().forEach(bill ->
					outstandingBill.append(bill.getBillChargeType()
							.getCode().concat(HYPHEN).concat(bill.getChargedAmount().toString()).concat(SPACE)));
				customFields.put(customFieldName
						.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", ""), outstandingBill.toString());
				break;
			case "field_4":
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", ""),
						String.valueOf(BigDecimal.valueOf(customer.getAverageGearOrderValue()).setScale(
								BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN)));
				break;
			case "field_5":
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", ""),
						String.valueOf(BigDecimal.valueOf(customer.getGearValueOrdersInProgress())
								.setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN)));
				break;
			case "field_6":
				if(BooleanUtils.isTrue(order.getIsRentalOrder())) {
					final LocalDateTime rentalStartDate = getFormattedDateTime(order.getRentalStartDate());
					final LocalDateTime rentalEndDate = getFormattedDateTime(order.getRentalEndDate());
					customFields.put(customFieldName
									.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", ""),
							String.valueOf(ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate.plusDays(1))));
					break;
				}
			default:
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + ".", ""), "");
		}
	}

	/**
	 * It gets the formatted date
	 * @param date the date
	 * @return the formatted date
	 */
	private static LocalDateTime getFormattedDateTime(final Date date)
	{
		final Instant instant = Instant.ofEpochMilli(date.getTime());
		return LocalDateTime.ofInstant(instant, ZoneId.systemDefault());
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
