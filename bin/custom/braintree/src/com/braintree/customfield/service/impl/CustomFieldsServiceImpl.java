package com.braintree.customfield.service.impl;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.braintree.constants.BraintreeConstants;
import com.braintree.customfield.service.CustomFieldsService;

import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
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
import java.util.Objects;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;


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
				.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY);
		switch (customFieldName) {
			case "order_count":
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY),
						String.valueOf(customer.getOrderCount()));
				break;
			case "incompleted_order_count":
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY),
						String.valueOf(customer.getInprocessOrderCount()));
				break;
			case "outstanding_bill":
				StringBuilder outstandingBill = new StringBuilder();
				customer.getOutstandingBills().forEach(bill ->
					outstandingBill.append(bill.getBillChargeType()
							.getCode().concat(HYPHEN).concat(bill.getChargedAmount().toString()).concat(SPACE)));
				customFields.put(customFieldName
						.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY), outstandingBill.toString());
				break;
			case "average_order_value":
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY),
						String.valueOf(BigDecimal.valueOf(customer.getAverageGearOrderValue()).setScale(
								BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN)));
				break;
			case "sum_of_gear_value_in_pre_shipping_status":
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY),
						String.valueOf(BigDecimal.valueOf(getGearValueOrdersInProgress(order, customer))
								.setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN)));
				break;
			case "rental_duration":
				if(BooleanUtils.isTrue(order.getIsRentalOrder())) {
					final LocalDateTime rentalStartDate = getFormattedDateTime(order.getRentalStartDate());
					final LocalDateTime rentalEndDate = getFormattedDateTime(order.getRentalEndDate());
					customFields.put(customFieldName
									.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY),
							String.valueOf(ChronoUnit.DAYS.between(rentalStartDate, rentalEndDate.plusDays(1))));
				}
				else
				{
					customFields.put(customFieldName
							.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY), StringUtils.EMPTY);
				}
				break;
			case "order_number":
				customFields.put(customFieldName
						.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY),
						order instanceof CartModel ? StringUtils.EMPTY : order.getCode());
				break;
			case "sum_of_gear_value_in_order":
				customFields.put(customFieldName
						.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY),
						String.valueOf(order.getSumOfGearValueOnOrder()));
				break;
			case "phone_number":
				customFields.put(customFieldName
						.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY),
						getPhoneNumber(order));
				break;
			default:
				customFields.put(customFieldName
								.replaceFirst(BraintreeConstants.BRAINTRE_CUSTOM_FIELD_GENERAL_KEY + BraintreeConstants.CONFIGURATION_PROPERTY_DELIMETER, StringUtils.EMPTY), StringUtils.EMPTY);
		}
	}
	
	private double getGearValueOrdersInProgress(final AbstractOrderModel order, final CustomerModel customer)
	{
		if(order instanceof CartModel)
		{
			final Double priceOfProducts = order.getEntries().stream().mapToDouble(AbstractOrderEntryModel::getTotalPrice).sum();
			return customer.getGearValueOrdersInProgress() + priceOfProducts.doubleValue();
		}
		return customer.getGearValueOrdersInProgress();
	}
	
	private String getPhoneNumber(final AbstractOrderModel order)
	{
		if(order instanceof CartModel)
		{
			return Objects.nonNull(order.getPaymentInfo()) && Objects.nonNull(order.getPaymentInfo().getBillingAddress())
					&& StringUtils.isNotBlank(order.getPaymentInfo().getBillingAddress().getPhone1()) 
					? order.getPaymentInfo().getBillingAddress().getPhone1()
							: StringUtils.EMPTY;			
		}
		return Objects.nonNull(order.getPaymentAddress()) && StringUtils.isNotBlank(order.getPaymentAddress().getPhone1()) 
				? order.getPaymentAddress().getPhone1() 
						: StringUtils.EMPTY;
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
