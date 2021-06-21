/*
 * [y] hybris Platform
 *
 * Copyright (c) 2000-2016 hybris AG
 * All rights reserved.
 *
 * This software is the confidential and proprietary information of hybris
 * ("Confidential Information"). You shall not disclose such Confidential
 * Information and shall use it only in accordance with the terms of the
 * license agreement you entered into with hybris.
 *
 *
 */
package com.braintree.converters;

import com.braintree.enums.BrainTreeCardType;
import com.braintree.enums.BrainTreePaymentMethod;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintreegateway.Address;
import com.braintreegateway.AmexExpressCheckoutCard;
import com.braintreegateway.AndroidPayCard;
import com.braintreegateway.ApplePayCard;
import com.braintreegateway.CreditCard;
import com.braintreegateway.PayPalAccount;
import com.braintreegateway.PaymentMethod;
import de.hybris.platform.converters.impl.AbstractPopulatingConverter;

import java.util.Calendar;
import java.util.Date;


public class BraintreePaymentMethodConverter extends AbstractPopulatingConverter<PaymentMethod, BrainTreePaymentInfoModel>
{
	private BraintreeAddressConverter braintreeAddressConverter;

	@Override
	public void populate(final PaymentMethod method, final BrainTreePaymentInfoModel model)
	{
		if (method != null && model != null)
		{
			model.setPaymentMethodToken(method.getToken());
			model.setImageSource(method.getImageUrl());
			model.setIsDefault(method.isDefault());

			if (method instanceof CreditCard)
			{
				saveCreditCardInfo((CreditCard) method, model);
			}
			else if (method instanceof PayPalAccount)
			{
				savePayPalInfo((PayPalAccount) method, model);
			}
			else if (method instanceof ApplePayCard)
			{
				saveApplePayCardInfo((ApplePayCard) method, model);
			}
			else if (method instanceof AndroidPayCard)
			{
				saveAndroidPayCardInfo((AndroidPayCard) method, model);
			}
			else if (method instanceof AmexExpressCheckoutCard)
			{
				saveAmexExpressCheckoutCardInfo((AmexExpressCheckoutCard) method, model);
			}
		}
	}

	private void saveCreditCardInfo(final CreditCard method, final BrainTreePaymentInfoModel model)
	{
		final CreditCard cc = method;
		final String formattedCardNumber = String.format("%s******%s", cc.getBin(), cc.getLast4());
		model.setPaymentInfo(formattedCardNumber);
		model.setUpdatedAt(cc.getUpdatedAt().getTime());
		model.setCustomerId(cc.getCustomerId());
		model.setCardType(BrainTreeCardType.valueOf(cc.getCardType()));
		model.setCardholderName(cc.getCardholderName());
		model.setCardNumber(formattedCardNumber);
		model.setExpirationMonth(cc.getExpirationMonth());
		model.setExpirationYear(cc.getExpirationYear());
		model.setIsDefault(cc.isDefault());
		model.setCustomerLocation(cc.getCustomerLocation());
		model.setCreatedAt(formatDate(cc.getCreatedAt()));
		model.setPaymentMethodToken(cc.getToken());
		model.setPaymentProvider(BrainTreePaymentMethod.CREDITCARD.toString());

		final Address billingAddress = cc.getBillingAddress();
		if (billingAddress != null)
		{
			model.setBillingAddress(getBraintreeAddressConverter().convert(billingAddress));
		}
	}

	private void savePayPalInfo(final PayPalAccount method, final BrainTreePaymentInfoModel model)
	{
		final PayPalAccount ppa = method;
		model.setPaymentInfo(ppa.getEmail());
		model.setUpdatedAt(ppa.getUpdatedAt().getTime());
		model.setPaymentMethodToken(ppa.getToken());
		model.setCustomerId(ppa.getCustomerId());
		model.setCreatedAt(formatDate(ppa.getCreatedAt()));
		model.setPaymentMethodToken(ppa.getToken());
		model.setIsDefault(ppa.isDefault());
		model.setPaymentProvider(BrainTreePaymentMethod.PAYPAL.toString());
	}

	private void saveApplePayCardInfo(final ApplePayCard method, final BrainTreePaymentInfoModel model)
	{
		final ApplePayCard apc = method;
		model.setPaymentInfo(apc.getLast4());
		model.setUpdatedAt(apc.getUpdatedAt().getTime());
		model.setCreatedAt(formatDate(apc.getCreatedAt()));
		model.setPaymentMethodToken(apc.getToken());
		model.setIsDefault(apc.isDefault());
		model.setExpirationMonth(apc.getExpirationMonth());
		model.setExpirationYear(apc.getExpirationYear());
		model.setPaymentProvider(BrainTreePaymentMethod.APPLEPAYCARD.toString());
	}

	private void saveAndroidPayCardInfo(final AndroidPayCard method, final BrainTreePaymentInfoModel model)
	{
		final AndroidPayCard apc = method;
		model.setPaymentInfo(String.format("%s******%s", apc.getBin(), apc.getLast4()));
		model.setUpdatedAt(apc.getUpdatedAt().getTime());
		model.setCreatedAt(formatDate(apc.getCreatedAt()));
		model.setPaymentMethodToken(apc.getToken());
		model.setExpirationMonth(apc.getExpirationMonth());
		model.setExpirationYear(apc.getExpirationYear());
		model.setPaymentProvider(BrainTreePaymentMethod.ANDROIDPAYCARD.toString());
	}

	private void saveAmexExpressCheckoutCardInfo(final AmexExpressCheckoutCard method, final BrainTreePaymentInfoModel model)
	{
		final AmexExpressCheckoutCard aecc = method;
		model.setPaymentInfo(aecc.getBin());
		model.setUpdatedAt(aecc.getUpdatedAt().getTime());
		model.setCreatedAt(formatDate(aecc.getCreatedAt()));
		model.setPaymentMethodToken(aecc.getToken());
		model.setIsDefault(aecc.isDefault());
		model.setCustomerId(aecc.getCustomerId());
		model.setExpirationMonth(aecc.getExpirationMonth());
		model.setExpirationYear(aecc.getExpirationYear());
		model.setPaymentProvider(BrainTreePaymentMethod.AMEXEXPRESSCHECKOUTCARD.toString());
	}

	private Date formatDate(final Calendar createdAt)
	{
		if (createdAt != null)
		{
			return createdAt.getTime();
		}
		return null;
	}

	public BraintreeAddressConverter getBraintreeAddressConverter()
	{
		return braintreeAddressConverter;
	}

	public void setBraintreeAddressConverter(final BraintreeAddressConverter braintreeAddressConverter)
	{
		this.braintreeAddressConverter = braintreeAddressConverter;
	}
}
