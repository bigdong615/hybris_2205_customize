/**
 *
 */
package com.braintree.converters.populators.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;

import de.hybris.platform.converters.Populator;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;

import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.payment.dto.BraintreeInfo;


public class BrainTreeInfoPopulator implements Populator<BrainTreeSubscriptionInfoData, BraintreeInfo>
{
	@Override
	public void populate(final BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData, final BraintreeInfo target)
			throws ConversionException
	{
		validateParameterNotNull(brainTreeSubscriptionInfoData, "Subscription data cannot be null");

		target.setPaymentMethodToken(brainTreeSubscriptionInfoData.getPaymentMethodToken());
		target.setNonce(brainTreeSubscriptionInfoData.getNonce());
		target.setDeviceData(brainTreeSubscriptionInfoData.getDeviceData());
		target.setImageSource(brainTreeSubscriptionInfoData.getImageSource());
		target.setExpirationMonth(brainTreeSubscriptionInfoData.getExpirationMonth());
		target.setExpirationYear(brainTreeSubscriptionInfoData.getExpirationYear());
		target.setLiabilityShifted(brainTreeSubscriptionInfoData.getLiabilityShifted());

		target.setPaymentProvider(brainTreeSubscriptionInfoData.getPaymentProvider());
		target.setSavePaymentInfo(brainTreeSubscriptionInfoData.isSavePaymentInfo());
		target.setShouldBeSaved(brainTreeSubscriptionInfoData.isShouldBeSaved());
		target.setCardNumber(brainTreeSubscriptionInfoData.getCardNumber());
		target.setCardType(brainTreeSubscriptionInfoData.getCardType());
		target.setEmail(brainTreeSubscriptionInfoData.getEmail());
		target.setCardholderName(brainTreeSubscriptionInfoData.getCardholder());
		target.setIntent(brainTreeSubscriptionInfoData.getIntent());
		target.setAmount(brainTreeSubscriptionInfoData.getAmount());
	    target.setIsDefault(brainTreeSubscriptionInfoData.getIsDefault());
    target.setBraintreeAddressId(brainTreeSubscriptionInfoData.getBraintreeAddressId());
	}
}
