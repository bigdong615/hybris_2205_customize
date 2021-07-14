/**
 *
 */
package com.braintree.converters.populators.impl;

import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.converters.Populator;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import com.braintree.model.BrainTreePaymentInfoModel;


public class BrainTreePaymentInfoPopulator implements Populator<BrainTreePaymentInfoModel, CCPaymentInfoData>
{
	private Converter<AddressModel, AddressData> addressConverter;

	@Override
	public void populate(final BrainTreePaymentInfoModel source, final CCPaymentInfoData target) throws ConversionException
	{
		target.setId(source.getPk().toString());
		target.setCardNumber(source.getCardNumber());
		target.setExpiryYear(source.getExpirationYear());
		target.setExpiryMonth(source.getExpirationMonth());
		target.setAccountHolderName(source.getImageSource());
		target.setSubscriptionId(source.getPaymentProvider());
		target.setSaved(source.isSaved());
		target.setPaymentMethodToken(source.getPaymentMethodToken());
		target.setCardholderName(source.getCardholderName());
		target.setPayer(source.getPayer());
		target.setPaymentMethodNonce(source.getNonce());
		if (source.getCardType() != null)
		{
			target.setCardType(source.getCardType().toString());
		}

		if (source.getBillingAddress() != null)
		{
			target.setBillingAddress(getAddressConverter().convert(source.getBillingAddress()));
		}
	}

	/**
	 * @return the addressConverter
	 */
	public Converter<AddressModel, AddressData> getAddressConverter()
	{
		return addressConverter;
	}

	/**
	 * @param addressConverter
	 *           the addressConverter to set
	 */
	public void setAddressConverter(final Converter<AddressModel, AddressData> addressConverter)
	{
		this.addressConverter = addressConverter;
	}

}
