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

import org.apache.commons.lang3.StringUtils;

import com.bl.core.constants.BlCoreConstants;
import com.braintree.model.BrainTreePaymentInfoModel;


public class BrainTreePaymentInfoPopulator implements Populator<BrainTreePaymentInfoModel, CCPaymentInfoData>
{
	private Converter<AddressModel, AddressData> addressConverter;

	@Override
	public void populate(final BrainTreePaymentInfoModel source, final CCPaymentInfoData target) throws ConversionException
	{
		target.setId(source.getPk().toString());
		target.setCardNumber(getMaskedLastFourDigit(source.getCardNumber()));
		target.setExpiryYear(source.getExpirationYear());
		target.setExpiryMonth(source.getExpirationMonth());
		target.setAccountHolderName(source.getImageSource());
		target.setSubscriptionId(source.getPaymentProvider());
		target.setSaved(source.isSaved());
		target.setPaymentMethodToken(source.getPaymentMethodToken());
		target.setCardholderName(source.getCardholderName());
		target.setPayer(source.getPayer());
		target.setPaymentMethodNonce(source.getNonce());
		target.setIsDefault(source.isIsDefault());
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
	 * Gets the masked last four digit.
	 *
	 * @param cardNumber the card number
	 * @return the masked last four digit
	 */
	private String getMaskedLastFourDigit(final String cardNumber)
	{
		if(StringUtils.isNotBlank(cardNumber) && cardNumber.contains(BlCoreConstants.CARD_MASK))
		{
			final String[] cardNumberSplit = cardNumber.split(BlCoreConstants.MASKED_CARD_SEPARATOR);
			if(cardNumberSplit.length == BlCoreConstants.INT_TWO)
			{
				return String.format(BlCoreConstants.MASKED_CARD_FORMAT, cardNumberSplit[BlCoreConstants.INT_ONE]);
			}
		}
		return cardNumber;
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
