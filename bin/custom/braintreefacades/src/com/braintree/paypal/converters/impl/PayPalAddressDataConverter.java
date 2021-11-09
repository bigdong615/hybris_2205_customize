package com.braintree.paypal.converters.impl;

import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import com.bl.core.constants.BlCoreConstants;
import com.braintree.hybris.data.PayPalAddressData;
import com.braintree.paypal.validator.PayPalAddressDetailValidator;


public class PayPalAddressDataConverter implements Converter<AddressModel, PayPalAddressData>
{

	private static final String EMPTY_STRING = "";
	PayPalAddressDetailValidator payPalAddressDetailValidator;

	@Override
	public PayPalAddressData convert(final AddressModel hybrisAddress) throws ConversionException
	{
		final PayPalAddressData payPalAddressData = new PayPalAddressData();
		return convert(hybrisAddress, payPalAddressData);
	}

	@Override
	public PayPalAddressData convert(final AddressModel hybrisAddress, final PayPalAddressData paypalAddress)
			throws ConversionException
	{
		String recipientName = StringUtils.stripToEmpty(hybrisAddress.getFirstname()) + " " + StringUtils.stripToEmpty(hybrisAddress.getLastname());
		if (hybrisAddress.getTitle() != null){
			recipientName = hybrisAddress.getTitle().getName() + " " + recipientName;
		}

		paypalAddress.setLocality(hybrisAddress.getTown());
		paypalAddress.setPhone(checkIfPhoneNumberIsValid(hybrisAddress.getPhone1()) ? hybrisAddress.getPhone1() : StringUtils.EMPTY);
		paypalAddress.setPostalCode(hybrisAddress.getPostalcode());
		paypalAddress.setRecipientName(recipientName);
		paypalAddress.setStreetAddress(hybrisAddress.getLine1());
		paypalAddress.setExtendedAddress(hybrisAddress.getLine2());
		paypalAddress.setType("Personal");
		if (hybrisAddress.getRegion() != null)
		{
			paypalAddress.setRegion(hybrisAddress.getRegion().getIsocodeShort());
		}
		if (hybrisAddress.getCountry() != null)
		{
			paypalAddress.setCountryCodeAlpha2(hybrisAddress.getCountry().getIsocode());
		}
		return paypalAddress;
	}

	public AddressData convert(final PayPalAddressData address)
	{
		final AddressData addressData = new AddressData();
		final boolean isCountryIsoCodeValid = payPalAddressDetailValidator.validatePayPalCountryCode(address.getCountryCode());
		final boolean isRegionIsoCodeValid = payPalAddressDetailValidator.validatePayPalRegionCode(address.getCountryCode(), address.getState());
		if (isCountryIsoCodeValid)
		{
			final CountryData country = new CountryData();
			country.setIsocode(address.getCountryCode());
			addressData.setCountry(country);
		}

		if (isRegionIsoCodeValid)
		{
			final RegionData region = new RegionData();
			region.setIsocode(address.getCountryCode() + "-" + address.getState());
			addressData.setRegion(region);
		}

		addressData.setLine1(address.getLine1());
		addressData.setLine2(address.getLine2());
		addressData.setPhone(address.getPhone());
		addressData.setPostalCode(address.getPostalCode());
		addressData.setFirstName(address.getRecipientName());
		addressData.setLastName(EMPTY_STRING);
		addressData.setTown(address.getCity());

		return addressData;
	}
	
	/**
	 * Check if phone number has valid pattern.
	 *
	 * @param phoneNumber the phone number
	 * @return true, if successful
	 */
	private boolean checkIfPhoneNumberIsValid(final String phoneNumber) {
	  if(StringUtils.isNotBlank(phoneNumber)) {
	    final Pattern regexPattern = Pattern.compile(BlCoreConstants.PHONE_REGEX_PATTERN);
	    final Matcher matchPatter = regexPattern.matcher(phoneNumber);
	    return matchPatter.matches();
	  }
	  return Boolean.FALSE;
	}
	


	public PayPalAddressDetailValidator getPayPalAddressDetailValidator()
	{
		return payPalAddressDetailValidator;
	}


	public void setPayPalAddressDetailValidator(final PayPalAddressDetailValidator payPalAddressDetailValidator)
	{
		this.payPalAddressDetailValidator = payPalAddressDetailValidator;
	}

}
