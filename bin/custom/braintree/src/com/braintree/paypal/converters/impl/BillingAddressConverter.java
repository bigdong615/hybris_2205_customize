package com.braintree.paypal.converters.impl;

import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;


public class BillingAddressConverter implements Converter<AddressModel, BillingInfo>
{

	@Override
	public BillingInfo convert(final AddressModel address) throws ConversionException
	{
		final BillingInfo billingInfo = new BillingInfo();
		return convert(address, billingInfo);
	}

	@Override
	public BillingInfo convert(final AddressModel address, final BillingInfo billingInfo) throws ConversionException
	{
		if (address != null)
		{
			billingInfo.setFirstName(address.getFirstname());
			billingInfo.setLastName(address.getLastname());
			billingInfo.setStreet1(address.getStreetname());
			billingInfo.setStreet2(address.getStreetnumber());
			billingInfo.setCity(address.getTown());
			if (address.getRegion() != null)
			{
				billingInfo.setState(address.getRegion().getIsocodeShort());
			}
			billingInfo.setPostalCode(address.getPostalcode());
			if (address.getCountry() != null)
			{
				billingInfo.setCountry(address.getCountry().getIsocode());
			}
			if (address.getOwner() instanceof CustomerModel)
			{
				billingInfo.setEmail(((CustomerModel) address.getOwner()).getContactEmail());
			}
			else
			{
				billingInfo.setEmail(address.getEmail());
			}
		}

		return billingInfo;
	}

}
