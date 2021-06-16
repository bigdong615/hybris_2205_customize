/**
 *
 */
package com.braintree.paypal.converters.impl;

import static com.braintree.constants.BraintreeConstants.PAYPAL_PAYMENT;
import static com.braintree.constants.BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT;

import com.braintree.constants.BraintreeConstants;
import de.hybris.platform.servicelayer.dto.converter.ConversionException;
import de.hybris.platform.servicelayer.dto.converter.Converter;

import org.apache.commons.lang.StringUtils;

import com.braintree.hybris.data.BrainTreePaymentInfoData;
import com.braintree.model.BrainTreePaymentInfoModel;


public class BrainTreePaymentInfoDataConverter implements Converter<BrainTreePaymentInfoModel, BrainTreePaymentInfoData>
{

	@Override
	public BrainTreePaymentInfoData convert(final BrainTreePaymentInfoModel paymentInfo) throws ConversionException
	{
		final BrainTreePaymentInfoData paymentData = new BrainTreePaymentInfoData();
		return convert(paymentInfo, paymentData);
	}

	@Override
	public BrainTreePaymentInfoData convert(final BrainTreePaymentInfoModel paymentInfo,
			final BrainTreePaymentInfoData paymentData) throws ConversionException
	{
		paymentData.setPaymentType(paymentInfo.getPaymentProvider());
		paymentData.setCardNumber(paymentInfo.getCardNumber());
		paymentData.setEmail(paymentInfo.getPayer());
		if (paymentInfo.getCardType() != null)
		{
			paymentData.setCardType(paymentInfo.getCardType().getCode());
		}
		if (PAYPAL_PAYMENT.equals(paymentInfo.getPaymentProvider()) && StringUtils.isNotBlank(paymentInfo.getPaymentInfo()))
		{
			paymentData.setEmail(paymentInfo.getPaymentInfo());
		}
		return paymentData;
	}

}
