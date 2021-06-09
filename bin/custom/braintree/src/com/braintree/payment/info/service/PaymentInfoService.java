package com.braintree.payment.info.service;

import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.commercefacades.user.data.AddressData;


public interface PaymentInfoService
{
	/**
	 * remove paymentInfo by customerId and paymentMethodToken
	 */
	void remove(String customerId, String paymentMethodToken);

	/**
	 * add paymentInfo to customer
	 */
	void addToCustomer(BrainTreePaymentInfoModel paymentInfo);

	/**
	 * update customer paymentInfo
	 */
	void update(String paymentMethodToken, BrainTreePaymentInfoModel paymentInfo);

	/**
	 * update customer paymentInfo and address
	 */
	void update(BrainTreePaymentInfoModel braintreePaymentInfo, AddressData addressData);
}
