package com.braintree.payment.info.dao;

import com.braintree.model.BrainTreePaymentInfoModel;


public interface BrainTreePaymentInfoDao
{
	/**
	 * find paymentInfo by customerId and paymentMethodToken
	 */
	BrainTreePaymentInfoModel find(String customerId, String paymentMethodToken);

}
