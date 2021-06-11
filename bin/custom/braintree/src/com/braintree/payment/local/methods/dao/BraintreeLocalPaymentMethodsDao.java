package com.braintree.payment.local.methods.dao;

import com.braintree.model.BraintreeLocalPaymentMethodsModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;

import java.util.List;


public interface BraintreeLocalPaymentMethodsDao
{
	List<BraintreeLocalPaymentMethodsModel> getAll();

	OrderModel getOrderByPaymentId(String paymentId);


	CartModel getCarByPaymentId(String paymentId);
}
