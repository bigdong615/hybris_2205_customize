package com.braintree.payment.local.methods.service;

import com.braintree.model.BraintreeLocalPaymentMethodsModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;

import java.util.List;


public interface BraintreeLocalPaymentMethodsService
{
	List<BraintreeLocalPaymentMethodsModel> getAllLocalPaymentMethods();

	OrderModel getOrderByPaymentId(String paymentId);

	CartModel getCartByPaymentId(String paymentId);
}
