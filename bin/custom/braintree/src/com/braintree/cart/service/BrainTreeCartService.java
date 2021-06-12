package com.braintree.cart.service;

import de.hybris.platform.core.model.order.CartModel;


public interface BrainTreeCartService
{
	CartModel getCartForPaymentId(String paymentId);
}
