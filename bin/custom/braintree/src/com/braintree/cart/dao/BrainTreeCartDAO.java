package com.braintree.cart.dao;

import de.hybris.platform.core.model.order.CartModel;


public interface BrainTreeCartDAO
{
	CartModel getCartForPaymentId(String paymentId);
}
