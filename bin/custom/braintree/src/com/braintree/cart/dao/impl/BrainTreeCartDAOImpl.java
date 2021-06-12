package com.braintree.cart.dao.impl;

import com.braintree.cart.dao.BrainTreeCartDAO;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.commerceservices.order.dao.impl.DefaultCommerceCartDao;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class BrainTreeCartDAOImpl extends DefaultCommerceCartDao implements BrainTreeCartDAO
{
	protected static final String GET_CART_BY_PAYMENT_ID = "SELECT {cart:" + CartModel.PK + "} FROM {"
			+ CartModel._TYPECODE + " as cart JOIN " + BrainTreePaymentInfoModel._TYPECODE +
			" as brainTreePaymentInfo" + " ON {cart:" + CartModel.PAYMENTINFO + "}={brainTreePaymentInfo:" +
			BrainTreePaymentInfoModel.PK + "}} " + "WHERE {brainTreePaymentInfo:" +
			BrainTreePaymentInfoModel.PAYMENTINFO+ "} = ?paymentId";

	@Override public CartModel getCartForPaymentId(String paymentId)
	{
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("paymentId", paymentId);
		final SearchResult<CartModel> searchResult = search(createSearchQuery(GET_CART_BY_PAYMENT_ID, params, CartModel.class));
		final List<CartModel> carts = searchResult.getResult();

		return carts.stream().findFirst().orElse(null);
	}
}
