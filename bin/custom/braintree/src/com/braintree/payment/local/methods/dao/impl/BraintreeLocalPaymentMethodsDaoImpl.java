package com.braintree.payment.local.methods.dao.impl;

import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.enums.BrainTreePaymentMethod;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.model.BraintreeLocalPaymentMethodsModel;
import com.braintree.payment.local.methods.dao.BraintreeLocalPaymentMethodsDao;
import de.hybris.platform.commerceservices.order.dao.impl.DefaultCommerceCartDao;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.jalo.order.Order;
import de.hybris.platform.servicelayer.search.FlexibleSearchQuery;
import de.hybris.platform.servicelayer.search.FlexibleSearchService;
import de.hybris.platform.servicelayer.search.SearchResult;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class BraintreeLocalPaymentMethodsDaoImpl extends DefaultCommerceCartDao implements BraintreeLocalPaymentMethodsDao
{
	private final String selectAllQuery = "SELECT {" + BraintreeLocalPaymentMethodsModel.PK + "} FROM {" + BraintreeLocalPaymentMethodsModel._TYPECODE
			+ "} WHERE {" + BraintreeLocalPaymentMethodsModel.ISENABLED + "} = ?isEnabled" ;

	protected static final String GET_ORDER_BY_PAYMENT_ID = "SELECT {order:" + OrderModel.PK + "} FROM {"
			+ OrderModel._TYPECODE + " as order JOIN " + BrainTreePaymentInfoModel._TYPECODE +
			" as brainTreePaymentInfo" + " ON {order:" + OrderModel.PAYMENTINFO + "} = {brainTreePaymentInfo:" +
			BrainTreePaymentInfoModel.PK + "}} " + "WHERE {brainTreePaymentInfo:" +
			BrainTreePaymentInfoModel.NONCE+ "} = ?nonce";

	protected static final String GET_CART_BY_PAYMENT_ID = "SELECT {cart:" + CartModel.PK + "} FROM {"
			+ CartModel._TYPECODE + " as cart JOIN " + BrainTreePaymentInfoModel._TYPECODE +
			" as brainTreePaymentInfo" + " ON {cart:" + CartModel.PAYMENTINFO + "}={brainTreePaymentInfo:" +
			BrainTreePaymentInfoModel.PK + "}} " + "WHERE {brainTreePaymentInfo:" +
			BrainTreePaymentInfoModel.PAYMENTID+ "} = ?paymentId AND {brainTreePaymentInfo:" +
			BrainTreePaymentInfoModel.PAYMENTPROVIDER + "} = ?paymentProvider";

	@Override
	public List<BraintreeLocalPaymentMethodsModel> getAll()
	{
		final FlexibleSearchQuery fQuery = new FlexibleSearchQuery(selectAllQuery);

		fQuery.addQueryParameter("isEnabled",Boolean.TRUE);
		SearchResult<BraintreeLocalPaymentMethodsModel> searchResult = search(fQuery);
		return searchResult.getResult();
	}

	@Override
	public OrderModel getOrderByPaymentId(String paymentId)
	{
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("nonce", paymentId);
		SearchResult<OrderModel> searchResult = search(createSearchQuery(GET_ORDER_BY_PAYMENT_ID, params, OrderModel.class));
		List<OrderModel> models = searchResult.getResult();

		return models.stream().findFirst().orElse(null);
	}

	@Override public CartModel getCarByPaymentId(String paymentId)
	{
		final Map<String, Object> params = new HashMap<String, Object>();
		params.put("paymentId", paymentId);
		params.put("paymentProvider", BrainTreePaymentMethod.LOCALPAYMENT.toString());
		final SearchResult<CartModel> searchResult = search(createSearchQuery(GET_CART_BY_PAYMENT_ID, params, CartModel.class));
		final List<CartModel> carts = searchResult.getResult();

		return carts.stream().findFirst().orElse(null);
	}
}
