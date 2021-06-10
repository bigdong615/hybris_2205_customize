package com.braintree.payment.local.methods.service.impl;

import com.braintree.model.BraintreeLocalPaymentMethodsModel;
import com.braintree.payment.local.methods.dao.BraintreeLocalPaymentMethodsDao;
import com.braintree.payment.local.methods.service.BraintreeLocalPaymentMethodsService;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;

import java.util.List;


public class BraintreeLocalLocalPaymentMethodsServiceImpl implements BraintreeLocalPaymentMethodsService
{

	private BraintreeLocalPaymentMethodsDao braintreeLocalPaymentMethodsDao;

	@Override
	public List<BraintreeLocalPaymentMethodsModel> getAllLocalPaymentMethods()
	{
		return getBraintreeLocalPaymentMethodsDao().getAll();
	}

	@Override
	public OrderModel getOrderByPaymentId(String paymentId)
	{
		return getBraintreeLocalPaymentMethodsDao().getOrderByPaymentId(paymentId);
	}

	@Override public CartModel getCartByPaymentId(String paymentId){
		return getBraintreeLocalPaymentMethodsDao().getCarByPaymentId(paymentId);
	}

	public BraintreeLocalPaymentMethodsDao getBraintreeLocalPaymentMethodsDao()
	{
		return braintreeLocalPaymentMethodsDao;
	}

	public void setBraintreeLocalPaymentMethodsDao(
			BraintreeLocalPaymentMethodsDao braintreeLocalPaymentMethodsDao)
	{
		this.braintreeLocalPaymentMethodsDao = braintreeLocalPaymentMethodsDao;
	}

}
