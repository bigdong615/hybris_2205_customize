package com.braintree.cart.service.impl;

import com.braintree.cart.dao.BrainTreeCartDAO;
import com.braintree.cart.service.BrainTreeCartService;
import de.hybris.platform.core.model.order.CartModel;

import javax.annotation.Resource;


public class BrainTreeCartServiceImpl implements BrainTreeCartService
{

	@Resource(name = "brainTreeCartDao")
	private BrainTreeCartDAO brainTreeCartDao;

	@Override
	public CartModel getCartForPaymentId(String paymentId){
		return brainTreeCartDao.getCartForPaymentId(paymentId);
	}

	public BrainTreeCartDAO getBrainTreeCartDao()
	{
		return brainTreeCartDao;
	}

	public void setBrainTreeCartDao(BrainTreeCartDAO brainTreeCartDao)
	{
		this.brainTreeCartDao = brainTreeCartDao;
	}
}
