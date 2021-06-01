package com.braintree.order.submitForSettlement.service;

import com.braintree.exceptions.BraintreeErrorException;
import de.hybris.platform.core.model.order.OrderModel;

import java.math.BigDecimal;


public interface BraintreeSubmitForSettlementService
{

	boolean submitForSettlement(OrderModel orderModel, BigDecimal amount, String authorizeTransactionID)
			throws BraintreeErrorException;

	boolean isSubmitForSettlementAvailable(OrderModel orderModel);
}
