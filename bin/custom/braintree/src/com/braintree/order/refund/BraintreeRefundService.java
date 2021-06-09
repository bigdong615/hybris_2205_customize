package com.braintree.order.refund;

import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.refund.RefundService;
import de.hybris.platform.returns.model.ReturnRequestModel;


public interface BraintreeRefundService extends RefundService
{
	void applyBraintreeRefund(OrderModel refundOrder, OrderModel originalOrder, ReturnRequestModel returnRequestModel);

	void applyBraintreeRefund(ReturnRequestModel returnRequestModel, Double refundAmount);
}
