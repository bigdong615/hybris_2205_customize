package com.braintree.order.capture.partial.process;


import de.hybris.platform.core.model.order.OrderModel;


public interface OrderCompleteProcessService
{

	void startOrderCompletionProcess(final OrderModel order);
}
