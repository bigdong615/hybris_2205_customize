/**
 *
 */
package com.braintree.order.service;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;


public interface OrderRetrievalService
{
	String getOrderCodeForTransaction(final String transactionID);

	PaymentTransactionModel getTransactionForId(final String transactionID);

	AbstractOrderModel getOrderForTransaction(final String transactionID);
}
