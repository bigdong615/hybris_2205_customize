package com.braintree.transaction.service;

import com.braintree.command.result.BrainTreeCreatePaymentMethodResult;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.command.result.BrainTreeSaleTransactionResult;
import com.braintree.command.result.BrainTreeSubmitForSettlementTransactionResult;
import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.hybris.data.BraintreeTransactionEntryData;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.dto.BraintreeInfo;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;

import java.math.BigDecimal;
import java.util.Map;


public interface BrainTreeTransactionService
{
	boolean createAuthorizationTransaction();

	boolean createAuthorizationTransaction(Map<String, String> customFields);

	boolean createPaymentMethodTokenForOrderReplenishment();

	PaymentTransactionEntryModel createCancelTransaction(PaymentTransactionModel transaction, BrainTreeVoidResult voidResult);

	PaymentTransactionEntryModel createCancelTransaction(PaymentTransactionModel transaction,
			BraintreeTransactionEntryData brainTreeTransaction);

	PaymentTransactionEntryModel createRefundTransaction(PaymentTransactionModel transaction,
			BrainTreeRefundTransactionResult result);

	PaymentTransactionEntryModel createPartialRefundTransaction(PaymentTransactionModel transaction,
			BrainTreeRefundTransactionResult result);

	PaymentTransactionEntryModel createPartialCaptureTransaction(PaymentTransactionModel transaction,
			BrainTreeSaleTransactionResult result);

	PaymentTransactionEntryModel createSubmitForSettlementTransaction(PaymentTransactionModel transaction,
			BrainTreeSubmitForSettlementTransactionResult result);

	PaymentTransactionEntryModel createAuthorizationTransaction(AbstractOrderModel cart);

	PaymentTransactionEntryModel createAuthorizationTransaction(final AbstractOrderModel cart, Map<String, String> customFields);

	PaymentTransactionEntryModel createAuthorizationTransaction(final AbstractOrderModel cart, Map<String, String> customFields,
			BigDecimal totalAmount);

	PaymentTransactionEntryModel createAuthorizationTransaction(final OrderModel cart, BigDecimal totalAmount);

	BrainTreePaymentInfoModel createSubscription(final AddressModel billingAddress, final CustomerModel customer,
			final BraintreeInfo braintreeInfo);

	BrainTreePaymentInfoModel createSubscription(final AddressModel billingAddress, final CustomerModel customer,
			final BraintreeInfo braintreeInfo, AbstractOrderModel abstractOrderModel);

	void createOrderTransaction(AbstractOrderModel cart, BrainTreeCreatePaymentMethodResult result);

	/**
	 * To create the auth transaction of the order
	 * @param orderModel the order code
	 * @param amountToAuthorize the amount to authorize
	 * @param submitForSettlement submit for settlement flag
	 * @param paymentInfo the payment info model
	 * @return  boolean
	 */
	boolean createAuthorizationTransactionOfOrder(final AbstractOrderModel orderModel, final BigDecimal
			amountToAuthorize, final boolean submitForSettlement, final BrainTreePaymentInfoModel paymentInfo);

	/**
	 * To create the capture transaction of the order
	 * @param orderModel the order model
	 * @param amount the amount
	 * @param requestId the request id
	 * @return  boolean
	 * @throws BraintreeErrorException braintree error exception
	 */
	boolean captureAuthorizationTransaction(final OrderModel orderModel, final BigDecimal amount,
			final String requestId) throws BraintreeErrorException;
}
