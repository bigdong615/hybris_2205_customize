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

/**
 * Service for creating transactions
 */
public interface BrainTreeTransactionService
{

	/**
	 * It creates authorization transaction
	 * @return boolean
	 */
	boolean createAuthorizationTransaction();

	/**
	 * It creates authorization transaction
	 * @param customFields the custom fields
	 * @return boolean
	 */
	boolean createAuthorizationTransaction(Map<String, String> customFields);

	/**
	 * It creates the payment method
	 * @return boolean
	 */
	boolean createPaymentMethodTokenForOrderReplenishment();

	/**
	 * It creates the cancel transaction
	 * @param transaction the transaction
	 * @param voidResult the void result
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createCancelTransaction(PaymentTransactionModel transaction, BrainTreeVoidResult voidResult);

	/**
	 * It creates the cancel transaction
	 * @param transaction the transaction
	 * @param brainTreeTransaction the braintree transaction data
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createCancelTransaction(PaymentTransactionModel transaction,
			BraintreeTransactionEntryData brainTreeTransaction);

	/**
	 * It creates the refund transaction
	 * @param transaction the transaction
	 * @param result the transaction result
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createRefundTransaction(PaymentTransactionModel transaction,
			BrainTreeRefundTransactionResult result);

	/**
	 * It creates the partial capture transaction
	 * @param transaction the transaction
	 * @param result the transaction result
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createPartialRefundTransaction(PaymentTransactionModel transaction,
			BrainTreeRefundTransactionResult result);

	/**
	 * It creates the partial capture transaction
	 * @param transaction the transaction
	 * @param result the transaction result
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createPartialCaptureTransaction(PaymentTransactionModel transaction,
			BrainTreeSaleTransactionResult result);

	/**
	 * It creates the capture transaction
	 * @param transaction the transaction
	 * @param result the transaction result
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createSubmitForSettlementTransaction(PaymentTransactionModel transaction,
			BrainTreeSubmitForSettlementTransactionResult result);

	/**
	 * It creates the authorization transaction
	 * @param cart the cart
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createAuthorizationTransaction(AbstractOrderModel cart);

	/**
	 * It creates the authorization transaction
	 * @param cart the cart
	 * @param customFields the custom fields
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createAuthorizationTransaction(final AbstractOrderModel cart, Map<String, String> customFields);

	/**
	 * It creates the authorization transaction
	 * @param cart the cart
	 * @param customFields the custom fields
	 * @param totalAmount the amount
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createAuthorizationTransaction(final AbstractOrderModel cart, Map<String, String> customFields,
			BigDecimal totalAmount);

	/**
	 * It creates the authorization transaction
	 * @param cart the cart
	 * @param totalAmount the total amount
	 * @return transaction entry
	 */
	PaymentTransactionEntryModel createAuthorizationTransaction(final OrderModel cart, BigDecimal totalAmount);

	/**
	 * It creates subscription i.e it verifies and stores the payment details
	 * @param billingAddress the billing address
	 * @param customer the customer
	 * @param braintreeInfo the braintree info
	 * @return payment info model
	 */
	BrainTreePaymentInfoModel createSubscription(final AddressModel billingAddress, final CustomerModel customer,
			final BraintreeInfo braintreeInfo);

	/**
	 * It creates subscription i.e it verifies and stores the payment details
	 * @param billingAddress the billing address
	 * @param customer the customer
	 * @param braintreeInfo the braintree info
	 * @param abstractOrderModel the order
	 * @return payment info model
	 */
	BrainTreePaymentInfoModel createSubscription(final AddressModel billingAddress, final CustomerModel customer,
			final BraintreeInfo braintreeInfo, AbstractOrderModel abstractOrderModel);

	/**
	 * It creates order transaction
	 * @param cart the cart
	 * @param result create payment method result
	 */
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
