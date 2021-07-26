/**
 *
 */
package com.braintree.method;

import com.braintree.command.request.BrainTreeAddressRequest;
import com.braintree.command.request.BrainTreeAuthorizationRequest;
import com.braintree.command.request.BrainTreeCloneTransactionRequest;
import com.braintree.command.request.BrainTreeCreateCreditCardPaymentMethodRequest;
import com.braintree.command.request.BrainTreeCreatePaymentMethodRequest;
import com.braintree.command.request.BrainTreeCustomerRequest;
import com.braintree.command.request.BrainTreeDeletePaymentMethodRequest;
import com.braintree.command.request.BrainTreeFindMerchantAccountRequest;
import com.braintree.command.request.BrainTreeGenerateClientTokenRequest;
import com.braintree.command.request.BrainTreeRefundTransactionRequest;
import com.braintree.command.request.BrainTreeSaleTransactionRequest;
import com.braintree.command.request.BrainTreeSubmitForSettlementTransactionRequest;
import com.braintree.command.request.BrainTreeUpdateCustomerRequest;
import com.braintree.command.request.BrainTreeUpdatePaymentMethodRequest;
import com.braintree.command.result.BrainTreeAddressResult;
import com.braintree.command.result.BrainTreeCloneTransactionResult;
import com.braintree.command.result.BrainTreeCreatePaymentMethodResult;
import com.braintree.command.result.BrainTreeCustomerResult;
import com.braintree.command.result.BrainTreeFindCustomerResult;
import com.braintree.command.result.BrainTreeFindMerchantAccountResult;
import com.braintree.command.result.BrainTreeGenerateClientTokenResult;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.command.result.BrainTreeSaleTransactionResult;
import com.braintree.command.result.BrainTreeSubmitForSettlementTransactionResult;
import com.braintree.command.result.BrainTreeUpdateCustomerResult;
import com.braintree.command.result.BrainTreeUpdatePaymentMethodResult;
import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.dto.BraintreeInfo;
import com.braintreegateway.AndroidPayDetails;
import com.braintreegateway.CreditCard;
import com.braintreegateway.PayPalAccount;
import com.braintreegateway.WebhookNotification;
import de.hybris.platform.braintree.data.BrainTreeWebhookNotificationRequest;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.commands.request.AuthorizationRequest;
import de.hybris.platform.payment.commands.request.CreateSubscriptionRequest;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.commands.result.AuthorizationResult;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.payment.methods.PaymentMethod;

/**
 * Service class for Braintree Payment
 */
public interface BrainTreePaymentService extends PaymentMethod
{

	/**
	 * It creates customer subscription
	 * @param subscriptionRequest subscription request
	 * @return subscription result
	 */
	SubscriptionResult createCustomerSubscription(final CreateSubscriptionRequest subscriptionRequest);

	/**
	 * It generates client token
	 * @param clientTokenRequest client token request
	 * @return BrainTreeGenerateClientTokenResult
	 */
	BrainTreeGenerateClientTokenResult generateClientToken(final BrainTreeGenerateClientTokenRequest clientTokenRequest);

	/**
	 * It finds the customer in Braintree
	 * @param findCustomerRequest find Customer Request
	 * @return customer result
	 */
	BrainTreeFindCustomerResult findCustomer(final BrainTreeCustomerRequest findCustomerRequest);

	/**
	 * It generates client token
	 * @return token
	 */
	String generateClientToken();

	/**
	 * It authorize the payment
	 * @param authorizationRequest authorization request
	 * @return AuthorizationResult authorization result
	 */
	AuthorizationResult authorize(AuthorizationRequest authorizationRequest);

	/**
	 * It authorize the payment
	 * @param authorizationRequest authorization request
	 * @param customer the customer
	 * @return AuthorizationResult authorization result
	 */
	AuthorizationResult authorize(BrainTreeAuthorizationRequest authorizationRequest, CustomerModel customer);

	/**
	 * void transaction on braintree side
	 *
	 * @param voidRequest
	 *           request
	 * @return result of void operation
	 */
	BrainTreeVoidResult voidTransaction(VoidRequest voidRequest);

	/**
	 * clone transaction on braintree side
	 *
	 * @param request
	 *           simple request with transaction id, amount and submitForSettlement sign
	 * @return result of clone operation
	 */
	BrainTreeCloneTransactionResult cloneTransaction(BrainTreeCloneTransactionRequest request);

	/**
	 * It creates subscription i.e. verify and stores the payment details
	 * @param customer the customer
	 * @param paymentInfoId the payment info id
	 * @return payment info model
	 */
	BrainTreePaymentInfoModel completeCreateSubscription(CustomerModel customer, String paymentInfoId);

	/**
	 * It generates client token
	 * @param site the site
	 * @param currency the currency
	 * @return token
	 */
	String generateClientToken(final String site, final String currency);

	/**
	 * It creates payment method
	 * @param request the request
	 * @return create payment method result
	 */
	BrainTreeCreatePaymentMethodResult createPaymentMethod(BrainTreeCreatePaymentMethodRequest request);

	/**
	 * It creates payment method for credit card
	 * @param request the request
	 * @return payment method result
	 */
	BrainTreePaymentMethodResult createCreditCardPaymentMethod(BrainTreeCreateCreditCardPaymentMethodRequest request);

	/**
	 * It creates the nonce
	 * @param request the request
	 * @return nonce
	 */
	String createPaymentMethodNonce(String request);


	/**
	 * refund transaction with specific amount
	 *
	 * @param request
	 *           wih transaction id and amount
	 * @return result of refund operation
	 */
	BrainTreeRefundTransactionResult refundTransaction(BrainTreeRefundTransactionRequest request);

	/**
	 * update braintree customer
	 *
	 * @param request
	 *           wih updated customer fields
	 * @return result of update operation
	 */
	BrainTreeUpdateCustomerResult updateCustomer(BrainTreeUpdateCustomerRequest request);

	/**
	 * It finds the merchant account in bBraintree
	 * @param brainTreeFindMerchantAccountRequest
	 *           BrainTreeFindMerchantAccountRequest
	 * @return BrainTreeFindMerchantAccountResult
	 */
	BrainTreeFindMerchantAccountResult findMerchantAccount(
			final BrainTreeFindMerchantAccountRequest brainTreeFindMerchantAccountRequest);

	/**
	 * create new transaction
	 *
	 * @param request
	 *           BrainTreeSaleTransactionRequest
	 * @return BrainTreeSaleTransactionResult
	 */
	BrainTreeSaleTransactionResult saleTransaction(BrainTreeSaleTransactionRequest request);

	/**
	 * It deles the payment method
	 * @param request the request
	 * @return payment method result
	 */
	BrainTreePaymentMethodResult deletePaymentMethod(BrainTreeDeletePaymentMethodRequest request);

	/**
	 * remove customer from vault by customer id
	 *
	 * @param request
	 *           BrainTreeCustomerRequest
	 * @return BrainTreeCustomerResult
	 */
	BrainTreeCustomerResult removeCustomer(BrainTreeCustomerRequest request);

	/**
	 * update payment method
	 *
	 * @param request
	 *           BrainTreeUpdatePaymentMethodRequest
	 * @return BrainTreeUpdatePaymentMethodResult
	 */
	BrainTreeUpdatePaymentMethodResult updatePaymentMethod(BrainTreeUpdatePaymentMethodRequest request);


	/**
	 * Submit For Settlement transaction with specific amount
	 *
	 * @param request
	 *           wih transaction id and amount
	 * @return result of submit operation
	 */
	BrainTreeSubmitForSettlementTransactionResult submitForSettlementTransaction(
			BrainTreeSubmitForSettlementTransactionRequest request);

	/**
	 * It creates address
	 * @param customer the customer
	 * @param addressRequest the address request
	 * @return address result
	 */
	BrainTreeAddressResult createAddress(BrainTreeAddressRequest addressRequest, CustomerModel customer);

	/**
	 * It updates addressb
	 * @param addressRequest the address request
	 * @return address result
	 */
	BrainTreeAddressResult updateAddress(BrainTreeAddressRequest addressRequest);

	/**
	 * It removes address
	 * @param addressRequest the address request
	 * @return address result
	 */
	BrainTreeAddressResult removeAddress(BrainTreeAddressRequest addressRequest);

	/**
	 * It creates the payment method
	 * @param customer the customer
	 * @param billingAddress the billing address
	 * @param braintreeInfo the braintree info
	 * @return payment method result
	 */
	BrainTreeCreatePaymentMethodResult createPaymentMethodForCustomer(final CustomerModel customer,
			final AddressModel billingAddress, final BraintreeInfo braintreeInfo);

	/**
	 * It creates partial capture transaction
	 * @param request the request
	 * @return transaction result
	 */
	BrainTreeSaleTransactionResult partialCaptureTransaction(BrainTreeSaleTransactionRequest request);

	/**
	 * It creates customer
	 * @param customer the customer
	 * @param billingAddress the billing address
	 * @return customer id
	 */
	String createCustomer(final CustomerModel customer, final AddressModel billingAddress);

	/**
	 * It updates payment info
	 * @param paymentInfo the payment info
	 * @param result the result
	 */
	void updatePaymentInfo(final PaymentInfoModel paymentInfo, final CreditCard result);

	/**
	 * It updates payment info
	 * @param paymentInfo the payment info
	 * @param result the result
	 */
	void updatePaymentInfo(final PaymentInfoModel paymentInfo, final AndroidPayDetails result);

	/**
	 * It updates payment info
	 * @param paymentInfo the payment info
	 * @param result the result
	 */
	void updatePaymentInfo(final PaymentInfoModel paymentInfo, final PayPalAccount result);

	/**
	 * It gets webhook notification
	 * @param webhookNotificationRequest webhook notification request
	 * @return the webhook notification
	 */
	WebhookNotification getWebhookNotification(BrainTreeWebhookNotificationRequest webhookNotificationRequest);

	/**
	 * It fetches the payment method by token
	 * @param paymentMethodToken payment method token
	 * @return paypal account
	 */
	PayPalAccount getPaymentMethodFromBTByToken(final String paymentMethodToken);

	/**
	 * It fetches the payment info model by payment info id
	 * @param customer the customer
	 * @param paymentInfoId the payment info id
	 * @param nonce the nonce
	 * @return BrainTreePaymentInfoModel
	 */
	public BrainTreePaymentInfoModel getBrainTreePaymentInfoForCode(final CustomerModel customer, final String
			paymentInfoId, final String nonce);

	/**
	 * It creates payment method
	 */
	public void createPaymentMethodTokenForOrderReplenishment();
}
