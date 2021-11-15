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
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.commands.request.AuthorizationRequest;
import de.hybris.platform.payment.commands.request.CreateSubscriptionRequest;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.commands.result.AuthorizationResult;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.payment.methods.PaymentMethod;


public interface BrainTreePaymentService extends PaymentMethod
{

	SubscriptionResult createCustomerSubscription(final CreateSubscriptionRequest subscriptionRequest);

	BrainTreeGenerateClientTokenResult generateClientToken(final BrainTreeGenerateClientTokenRequest clientTokenRequest);

	BrainTreeFindCustomerResult findCustomer(final BrainTreeCustomerRequest findCustomerRequest);

	String generateClientToken();

	AuthorizationResult authorize(AuthorizationRequest authorizationRequest);

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
	 * @param customer
	 * @param paymentInfoId
	 * @return
	 */
	BrainTreePaymentInfoModel completeCreateSubscription(CustomerModel customer, String paymentInfoId);

	String generateClientToken(final String site, final String currency);

	/**
	 * @param request
	 * @return
	 */
	BrainTreeCreatePaymentMethodResult createPaymentMethod(BrainTreeCreatePaymentMethodRequest request);

	/**
	 * @param request
	 * @return
	 */
	BrainTreePaymentMethodResult createCreditCardPaymentMethod(BrainTreeCreateCreditCardPaymentMethodRequest request);

	/**
	 * @param request
	 * @return
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
	 * @param addressRequest
	 * @return
	 */
	BrainTreeAddressResult createAddress(BrainTreeAddressRequest addressRequest, CustomerModel customer);

	/**
	 *
	 * @param addressRequest
	 */
	BrainTreeAddressResult updateAddress(BrainTreeAddressRequest addressRequest);

	/**
	 * @param addressRequest
	 */
	BrainTreeAddressResult removeAddress(BrainTreeAddressRequest addressRequest);

	BrainTreeCreatePaymentMethodResult createPaymentMethodForCustomer(final CustomerModel customer,
			final AddressModel billingAddress, final BraintreeInfo braintreeInfo);

	BrainTreeSaleTransactionResult partialCaptureTransaction(BrainTreeSaleTransactionRequest request);

	String createCustomer(final CustomerModel customer, final AddressModel billingAddress);

	void updatePaymentInfo(final PaymentInfoModel paymentInfo, final CreditCard result);

	void updatePaymentInfo(final PaymentInfoModel paymentInfo, final AndroidPayDetails result);

	void updatePaymentInfo(final PaymentInfoModel paymentInfo, final PayPalAccount result);

	WebhookNotification getWebhookNotification(BrainTreeWebhookNotificationRequest webhookNotificationRequest);

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

	/**
	 * This method is used to update payment info
	 * @param customer For the current customer
	 * @param paymentInfoId for the current order
	 * @return payment info
	 */
	BrainTreePaymentInfoModel completeCreateSubscriptionForModifyPayment(final CustomerModel customer, final String paymentInfoId,final AbstractOrderModel order);
	
	/**
	 * It fetches the payment info model by payment info id to make deposit.
	 *
	 * @param customer the customer
	 * @param paymentInfoId the payment info id
	 * @param nonce the nonce
	 * @param depositAmount the deposit amount
	 * @return BrainTreePaymentInfoModel
	 */
  public BrainTreePaymentInfoModel getBrainTreePaymentInfoForCodeToDeposit(final CustomerModel customer, final String
      paymentInfoId, final String nonce, final Double depositAmount);
  
  /**
   * Gets the cloned payment info for code.
   *
   * @param customer the customer
   * @param paymentInfoId the payment info id
   * @param nonce the nonce
   * @return the cloned payment info for code
   */
  public BrainTreePaymentInfoModel getModifyOrderPaymentInfoForCode(final CustomerModel customer, final String paymentInfoId,
      final String nonce, final Double newAmount);

	/**
	 * Gets the cloned payment info model.
	 *
	 * @param customer the customer
	 * @param paymentInfoId the payment info id
	 * @param nonce the nonce
	 * @return the cloned payment info for code
	 */
	public BrainTreePaymentInfoModel getClonedPaymentInfoForCode(final CustomerModel customer, final String paymentInfoId,
			final String nonce);
}
