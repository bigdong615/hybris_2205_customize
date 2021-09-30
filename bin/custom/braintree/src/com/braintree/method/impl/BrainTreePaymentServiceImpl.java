package com.braintree.method.impl;

import static com.braintree.constants.BraintreeConstants.BRAINTREE_AUTHENTICATION_TOKEN;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PROVIDER_NAME;
import static com.braintree.constants.BraintreeConstants.CARD_NUMBER_MASK;

import com.bl.logging.BlLogger;
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
import com.braintree.commands.BrainTreeCloneCommand;
import com.braintree.commands.BrainTreeCreateAddressCommand;
import com.braintree.commands.BrainTreeCreateCreditCardPaymentMethodCommand;
import com.braintree.commands.BrainTreeCreatePaymentMethodCommand;
import com.braintree.commands.BrainTreeCreatePaymentMethodNonceCommand;
import com.braintree.commands.BrainTreeDeletePaymentMethodCommand;
import com.braintree.commands.BrainTreeFindCustomerCommand;
import com.braintree.commands.BrainTreeFindMerchantAccountCommand;
import com.braintree.commands.BrainTreeGenerateClientTokenCommand;
import com.braintree.commands.BrainTreePartialCaptureCommand;
import com.braintree.commands.BrainTreeRefundCommand;
import com.braintree.commands.BrainTreeRemoveAddressCommand;
import com.braintree.commands.BrainTreeRemoveCustomerCommand;
import com.braintree.commands.BrainTreeSaleCommand;
import com.braintree.commands.BrainTreeSubmitForSettlementCommand;
import com.braintree.commands.BrainTreeUpdateAddressCommand;
import com.braintree.commands.BrainTreeUpdateCustomerCommand;
import com.braintree.commands.BrainTreeUpdatePaymentMethodCommand;
import com.braintree.commands.BrainTreeVoidCommand;
import com.braintree.commands.BrainTreeWebhookNotificationCommand;
import com.braintree.commands.GetPaymentMethodByTokenCommand;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.customer.service.BrainTreeCustomerAccountService;
import com.braintree.enums.BrainTreeCardType;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.dto.BraintreeInfo;
import com.braintree.payment.info.dao.BrainTreePaymentInfoDao;
import com.braintree.paypal.converters.impl.BillingAddressConverter;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import com.braintreegateway.AndroidPayDetails;
import com.braintreegateway.CreditCard;
import com.braintreegateway.Customer;
import com.braintreegateway.PayPalAccount;
import com.braintreegateway.WebhookNotification;
import com.braintreegateway.exceptions.NotFoundException;
import de.hybris.platform.braintree.data.BrainTreeWebhookNotificationRequest;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.commands.AuthorizationCommand;
import de.hybris.platform.payment.commands.CreateSubscriptionCommand;
import de.hybris.platform.payment.commands.factory.CommandFactory;
import de.hybris.platform.payment.commands.factory.CommandFactoryRegistry;
import de.hybris.platform.payment.commands.factory.CommandNotSupportedException;
import de.hybris.platform.payment.commands.request.AuthorizationRequest;
import de.hybris.platform.payment.commands.request.CreateSubscriptionRequest;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.commands.result.AuthorizationResult;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


public class BrainTreePaymentServiceImpl implements BrainTreePaymentService
{
	private static final Logger LOG = Logger.getLogger(BrainTreePaymentServiceImpl.class);

	private ModelService modelService;
	private CartService cartService;
	private BrainTreeCustomerAccountService brainTreeCustomerAccountService;
	private BrainTreeConfigService brainTreeConfigService;
	private CommandFactoryRegistry commandFactoryRegistry;

	private BillingAddressConverter billingAddressConverter;
	private CheckoutCustomerStrategy checkoutCustomerStrategy;

	private BrainTreePaymentTransactionService brainTreePaymentTransactionService;
	private BrainTreePaymentInfoDao brainTreePaymentInfoDao;

	@Override
	public AuthorizationResult authorize(final AuthorizationRequest authorizationRequest)
	{
		final CustomerModel customer = checkoutCustomerStrategy.getCurrentUserForCheckout();
		return authorize((BrainTreeAuthorizationRequest) authorizationRequest, customer);
	}

	@Override
	public AuthorizationResult authorize(final BrainTreeAuthorizationRequest authorizationRequest, final CustomerModel customer)
	{
		BlLogger.logMessage(LOG, Level.DEBUG, "authorize, authorizationRequest.getTotalAmount: " + authorizationRequest.getTotalAmount());

		try
		{
			if (brainTreeConfigService.getVaultingForCurrentUser(authorizationRequest.getPaymentType()) &&
					!checkIfBrainTreeCustomerExist(customer.getBraintreeCustomerId()))
			{
				saveBraintreeCustomerId(customer, getCart().getDeliveryAddress());
				authorizationRequest.setCustomerId(customer.getBraintreeCustomerId());
			}

			BlLogger.logMessage(LOG, Level.DEBUG, "authorizationRequest " + authorizationRequest);

			final AuthorizationCommand command = getCommandFactory().createCommand(AuthorizationCommand.class);
			return command.perform(authorizationRequest);
		}
		catch (final NotFoundException exception)
		{
			BlLogger.logMessage(LOG, Level.ERROR, "[BT Payment Service] Errors occured not fount some item in BrainTree"
					+ "(throws NotFoundException) ", exception);
			throw new AdapterException("Problem occurred in Payment Provider configuration. Please contact with store support.");
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "[BT Payment Service] Errors during authorization: {} ",
					exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage());
		}
	}

	@Override
	public BrainTreeVoidResult voidTransaction(final VoidRequest voidRequest)
	{
		try
		{
			final BrainTreeVoidCommand command = getCommandFactory().createCommand(BrainTreeVoidCommand.class);
			final BrainTreeVoidResult result = command.perform(voidRequest);
			BlLogger.logMessage(LOG, Level.DEBUG, "voidTransaction result " + result);
			return result;
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "[BT Payment Service] Errors during trying to void transaction: {} ",
					exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeCloneTransactionResult cloneTransaction(final BrainTreeCloneTransactionRequest request)
	{
		try
		{
			final BrainTreeCloneCommand command = getCommandFactory().createCommand(BrainTreeCloneCommand.class);
			return command.perform(request);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to clone transaction: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeRefundTransactionResult refundTransaction(final BrainTreeRefundTransactionRequest request)
	{
		try
		{
			final BrainTreeRefundCommand command = getCommandFactory().createCommand(BrainTreeRefundCommand.class);
			return command.perform(request);
		}
		catch (final CommandNotSupportedException exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to refund transaction: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeUpdateCustomerResult updateCustomer(final BrainTreeUpdateCustomerRequest request)
	{
		try
		{
			final BrainTreeUpdateCustomerCommand command = getCommandFactory().createCommand(BrainTreeUpdateCustomerCommand.class);
			return command.perform(request);
		}
		catch (final CommandNotSupportedException exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to update customer: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeSaleTransactionResult saleTransaction(final BrainTreeSaleTransactionRequest request)
	{
		try
		{
			final BrainTreeSaleCommand command = getCommandFactory().createCommand(BrainTreeSaleCommand.class);
			return command.perform(request);
		}
		catch (final CommandNotSupportedException exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to sate transaction: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeSaleTransactionResult partialCaptureTransaction(final BrainTreeSaleTransactionRequest request)
	{
		try
		{
			final BrainTreePartialCaptureCommand command = getCommandFactory().createCommand(BrainTreePartialCaptureCommand.class);
			return command.perform(request);
		}
		catch (final CommandNotSupportedException exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to sate transaction: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeCustomerResult removeCustomer(final BrainTreeCustomerRequest request)
	{
		try
		{
			final BrainTreeRemoveCustomerCommand command = getCommandFactory().createCommand(BrainTreeRemoveCustomerCommand.class);
			return command.perform(request);
		}
		catch (final CommandNotSupportedException exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to remove customer: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeUpdatePaymentMethodResult updatePaymentMethod(final BrainTreeUpdatePaymentMethodRequest request)
	{
		try
		{
			final BrainTreeUpdatePaymentMethodCommand command = getCommandFactory().createCommand(
					BrainTreeUpdatePaymentMethodCommand.class);
			return command.perform(request);
		}
		catch (final CommandNotSupportedException exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to update payment Method: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public SubscriptionResult createCustomerSubscription(final CreateSubscriptionRequest subscriptionRequest)
	{
		try
		{
			final CreateSubscriptionCommand command = getCommandFactory().createCommand(CreateSubscriptionCommand.class);
			return command.perform(subscriptionRequest);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during customer creation: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage());
		}
	}

	@Override
	public BrainTreeGenerateClientTokenResult generateClientToken(final BrainTreeGenerateClientTokenRequest clientTokenRequest)
	{
		try
		{
			final BrainTreeGenerateClientTokenCommand command = getCommandFactory()
					.createCommand(BrainTreeGenerateClientTokenCommand.class);

			return command.perform(clientTokenRequest);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during token generation: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeFindCustomerResult findCustomer(final BrainTreeCustomerRequest findCustomerRequest)
	{
		try
		{
			final BrainTreeFindCustomerCommand command = getCommandFactory().createCommand(BrainTreeFindCustomerCommand.class);
			return command.perform(findCustomerRequest);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to find customer generation: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeSubmitForSettlementTransactionResult submitForSettlementTransaction(
			final BrainTreeSubmitForSettlementTransactionRequest request)
	{
		LOG.info("submitForSettlementTransaction");
		try
		{
			final BrainTreeSubmitForSettlementCommand command = getCommandFactory()
					.createCommand(BrainTreeSubmitForSettlementCommand.class);
			final BrainTreeSubmitForSettlementTransactionResult result = command.perform(request);

			updateCaptureTransaction(request);

			return result;
		}
		catch (final CommandNotSupportedException exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to submit for settlement transaction: " + exception.getMessage(),
					exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	private void updateCaptureTransaction(final BrainTreeSubmitForSettlementTransactionRequest request)
	{
		List<PaymentTransactionModel> transactions = brainTreePaymentTransactionService
				.getTransactionsByRequestIdAndPaymentProvider(request.getTransactionId(), BraintreeConstants.BRAINTREE_PROVIDER_NAME);
		for (PaymentTransactionModel transactionModel : transactions)
		{
			for (PaymentTransactionEntryModel transactionEntryModel : transactionModel.getEntries())
			{
				if (PaymentTransactionType.CAPTURE.equals(transactionEntryModel.getType()))
				{
					transactionEntryModel.setAmount(request.getAmount());
					modelService.save(transactionEntryModel);
				}
			}
		}
	}

	@Override
	public BrainTreeAddressResult createAddress(final BrainTreeAddressRequest addressRequest, final CustomerModel customer)
	{
		try
		{
			populateCustomerID(addressRequest, customer);
			final BrainTreeCreateAddressCommand command = getCommandFactory().createCommand(BrainTreeCreateAddressCommand.class);
			return command.perform(addressRequest);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during address creation: " + exception.getMessage(), exception);
			return null;
		}
	}

	private void populateCustomerID(final BrainTreeAddressRequest addressRequest, final CustomerModel customer)
	{
		if (addressRequest.getCustomerId() == null)
		{
			if (customer.getBraintreeCustomerId() == null)
			{
				saveBraintreeCustomerId(customer);
				addressRequest.setCustomerId(customer.getBraintreeCustomerId());
			}
			else
			{
				addressRequest.setCustomerId(customer.getBraintreeCustomerId());
			}
		}
	}

	@Override
	public BrainTreeAddressResult updateAddress(final BrainTreeAddressRequest addressRequest)
	{
		try
		{
			final BrainTreeUpdateAddressCommand command = getCommandFactory().createCommand(BrainTreeUpdateAddressCommand.class);
			return command.perform(addressRequest);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during address[" + addressRequest.getAddressId() + "] update: " + exception
					.getMessage(), exception);
		}
		return null;
	}

	@Override
	public BrainTreeAddressResult removeAddress(final BrainTreeAddressRequest addressRequest)
	{
		try
		{
			final BrainTreeRemoveAddressCommand command = getCommandFactory().createCommand(BrainTreeRemoveAddressCommand.class);
			return command.perform(addressRequest);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during address[" + addressRequest.getAddressId() + "] creation: " + exception
					.getMessage(), exception);
		}
		return new BrainTreeAddressResult();
	}

	@Override
	public BrainTreePaymentInfoModel completeCreateSubscription(final CustomerModel customer, final String paymentInfoId)
	{
		final CartModel card = getCart();

		final BrainTreePaymentInfoModel paymentInfo = brainTreeCustomerAccountService.getBrainTreePaymentInfoForCode(customer,
				paymentInfoId);

		if (paymentInfo != null)
		{
			paymentInfo.setUsePaymentMethodToken(Boolean.TRUE);
			paymentInfo.setMerchantAccountIdForCurrentSite(getBrainTreeConfigService()
					.getMerchantAccountIdForCurrentSiteAndCurrency());
			modelService.save(paymentInfo);
			card.setPaymentInfo(paymentInfo);
			modelService.save(card);

			return paymentInfo;
		}
		else
		{
			return null;
		}
	}

	@Override
	public BrainTreePaymentInfoModel completeCreateSubscriptionForModifyPayment(final CustomerModel customer, final String paymentInfoId, final AbstractOrderModel order)
	{
		

		final BrainTreePaymentInfoModel paymentInfo = brainTreeCustomerAccountService.getBrainTreePaymentInfoForCode(customer,
				paymentInfoId);

		if (paymentInfo != null)
		{
			paymentInfo.setUsePaymentMethodToken(Boolean.TRUE);
			paymentInfo.setMerchantAccountIdForCurrentSite(getBrainTreeConfigService()
					.getMerchantAccountIdForCurrentSiteAndCurrency());
			modelService.save(paymentInfo);
			order.setPaymentInfo(paymentInfo);
			modelService.save(order);

			return paymentInfo;
		}
		else
		{
			return null;
		}
	}

	@Override
	public String generateClientToken()
	{
		return getClientToken();
	}

	@Override
	public String generateClientToken(final String site, final String currency)
	{
		return getClientToken();
	}

	@Override
	public BrainTreeCreatePaymentMethodResult createPaymentMethod(final BrainTreeCreatePaymentMethodRequest request)
	{
		LOG.info("createPaymentMethod");
		try
		{
			final BrainTreeCreatePaymentMethodCommand command = getCommandFactory()
					.createCommand(BrainTreeCreatePaymentMethodCommand.class);
			final BrainTreeCreatePaymentMethodResult result = command.perform(request);
			LOG.info("Created PaymentMethod, result: " + result);
			return result;
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors payment method creating: " + exception.getMessage(), exception);
			return null;
		}
	}

	@Override
	public BrainTreePaymentMethodResult createCreditCardPaymentMethod(final BrainTreeCreateCreditCardPaymentMethodRequest request)
	{
		try
		{
			final BrainTreeCreateCreditCardPaymentMethodCommand command = getCommandFactory().createCommand(
					BrainTreeCreateCreditCardPaymentMethodCommand.class);
			return command.perform(request);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors payment method creating: " + exception.getMessage(), exception);
			return null;
		}
	}

	@Override
	public String createPaymentMethodNonce(String request)
	{
		String result = null;
		LOG.info("createPaymentMethodNonce");
		try
		{
			BrainTreeCreatePaymentMethodNonceCommand command = getCommandFactory()
					.createCommand(BrainTreeCreatePaymentMethodNonceCommand.class);
			result = command.perform(request);
		}
		catch (Exception exception)
		{
			LOG.error("[BT Payment Service] Error when trying to create payment method nonce: " + exception.getMessage(), exception);
		}
		return result;
	}

	@Override
	public BrainTreePaymentMethodResult deletePaymentMethod(final BrainTreeDeletePaymentMethodRequest request)
	{
		try
		{
			final BrainTreeDeletePaymentMethodCommand command = getCommandFactory().createCommand(
					BrainTreeDeletePaymentMethodCommand.class);
			return command.perform(request);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to delete payment method: " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public BrainTreeFindMerchantAccountResult findMerchantAccount(
			final BrainTreeFindMerchantAccountRequest brainTreeFindMerchantAccountRequest)
	{
		try
		{
			final BrainTreeFindMerchantAccountCommand command = getCommandFactory().createCommand(
					BrainTreeFindMerchantAccountCommand.class);
			return command.perform(brainTreeFindMerchantAccountRequest);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to find merchant account: " + exception.getMessage(), exception);
			throw new AdapterException("Request failed!", exception);
		}
	}

	@Override
	public void createPaymentMethodTokenForOrderReplenishment()
	{
		final CustomerModel customer = checkoutCustomerStrategy.getCurrentUserForCheckout();
		final PaymentInfoModel paymentInfo = cartService.getSessionCart().getPaymentInfo();
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			if (!((BrainTreePaymentInfoModel) paymentInfo).getUsePaymentMethodToken().booleanValue())
			{
				final BrainTreeCreatePaymentMethodRequest request = new BrainTreeCreatePaymentMethodRequest(null,
						((BrainTreePaymentInfoModel) paymentInfo).getNonce(), customer.getBraintreeCustomerId());

				final BrainTreeCreatePaymentMethodResult result = createPaymentMethod(request);
				if (result != null)
				{
					((BrainTreePaymentInfoModel) paymentInfo).setPaymentMethodToken(result.getPaymentMethodToken());
					((BrainTreePaymentInfoModel) paymentInfo).setUsePaymentMethodToken(Boolean.TRUE);
					modelService.save(paymentInfo);
				}

			}
		}
		else
		{
			throw new AdapterException("Error during creation payment method for replenishment.");
		}
	}

	private CartModel getCart()
	{
		return cartService.getSessionCart();
	}

	private String getClientToken()
	{
		final String authenticationToken = getAuthenticationToken();
		if (authenticationToken != null && !StringUtils.EMPTY.equals(authenticationToken))
		{
			return authenticationToken;
		}

				final String merchantAccountId = getBrainTreeConfigService().getMerchantAccountIdForCurrentSiteAndCurrency();
				if (StringUtils.isNotEmpty(merchantAccountId))
				{
					final BrainTreeFindMerchantAccountRequest brainTreeFindMerchantAccountRequest = new BrainTreeFindMerchantAccountRequest(
							StringUtils.EMPTY);
					brainTreeFindMerchantAccountRequest.setMerchantAccount(merchantAccountId);
					final boolean isMerchantAccountExist = findMerchantAccount(brainTreeFindMerchantAccountRequest).isMerchantAccountExist();
					if (isMerchantAccountExist)
					{
						brainTreeFindMerchantAccountRequest.setMerchantAccount(merchantAccountId);
					}
				}
		BrainTreeGenerateClientTokenRequest brainTreeGenerateClientTokenRequest = new BrainTreeGenerateClientTokenRequest(
				StringUtils.EMPTY);
		if (StringUtils.isNotEmpty(merchantAccountId))
		{
			brainTreeGenerateClientTokenRequest.setMerchantAccount(merchantAccountId);
		}

		final BrainTreeGenerateClientTokenResult response = generateClientToken(brainTreeGenerateClientTokenRequest);
		return response.getClientToken();
	}

	private String getAuthenticationToken()
	{
		return brainTreeConfigService.getConfigurationService().getConfiguration().getString(BRAINTREE_AUTHENTICATION_TOKEN);
	}

	private boolean checkIfBrainTreeCustomerExist(final String braintreeCustomerId)
	{
		return findBrainTreeCustomer(braintreeCustomerId).isCustomerExist();
	}

	private SubscriptionResult createCustomerSubscription(final CustomerModel customer, final AddressModel shippingAddress)
	{
		final BillingInfo billingInfo = new BillingInfo();
		if (shippingAddress != null)
		{
			billingAddressConverter.convert(shippingAddress, billingInfo);
		}
		else
		{
			billingInfo.setEmail(customer.getContactEmail());
		}

		final CreateSubscriptionRequest createSubscriptionRequest = new CreateSubscriptionRequest(null, billingInfo, null, null,
				null, null, null);

		return createCustomerSubscription(createSubscriptionRequest);
	}


	private String saveBraintreeCustomerId(final CustomerModel customer, final AddressModel shippingAddress)
	{
		final SubscriptionResult result = createCustomerSubscription(customer, shippingAddress);
		customer.setBraintreeCustomerId(result.getMerchantTransactionCode());
		getModelService().save(customer);
		return result.getMerchantTransactionCode();
	}

	private String saveBraintreeCustomerId(final CustomerModel customer)
	{
		final SubscriptionResult result = createCustomerSubscription(customer, null);
		customer.setBraintreeCustomerId(result.getMerchantTransactionCode());
		getModelService().save(customer);
		return result.getMerchantTransactionCode();
	}

	private CommandFactory getCommandFactory()
	{
		return commandFactoryRegistry.getFactory(BRAINTREE_PROVIDER_NAME);
	}

	@Override
	public BrainTreeCreatePaymentMethodResult createPaymentMethodForCustomer(final CustomerModel customer,
			final AddressModel billingAddress, final BraintreeInfo braintreeInfo)
	{
		final BillingInfo billingInfo = billingAddressConverter.convert(billingAddress);

		final BrainTreeCreatePaymentMethodRequest request = new BrainTreeCreatePaymentMethodRequest(null, braintreeInfo.getNonce(),
				braintreeInfo.getCardholderName(), customer.getBraintreeCustomerId(), billingAddress.getBrainTreeAddressId(),
				billingInfo, braintreeInfo.getAmount());

		BrainTreeFindCustomerResult findCustomerResult = findBrainTreeCustomer(customer.getBraintreeCustomerId());
		if (!findCustomerResult.isCustomerExist())
		{
			final String customerId = saveBraintreeCustomerId(customer, billingAddress);
			request.setCustomerId(customerId);
			return createPaymentMethod(request);
		}
		else
		{
			return findPaymentMethod(customer, findCustomerResult.getCustomer(), braintreeInfo, request);
		}
	}

	@Override
	public String createCustomer(CustomerModel customer, AddressModel billingAddress)
	{
		return saveBraintreeCustomerId(customer, billingAddress);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public BrainTreePaymentInfoModel getBrainTreePaymentInfoForCode(final CustomerModel customer, final String
			paymentInfoId, final String nonce) {
		final BrainTreePaymentInfoModel paymentInfo = brainTreeCustomerAccountService.getBrainTreePaymentInfoForCode(
				customer, paymentInfoId);
		if(null != paymentInfo) {
			paymentInfo.setNonce(nonce);
			getModelService().save(paymentInfo);
		}
		return paymentInfo;
	}

	private BrainTreeFindCustomerResult findBrainTreeCustomer(String braintreeCustomerId)
	{
		if (StringUtils.isEmpty(braintreeCustomerId))
		{
			return new BrainTreeFindCustomerResult(false);
		}
		final BrainTreeCustomerRequest findCustomerRequest = new BrainTreeCustomerRequest(braintreeCustomerId);
		findCustomerRequest.setCustomerId(braintreeCustomerId);
		return findCustomer(findCustomerRequest);
	}

	private BrainTreeCreatePaymentMethodResult findPaymentMethod(CustomerModel customerModel, Customer customer,
			BraintreeInfo braintreeInfo, BrainTreeCreatePaymentMethodRequest request)
	{

		BrainTreeCreatePaymentMethodResult result = null;

		if (BraintreeConstants.PAYPAL_PAYMENT.equals(braintreeInfo.getPaymentProvider())
				&& !BraintreeConstants.PAYPAL_INTENT_ORDER.equals(braintreeInfo.getIntent()))
		{
			result = hasPayPalAccount(customerModel, customer.getPayPalAccounts(), braintreeInfo);
		}

		if (result == null)
		{
			result = createPaymentMethod(request);
		}

		return result;
	}

	private BrainTreeCreatePaymentMethodResult hasPayPalAccount(CustomerModel customerModel, List<PayPalAccount> payPalAccounts,
			BraintreeInfo braintreeInfo)
	{
		if (StringUtils.isNotBlank(braintreeInfo.getEmail()))
		{
			List<String> paymentMethodTokens = getPaymentMethodTokensForPayPalAccount(customerModel.getPaymentInfos(),
					braintreeInfo.getEmail());
			if (!paymentMethodTokens.isEmpty())
			{
				for (PayPalAccount account : payPalAccounts)
				{
					for (String token : paymentMethodTokens)
					{
						if (token.equalsIgnoreCase(account.getToken()))
						{
							BrainTreeCreatePaymentMethodResult result = createPaymentMethodResult(account);
							braintreeInfo.setDuplicatedPayment(Boolean.TRUE);
							return result;
						}
					}
				}
			}
		}
		return null;
	}

	private List<String> getPaymentMethodTokensForPayPalAccount(Collection<PaymentInfoModel> paymentInfoModels, String payPalEmail)
	{
		List<String> paymentMethodTokens = new ArrayList<>();
		for (PaymentInfoModel paymentInfo : paymentInfoModels)
		{
			if (isSavedPayPalAccount(paymentInfo, payPalEmail))
			{
				paymentMethodTokens.add(((BrainTreePaymentInfoModel) paymentInfo).getPaymentMethodToken());
			}
		}
		return paymentMethodTokens;
	}

	private boolean isSavedPayPalAccount(PaymentInfoModel paymentInfo, String email)
	{
		return paymentInfo.isSaved()
				&& paymentInfo instanceof BrainTreePaymentInfoModel
				&& BraintreeConstants.PAYPAL_PAYMENT.equals(((BrainTreePaymentInfoModel) paymentInfo).getPaymentProvider())
				&& email.equals((paymentInfo).getBillingAddress().getEmail())
				&& StringUtils.isNotBlank(((BrainTreePaymentInfoModel) paymentInfo).getPaymentMethodToken());
	}

	private BrainTreeCreatePaymentMethodResult createPaymentMethodResult(PayPalAccount paymentMethod)
	{
		BrainTreeCreatePaymentMethodResult result = new BrainTreeCreatePaymentMethodResult();
		result.setPaymentMethodToken(paymentMethod.getToken());
		result.setImageSource(paymentMethod.getImageUrl());
		result.setEmail(paymentMethod.getEmail());
		result.setSuccess(Boolean.TRUE);
		result.setCardType(StringUtils.EMPTY);
		result.setExpirationMonth(StringUtils.EMPTY);
		result.setExpirationYear(StringUtils.EMPTY);
		result.setCardNumber(StringUtils.EMPTY);
		result.setCardholderName(StringUtils.EMPTY);
		return result;
	}

	@Override
	public WebhookNotification getWebhookNotification(BrainTreeWebhookNotificationRequest webhookNotificationRequest)
	{
		BrainTreeWebhookNotificationCommand command = null;
		try
		{
			command = getCommandFactory().createCommand(BrainTreeWebhookNotificationCommand.class);
			return command.perform(webhookNotificationRequest);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to get webhook notification " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}

	@Override
	public void updatePaymentInfo(PaymentInfoModel paymentInfo, CreditCard creditCard)
	{
		if (creditCard == null || creditCard.getToken() == null)
		{
			return;
		}
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			if (((BrainTreePaymentInfoModel) paymentInfo).isShouldBeSaved())
			{
				paymentInfo.setSaved(true);
			}
			if (brainTreeConfigService.getVaultingForCurrentUser(((BrainTreePaymentInfoModel) paymentInfo).getPaymentProvider()))
			{
				((BrainTreePaymentInfoModel) paymentInfo).setUsePaymentMethodToken(true);
				((BrainTreePaymentInfoModel) paymentInfo).setPaymentMethodToken(creditCard.getToken());
				((BrainTreePaymentInfoModel) paymentInfo).setExpirationMonth(creditCard.getExpirationMonth());
				((BrainTreePaymentInfoModel) paymentInfo).setExpirationYear(creditCard.getExpirationYear());
				((BrainTreePaymentInfoModel) paymentInfo).setImageSource(creditCard.getImageUrl());
				getModelService().save(paymentInfo);
			}
		}
	}

	@Override
	public void updatePaymentInfo(PaymentInfoModel paymentInfo, AndroidPayDetails androidPayDetails)
	{
		if (androidPayDetails.getToken() == null)
		{
			return;
		}
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			if (((BrainTreePaymentInfoModel) paymentInfo).isShouldBeSaved())
			{
				paymentInfo.setSaved(true);
			}
			if (brainTreeConfigService.getVaultingForCurrentUser(((BrainTreePaymentInfoModel) paymentInfo).getPaymentProvider()))
			{
				((BrainTreePaymentInfoModel) paymentInfo).setUsePaymentMethodToken(true);
				((BrainTreePaymentInfoModel) paymentInfo).setPaymentMethodToken(androidPayDetails.getToken());
				((BrainTreePaymentInfoModel) paymentInfo).setExpirationMonth(androidPayDetails.getExpirationMonth());
				((BrainTreePaymentInfoModel) paymentInfo).setExpirationYear(androidPayDetails.getExpirationYear());
				((BrainTreePaymentInfoModel) paymentInfo).setImageSource(androidPayDetails.getImageUrl());
				if (((BrainTreePaymentInfoModel) paymentInfo).getCardNumber() == null)
				{
					((BrainTreePaymentInfoModel) paymentInfo)
							.setCardNumber(String.format(CARD_NUMBER_MASK, androidPayDetails.getSourceCardLast4()));
					((BrainTreePaymentInfoModel) paymentInfo)
							.setCardType(BrainTreeCardType.valueOf(androidPayDetails.getSourceCardType()));
				}
				getModelService().save(paymentInfo);
			}
		}
	}

	@Override
	public void updatePaymentInfo(PaymentInfoModel paymentInfo, PayPalAccount payPalDetails)
	{
		if (payPalDetails.getToken() == null)
		{
			return;
		}
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			if (((BrainTreePaymentInfoModel) paymentInfo).isShouldBeSaved())
			{
				paymentInfo.setSaved(true);
			}
			if (brainTreeConfigService.getVaultingForCurrentUser(((BrainTreePaymentInfoModel) paymentInfo).getPaymentProvider()))
			{
				((BrainTreePaymentInfoModel) paymentInfo).setUsePaymentMethodToken(true);
				((BrainTreePaymentInfoModel) paymentInfo).setPaymentMethodToken(payPalDetails.getToken());
				((BrainTreePaymentInfoModel) paymentInfo).setImageSource(payPalDetails.getImageUrl());

				getModelService().save(paymentInfo);
			}
		}
	}

	public PayPalAccount getPaymentMethodFromBTByToken(final  String paymentMethodToken){
		GetPaymentMethodByTokenCommand command = null;
		try
		{
			command = getCommandFactory().createCommand(GetPaymentMethodByTokenCommand.class);
			return command.perform(paymentMethodToken);
		}
		catch (final Exception exception)
		{
			LOG.error("[BT Payment Service] Errors during trying to get webhook payment method by token " + exception.getMessage(), exception);
			throw new AdapterException(exception.getMessage(), exception);
		}
	}
	
  /**
   * {@inheritDoc}
   */
  @Override
  public BrainTreePaymentInfoModel getBrainTreePaymentInfoForCodeToDeposit(final CustomerModel customer, final String paymentInfoId,
      final String nonce, final Double depositAmount)
  {
    try
    {
      final BrainTreePaymentInfoModel brainTreePaymentInfoModel = getClonedPaymentInfoForCode(customer, paymentInfoId, nonce);
      if(Objects.nonNull(brainTreePaymentInfoModel))
      {
        brainTreePaymentInfoModel.setIsDepositPayment(Boolean.TRUE);
        brainTreePaymentInfoModel.setDepositAmount(depositAmount);
        getModelService().save(brainTreePaymentInfoModel);
        getModelService().refresh(brainTreePaymentInfoModel);
      }      
      return brainTreePaymentInfoModel;
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
          "Error while getting BraintreePaymentInfoModel for creating deposit for customer : {} and payment info ID ; {}", customer.getUid(),
          paymentInfoId);
    }
    return null;
  }
  
  @Override
  public BrainTreePaymentInfoModel getModifyOrderPaymentInfoForCode(final CustomerModel customer, final String paymentInfoId,
      final String nonce, final Double newAmount)
  {
    try
    {
      final BrainTreePaymentInfoModel brainTreePaymentInfoModel = getClonedPaymentInfoForCode(customer, paymentInfoId, nonce);
      if(Objects.nonNull(brainTreePaymentInfoModel))
      {
        brainTreePaymentInfoModel.setCreateNewTransaction(Boolean.TRUE);
        brainTreePaymentInfoModel.setNewAmount(newAmount);
        getModelService().save(brainTreePaymentInfoModel);
        getModelService().refresh(brainTreePaymentInfoModel);
      }      
      return brainTreePaymentInfoModel;
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
          "Error while getting BraintreePaymentInfoModel for creating deposit for customer : {} and payment info ID ; {}", customer.getUid(),
          paymentInfoId);
    }
    return null;
  }
  
  private BrainTreePaymentInfoModel getClonedPaymentInfoForCode(final CustomerModel customer, final String paymentInfoId,
      final String nonce)
  {
    try
    {
      final BrainTreePaymentInfoModel paymentInfo = brainTreeCustomerAccountService.getBrainTreePaymentInfoForCode(customer, paymentInfoId);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Original Payment Info PK is : {}", paymentInfo.getPk().toString());
      if (Objects.nonNull(paymentInfo))
      {
        final BrainTreePaymentInfoModel brainTreePaymentInfoModel = getModelService().clone(paymentInfo, BrainTreePaymentInfoModel.class);
        if (Objects.nonNull(brainTreePaymentInfoModel))
        {
          brainTreePaymentInfoModel.setNonce(nonce);
          brainTreePaymentInfoModel.setOriginal(paymentInfo);
          brainTreePaymentInfoModel.setDuplicate(Boolean.TRUE);
          brainTreePaymentInfoModel.setIsDefault(Boolean.FALSE);
          getModelService().save(brainTreePaymentInfoModel);
          getModelService().refresh(brainTreePaymentInfoModel);
          BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "After Cloning Payment Info PK is : {}", brainTreePaymentInfoModel.getPk().toString());
        }
        return brainTreePaymentInfoModel;
      }
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
          "Error while getting BraintreePaymentInfoModel for creating deposit for customer : {} and payment info ID ; {}", customer.getUid(),
          paymentInfoId);
    }
    return null;
  }

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the cartService
	 */
	public CartService getCartService()
	{
		return cartService;
	}

	/**
	 * @param cartService the cartService to set
	 */
	public void setCartService(final CartService cartService)
	{
		this.cartService = cartService;
	}

	/**
	 * @return the brainTreeCustomerAccountService
	 */
	public BrainTreeCustomerAccountService getBrainTreeCustomerAccountService()
	{
		return brainTreeCustomerAccountService;
	}

	/**
	 * @param brainTreeCustomerAccountService the brainTreeCustomerAccountService to set
	 */
	public void setBrainTreeCustomerAccountService(final BrainTreeCustomerAccountService brainTreeCustomerAccountService)
	{
		this.brainTreeCustomerAccountService = brainTreeCustomerAccountService;
	}

	/**
	 * @return the brainTreeConfigService
	 */
	public BrainTreeConfigService getBrainTreeConfigService()
	{
		return brainTreeConfigService;
	}

	/**
	 * @param brainTreeConfigService the brainTreeConfigService to set
	 */
	public void setBrainTreeConfigService(final BrainTreeConfigService brainTreeConfigService)
	{
		this.brainTreeConfigService = brainTreeConfigService;
	}

	/**
	 * @return the commandFactoryRegistry
	 */
	public CommandFactoryRegistry getCommandFactoryRegistry()
	{
		return commandFactoryRegistry;
	}

	/**
	 * @param commandFactoryRegistry the commandFactoryRegistry to set
	 */
	public void setCommandFactoryRegistry(final CommandFactoryRegistry commandFactoryRegistry)
	{
		this.commandFactoryRegistry = commandFactoryRegistry;
	}

	/**
	 * @return the billingAddressConverter
	 */
	public BillingAddressConverter getBillingAddressConverter()
	{
		return billingAddressConverter;
	}

	/**
	 * @param billingAddressConverter the billingAddressConverter to set
	 */
	public void setBillingAddressConverter(final BillingAddressConverter billingAddressConverter)
	{
		this.billingAddressConverter = billingAddressConverter;
	}

	/**
	 * @return the checkoutCustomerStrategy
	 */
	public CheckoutCustomerStrategy getCheckoutCustomerStrategy()
	{
		return checkoutCustomerStrategy;
	}

	/**
	 * @param checkoutCustomerStrategy the checkoutCustomerStrategy to set
	 */
	public void setCheckoutCustomerStrategy(final CheckoutCustomerStrategy checkoutCustomerStrategy)
	{
		this.checkoutCustomerStrategy = checkoutCustomerStrategy;
	}

	public BrainTreePaymentTransactionService getBrainTreePaymentTransactionService()
	{
		return brainTreePaymentTransactionService;
	}

	public void setBrainTreePaymentTransactionService(BrainTreePaymentTransactionService brainTreePaymentTransactionService)
	{
		this.brainTreePaymentTransactionService = brainTreePaymentTransactionService;
	}

	public BrainTreePaymentInfoDao getBrainTreePaymentInfoDao()
	{
		return brainTreePaymentInfoDao;
	}

	public void setBrainTreePaymentInfoDao(BrainTreePaymentInfoDao brainTreePaymentInfoDao)
	{
		this.brainTreePaymentInfoDao = brainTreePaymentInfoDao;
	}
}
