package com.braintree.transaction.service.impl;

import static com.braintree.constants.BraintreeConstants.BRAINTREE_ECVZ_ACEESS_TOKEN;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_MERCHANT_ID;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PAYMENT;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PRIVATE_KEY;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PROVIDER_NAME;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PUBLIC_KEY;
import static com.braintree.constants.BraintreeConstants.FAKE_REQUEST_ID;
import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_ORDER;
import static com.braintree.constants.BraintreeConstants.PAYPAL_PAYMENT;
import static com.braintree.constants.BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT;
import static com.braintree.constants.BraintreeConstants.PROPERTY_LEVEL2_LEVEL3;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;
import static de.hybris.platform.util.Config.getParameter;
import static org.apache.commons.lang.StringUtils.isNotEmpty;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.enums.PaymentTransactionTypeEnum;
import com.bl.logging.BlLogger;
import com.braintree.command.request.BrainTreeAuthorizationRequest;
import com.braintree.command.request.BrainTreeFindMerchantAccountRequest;
import com.braintree.command.request.beans.BrainTreeLineItemBean;
import com.braintree.command.result.BrainTreeAuthorizationResult;
import com.braintree.command.result.BrainTreeCreatePaymentMethodResult;
import com.braintree.command.result.BrainTreeRefundTransactionResult;
import com.braintree.command.result.BrainTreeSaleTransactionResult;
import com.braintree.command.result.BrainTreeSubmitForSettlementTransactionResult;
import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.customfield.service.CustomFieldsService;
import com.braintree.enums.BrainTreeCardType;
import com.braintree.exceptions.BraintreeErrorException;
import com.braintree.hybris.data.BraintreeTransactionEntryData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.order.refund.partial.services.BraintreePartialRefundService;
import com.braintree.order.submitForSettlement.service.BraintreeSubmitForSettlementService;
import com.braintree.payment.dto.BraintreeInfo;
import com.braintree.paypal.converters.impl.BillingAddressConverter;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.braintreegateway.BraintreeGateway;
import com.braintreegateway.CreditCard;
import com.braintreegateway.PayPalAccount;
import com.braintreegateway.PaymentMethod;
import com.braintreegateway.Result;
import com.braintreegateway.Transaction;
import com.braintreegateway.TransactionRequest;
import com.google.common.collect.Lists;
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.commerceservices.customer.CustomerAccountService;
import de.hybris.platform.commerceservices.enums.CustomerType;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.c2l.CurrencyModel;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.order.price.DiscountModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.payment.PaymentService;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.dto.TransactionStatusDetails;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.util.DiscountValue;
import de.hybris.platform.util.TaxValue;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Currency;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;



public class BrainTreeTransactionServiceImpl implements BrainTreeTransactionService
{
	
	private static final String ORDER_MUST_NOT_BE_NULL = "Order Must not be null";
  private static final double ZERO_PRICE = 0.0;
	private static final int ZERO = 0;

	private static final Logger LOG = Logger.getLogger(BrainTreeTransactionServiceImpl.class);

	private static final int DEFAULT_CURRENCY_DIGIT = 2;
	private static final int MAX_PRODUCT_NAME_FOR_NON_PAYPAL_WITH_LEVEL2LEVEL3_DATA = 35;
	private static final int CONVERT_TO_PERCENTAGE = 100;

	private CartService cartService;
	private ModelService modelService;
	private UserService userService;
	private BrainTreePaymentService brainTreePaymentService;
	private BrainTreePaymentTransactionService brainTreePaymentTransactionService;
	private PaymentService paymentService;
	private BillingAddressConverter billingAddressConverter;
	private CheckoutCustomerStrategy checkoutCustomerStrategy;
	private CommonI18NService commonI18NService;
	private BrainTreeConfigService brainTreeConfigService;
	private CustomFieldsService customFieldsService;
	private BraintreeSubmitForSettlementService braintreeSubmitForSettlementService;
	private CustomerAccountService customerAccountService;
  private BraintreePartialRefundService braintreePartialRefundService;
	
	@Override
	public boolean createAuthorizationTransaction()
	{
		return createAuthorizationTransaction(getCustomFields());
	}

	@Override
	public boolean createAuthorizationTransaction(Map<String, String> customFields)
	{
		final CartModel cart = cartService.getSessionCart();
		try {
			final BrainTreeAuthorizationResult result = brainTreeAuthorize(cart, getCustomFields(cart), 
				getBrainTreeConfigService().getAuthAMountToVerifyCard(), Boolean.FALSE, null);
			return handleAuthorizationResult(result, cart);
		} catch(final Exception ex) {
			BlLogger.logMessage(LOG, Level.ERROR,
				"Error occurred while creating authorization for the cart {} while placing an order", cart.getCode(), ex);
		}
		return false;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean createAuthorizationTransactionOfOrder(final AbstractOrderModel orderModel, final BigDecimal
			amountToAuthorize, final boolean submitForSettlement, final BrainTreePaymentInfoModel paymentInfo)
	{
		try {
			BrainTreeAuthorizationResult result = null;
			//Added condition for modifyPayment with zero order total (full Gift Card Order)
			if(amountToAuthorize != null && amountToAuthorize.compareTo(BigDecimal.ZERO) == ZERO){
				result = brainTreeAuthorize(orderModel, Collections.emptyMap(),
						getBrainTreeConfigService().getAuthAMountToVerifyCard(), submitForSettlement, paymentInfo);
			}else {
				 result = brainTreeAuthorize(orderModel, Collections.emptyMap(),
						amountToAuthorize, submitForSettlement, paymentInfo);
			}
			if(submitForSettlement) {
				createCaptureTransactionEntry(orderModel, result, paymentInfo);
				addTotalDepositedAmountOnOrder(orderModel, paymentInfo, result);
				return result.isSuccess();
			} else {
				return handleAuthorizationResult(result, orderModel);
			}
		} catch(final Exception ex) {
			BlLogger.logMessage(LOG, Level.ERROR,
					"Error occurred while creating authorization for the order {}", orderModel.getCode(), ex);
		}
			return false;
	}

  /**
   * Adds the total deposited amount on order.
   *
   * @param orderModel the order model
   * @param paymentInfo the payment info
   * @param result the result
   */
  private void addTotalDepositedAmountOnOrder(final AbstractOrderModel orderModel, final BrainTreePaymentInfoModel paymentInfo, 
      BrainTreeAuthorizationResult result)
  {
    if(result.isSuccess() && Objects.nonNull(paymentInfo) && BooleanUtils.isTrue(paymentInfo.isIsDepositPayment()))
    {
      final AtomicDouble depositAmountTotal = new AtomicDouble(ObjectUtils.defaultIfNull(orderModel.getDepositAmountTotal(), Double.valueOf(0.0d)));
      depositAmountTotal.addAndGet(paymentInfo.getDepositAmount());
      orderModel.setDepositAmountTotal(depositAmountTotal.get());
      getModelService().save(orderModel);
      getModelService().refresh(orderModel);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Deposit Amount Total : {} for order : {}", depositAmountTotal.get(), orderModel.getCode());
    }
  }

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean captureAuthorizationTransaction(final OrderModel orderModel, final BigDecimal amount,
			final String requestId)
			throws BraintreeErrorException {
				return getBraintreeSubmitForSettlementService()
						.submitForSettlement(orderModel, amount, requestId);
	}

	/**
	 * {@inheritDoc}
	 */
	public void voidAuthTransaction(final OrderModel order) {
		try {
			final String merchantTransactionCode = order.getUser().getUid();
			final List<PaymentTransactionModel> transactions = order.getPaymentTransactions();
			if (CollectionUtils.isNotEmpty(transactions) && null != merchantTransactionCode) {
				final List<PaymentTransactionEntryModel> transactionEntries = transactions.get(0).getEntries();
				final Optional<PaymentTransactionEntryModel> authEntry = transactionEntries.stream()
						.filter(transactionEntry ->
								transactionEntry.getType().equals(PaymentTransactionType.AUTHORIZATION) && transactionEntry
						.getAmount().intValue() == getBrainTreeConfigService().getAuthAMountToVerifyCard().intValue())
						.findFirst();
				if (authEntry.isPresent()) {
					final VoidRequest voidRequest = new VoidRequest(merchantTransactionCode,
							authEntry.get().getRequestId(), StringUtils.EMPTY,
							StringUtils.EMPTY);
					final BrainTreeVoidResult voidResult = brainTreePaymentService
							.voidTransaction(voidRequest);
					setAuthorizedFlagInOrder(voidResult.getTransactionStatus(), order, authEntry.get());
				}
			}
		} catch (final Exception ex) {
			BlLogger.logMessage(LOG, Level.ERROR, "Error occurred while voiding the auth transaction for order {} ",
					order.getCode(), ex);
		}
	}

	/**
	 * {@inheritDoc}
	 */
	public Result<Transaction> issueBlindCredit(final PaymentTransactionEntryModel paymentTransactionEntry,
			final BigDecimal refundAmount) {
		final PaymentTransactionModel transaction = paymentTransactionEntry.getPaymentTransaction();
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Refund initiation for legacy order {}", transaction.getOrder().getCode());
		TransactionRequest request = new TransactionRequest()
				.amount(refundAmount.setScale(BlInventoryScanLoggingConstants.TWO, RoundingMode.HALF_EVEN))
				.paymentMethodToken(((BrainTreePaymentInfoModel) transaction
						.getInfo()).getPaymentMethodToken());
		final Result<Transaction> result = getBraintreeGateway().transaction().credit(request);
		if(result.isSuccess() && Objects.nonNull(result.getTarget())) {
			BlLogger.logFormatMessageInfo(LOG, Level.INFO, "The response of the transaction of issuing credit for legacy order {} is {}",
					transaction.getOrder().getCode(), result.getTarget());
			final PaymentTransactionType transactionType = PaymentTransactionType.REFUND_STANDALONE;
			final String newEntryCode = paymentService.getNewPaymentTransactionEntryCode(transaction, transactionType);

			final PaymentTransactionEntryModel entry = modelService.create(PaymentTransactionEntryModel.class);
			entry.setType(transactionType);
			entry.setCode(newEntryCode);
			entry.setRequestId(result.getTarget().getId());
			entry.setPaymentTransaction(transaction);
			entry.setCurrency(resolveCurrency(result.getTarget().getCurrencyIsoCode()));
			entry.setAmount(formatAmount(result.getTarget().getAmount()));
			entry.setTransactionStatus(result.getTarget().getProcessorResponseType().toString());
			entry.setTime(new Date());
			modelService.saveAll(entry, transaction);
		}
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	public String getBraintreeAddressIDForLegacyPaymentMethods(final String paymentMethodToken) {
		final PaymentMethod paymentMethod = getBraintreeGateway().paymentMethod().find(paymentMethodToken);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Payment method : {} for payment method token : {}", paymentMethod, paymentMethodToken);
		if(paymentMethod instanceof CreditCard) {
			final CreditCard card  = (CreditCard) paymentMethod;
			return Objects.nonNull(card) && Objects.nonNull(card.getBillingAddress()) ? card
					.getBillingAddress().getId() : null;
		}
		return null;
	}

	/**
	 * It gets the braintree gateway
	 * @return BraintreeGateway
	 */
	private BraintreeGateway getBraintreeGateway()
	{
		final BraintreeGateway gateway;
		if (StringUtils.isEmpty(getParameter(BRAINTREE_ECVZ_ACEESS_TOKEN)))
		{
			gateway = new BraintreeGateway(getBrainTreeConfigService().getEnvironmentType(),
					getParameter(BRAINTREE_MERCHANT_ID), getParameter(BRAINTREE_PUBLIC_KEY), getParameter(BRAINTREE_PRIVATE_KEY));
		}
		else
		{
			gateway = new BraintreeGateway(getParameter(BRAINTREE_ECVZ_ACEESS_TOKEN));
		}
		return gateway;
	}

	/**
	 * It sets the transaction status and flags the order as 1$ authorization has been voided
	 * @param transactionStatus
	 * @param order
	 * @param paymentTransactionEntryModel
	 */
	private void setAuthorizedFlagInOrder(final TransactionStatus transactionStatus,
			final OrderModel order, final PaymentTransactionEntryModel paymentTransactionEntryModel) {
		if (TransactionStatus.ACCEPTED.equals(transactionStatus)) {
			order.setIsAuthorizationVoided(Boolean.TRUE);
			getModelService().save(order);
			paymentTransactionEntryModel.setTransactionStatus(Transaction.Status.VOIDED.name());
			getModelService().save(paymentTransactionEntryModel);
			BlLogger.logFormattedMessage(LOG, Level.INFO,
					"The order {} is marked as {} as authorization voided with transaction status {}",
					order.getCode(), order.getIsAuthorizationVoided(), paymentTransactionEntryModel.getTransactionStatus());
		}
	}

	private boolean handleAuthorizationResult(BrainTreeAuthorizationResult result, AbstractOrderModel cart)
	{
		PaymentTransactionEntryModel paymentTransactionEntry = null;
		if (result.isSuccess())
		{
			paymentTransactionEntry = createTransactionEntry(PaymentTransactionType.AUTHORIZATION, cart, result, null);
			saveIntent(cart);
			savePaymentTransaction(paymentTransactionEntry, cart);
			if (result.getAndroidPayDetails() != null)
			{
				brainTreePaymentService.updatePaymentInfo(cart.getPaymentInfo(), result.getAndroidPayDetails());
			}
			else
			{
				brainTreePaymentService.updatePaymentInfo(cart.getPaymentInfo(), result.getCreditCard());
			}
			LOG.info("[BT AUTHORIZE] Transaction with code : " + paymentTransactionEntry.getCode() + " was created with status "
					+ TransactionStatusDetails.SUCCESFULL.name());
			return true;
		}
		else
		{
			LOG.error("[BT AUTHORIZE] Failed!");
			return false;
		}
	}

	private void saveIntent(AbstractOrderModel cart)
	{
		BrainTreePaymentInfoModel brainTreePaymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
		brainTreePaymentInfo.setPayPalIntent(getBrainTreeConfigService().getIntent());
		modelService.save(brainTreePaymentInfo);
	}

	/**
	 * To authorize the order
	 * @param cart
	 * @param  customFields
	 * @param totalAmount
	 * @param submitForSettlement
	 * @return BrainTreeAuthorizationResult
	 */
	private BrainTreeAuthorizationResult brainTreeAuthorize(final AbstractOrderModel cart, Map<String, String> customFields,
			BigDecimal totalAmount, final Boolean submitForSettlement, final BrainTreePaymentInfoModel paymentInfo)
	{
		final CustomerModel customer = (CustomerModel) cart.getUser();

		BrainTreeAuthorizationRequest authorizationRequest = prepareAuthorizationRequest(cart, customer, customFields,
				totalAmount, submitForSettlement, paymentInfo);

		final BrainTreeAuthorizationResult brainTreeAuthorizationResult = (BrainTreeAuthorizationResult) brainTreePaymentService
				.authorize(authorizationRequest, customer);

		if(!StringUtils.isBlank(brainTreeAuthorizationResult.getPaymentMethodToken())){
			savePaymentMethodFromBTByToken(paymentInfo, brainTreeAuthorizationResult.getPaymentMethodToken(), cart);
		}

		return brainTreeAuthorizationResult;
	}

	private void savePaymentMethodFromBTByToken(final BrainTreePaymentInfoModel paymentInfo, final String paymentMethodToken, final AbstractOrderModel cart)
	{
		final PayPalAccount paymentMethod = brainTreePaymentService.getPaymentMethodFromBTByToken(paymentMethodToken);

		brainTreePaymentService.updatePaymentInfo(isPaymentForDeposit(paymentInfo) ? paymentInfo : cart.getPaymentInfo(), paymentMethod);

		LOG.warn(paymentMethod);

	}

	private BrainTreeAuthorizationRequest prepareAuthorizationRequest(AbstractOrderModel cart, CustomerModel customer,
			Map<String, String> customFields, BigDecimal authAmount, final Boolean submitForSettlement,
			BrainTreePaymentInfoModel paymentInfo)
	{
		if(null == paymentInfo) {
			paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
		}
		final String braintreeCustomerId = customer.getBraintreeCustomerId();
		validateParameterNotNullStandardMessage("paymentInfo", paymentInfo);
		String methodNonce = null;
		String deviceData = null;
		String paymentType = null;
		Boolean liabilityShifted = null;
		Boolean usePaymentMethodToken = null;
		String paymentMethodToken = null;
		Boolean threeDSecureConfiguration = null;
		Boolean advancedFraudTools = null;
		Boolean isSkip3dSecureLiabilityResult = null;
		String creditCardStatementName = null;
		String merchantAccountIdForCurrentSite = null;
		String brainTreeChannel = null;
		boolean storeInVault = false;
		String shipsFromPostalCode = null;
		String venmoProfileId = null;

		LOG.info("prepareAuthorizationRequest, order number: " + cart.getCode());
		LOG.info(
				"cart.totalPrice: " + cart.getTotalPrice() + ", authAmount: " + authAmount + ", total tax: " + cart.getTotalTax());

		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			methodNonce = paymentInfo.getNonce();
			deviceData = paymentInfo.getDeviceData();
			paymentType = paymentInfo.getPaymentProvider();
			usePaymentMethodToken = paymentInfo.getUsePaymentMethodToken();
			paymentMethodToken = paymentInfo.getPaymentMethodToken();
			if (PAYPAL_INTENT_ORDER.equals(getBrainTreeConfigService().getIntent()))
			{
				storeInVault = false;
			}
			else
			{
				storeInVault = getBrainTreeConfigService().getStoreInVaultIgnoringIntent();
			}

			if (paymentInfo.getLiabilityShifted() != null)
			{
				liabilityShifted = paymentInfo.getLiabilityShifted();
			}
			if (BraintreeConstants.VENMO_CHECKOUT.equalsIgnoreCase(paymentType) &&
					StringUtils.isNotBlank(getBrainTreeConfigService().getVenmoProfileId()))
			{
				venmoProfileId = getBrainTreeConfigService().getVenmoProfileId();
			}
			threeDSecureConfiguration =  paymentInfo.getThreeDSecureConfiguration();
			advancedFraudTools = paymentInfo.getAdvancedFraudTools();
			isSkip3dSecureLiabilityResult = paymentInfo.getIsSkip3dSecureLiabilityResult();
			creditCardStatementName = paymentInfo.getCreditCardStatementName();
			merchantAccountIdForCurrentSite = paymentInfo.getMerchantAccountIdForCurrentSite();
			brainTreeChannel = paymentInfo.getBrainTreeChannel();
			shipsFromPostalCode = paymentInfo.getShipsFromPostalCode();
		}

		final AddressModel shippingAddress;
		shippingAddress = cart.getDeliveryAddress();

		final BillingInfo shippingInfo = billingAddressConverter.convert(shippingAddress);

		final BrainTreeAuthorizationRequest authorizationRequest = new BrainTreeAuthorizationRequest(paymentInfo.getCode(), null,
				getCurrencyInstanceByCart(cart), authAmount, shippingInfo);
		authorizationRequest.setMethodNonce(methodNonce);
		authorizationRequest.setDeviceData(deviceData);
		authorizationRequest.setPaymentType(paymentType);
		authorizationRequest.setUsePaymentMethodToken(usePaymentMethodToken);
		authorizationRequest.setSubmitForSettlement(submitForSettlement);
		authorizationRequest.setPaymentMethodToken(paymentMethodToken);
		authorizationRequest.setLiabilityShifted(liabilityShifted);
		authorizationRequest.setCustomerId(braintreeCustomerId);
		authorizationRequest.setThreeDSecureConfiguration(threeDSecureConfiguration);
		authorizationRequest.setAdvancedFraudTools(advancedFraudTools);
		authorizationRequest.setIsSkip3dSecureLiabilityResult(isSkip3dSecureLiabilityResult);
		authorizationRequest.setCreditCardStatementName(creditCardStatementName);
		authorizationRequest.setVenmoProfileId(venmoProfileId);

		setAddressInPaymentInfo(authorizationRequest, shippingAddress, (paymentInfo.isCreateNewTransaction() || paymentInfo.isIsDepositPayment()) ? paymentInfo : cart.getPaymentInfo());

		if (StringUtils.isNotEmpty(merchantAccountIdForCurrentSite))
		{
			final BrainTreeFindMerchantAccountRequest brainTreeFindMerchantAccountRequest = new BrainTreeFindMerchantAccountRequest(
					StringUtils.EMPTY);
			brainTreeFindMerchantAccountRequest.setMerchantAccount(merchantAccountIdForCurrentSite);
			if (brainTreePaymentService.findMerchantAccount(brainTreeFindMerchantAccountRequest).isMerchantAccountExist())
			{
				authorizationRequest.setMerchantAccountIdForCurrentSite(merchantAccountIdForCurrentSite);
			}
		}
		authorizationRequest.setBrainTreeChannel(brainTreeChannel);
		authorizationRequest.setStoreInVault(storeInVault);
		if (customFields.size() > 0)
		{
			authorizationRequest.setCustomFields(customFields);
		}

		//		add Level 2 data
		authorizationRequest.setPurchaseOrderNumber(cart.getCode());
		BigDecimal authPercentage = BigDecimal.ZERO;

		//Added condition for modifyPayment with zero order total (full Gift Card Order)
		if(authAmount != null && (cart.getTotalPrice().compareTo(ZERO_PRICE) == ZERO )) {
			//		calc taxAmount via percentage of AUTH-amount
			 authPercentage = roundNumberToTwoDecimalPlaces(
					authAmount.doubleValue() * CONVERT_TO_PERCENTAGE / (cart.getGrandTotal().doubleValue()));
			LOG.info("authPercentage: " + authPercentage + ", as double: " + authPercentage.doubleValue());
		}else{
				if(authAmount != null) {
					//		calc taxAmount via percentage of AUTH-amount
					authPercentage = roundNumberToTwoDecimalPlaces(
							authAmount.doubleValue() * CONVERT_TO_PERCENTAGE / (cart.getTotalPrice().doubleValue()));
					LOG.info("authPercentage: " + authPercentage + ", as double: " + authPercentage.doubleValue());
				}

		}
		BigDecimal taxSupposed = roundNumberToTwoDecimalPlaces(
				cart.getTotalTax().doubleValue() * authPercentage.doubleValue() / CONVERT_TO_PERCENTAGE);
		LOG.info("taxSupposed: " + taxSupposed);
		authorizationRequest.setTaxAmountAuthorize(taxSupposed.doubleValue());

		//		add Level 3 data
		LOG.info("cart.getDeliveryCost: " + cart.getDeliveryCost());
		authorizationRequest.setShippingAmount(cart.getDeliveryCost());

		LOG.info("shipsFromPostalCode: " + shipsFromPostalCode);
		authorizationRequest.setShipsFromPostalCode(shipsFromPostalCode);

		setDeliveryAddress(cart.getDeliveryAddress(), authorizationRequest);

		setLineItem(cart, paymentInfo, authorizationRequest);

		return authorizationRequest;
	}

	/**
	 * It sets the delivery address  in auth request
	 * @param deliveryAddress
	 * @param authorizationRequest
	 */
	private void setDeliveryAddress(final AddressModel deliveryAddress,
			final BrainTreeAuthorizationRequest authorizationRequest) {
		if (deliveryAddress != null)
		{
			LOG.info("Delivery PostalCode: " + deliveryAddress.getPostalcode());
			authorizationRequest.setShippingPostalCode(deliveryAddress.getPostalcode());

			if (deliveryAddress.getCountry() != null)
			{
				LOG.info("Delivery CountryISO: " + deliveryAddress.getCountry().getIsocode());
				String alpha3Country = new Locale("en", deliveryAddress.getCountry().getIsocode()).getISO3Country();
				authorizationRequest.setShippingCountryCodeAlpha3(alpha3Country);
			}
		}
	}

	/**
	 * It sets the address in auth request
	 * @param authorizationRequest
	 * @param shippingAddress
	 * @param paymentInfo
	 */
	private void setAddressInPaymentInfo(final BrainTreeAuthorizationRequest authorizationRequest, final AddressModel
			shippingAddress, final PaymentInfoModel paymentInfo) {
		if (shippingAddress != null)
		{
			authorizationRequest.setBrainTreeAddressId(shippingAddress.getBrainTreeAddressId());
		}

		if (paymentInfo.getBillingAddress() != null)
		{
			final AddressModel billingAddress = paymentInfo.getBillingAddress();
			authorizationRequest.setBrainTreeBilligAddressId(billingAddress.getBrainTreeAddressId());
			final BillingInfo billingInfo = billingAddressConverter.convert(billingAddress);
			authorizationRequest.setBillingInfo(billingInfo);
		}
	}

	/**
	 * It sets the order entries in auth reqest
	 * @param cart
	 * @param paymentInfo
	 * @param authorizationRequest
	 */
	private void setLineItem(final AbstractOrderModel cart, final BrainTreePaymentInfoModel paymentInfo,
			final BrainTreeAuthorizationRequest authorizationRequest) {
		List<BrainTreeLineItemBean> lineItems = new ArrayList<>();
		boolean enableLevel2Level3Data = getBrainTreeConfigService().getConfigurationService().getConfiguration()
				.getBoolean(PROPERTY_LEVEL2_LEVEL3);
		LOG.info("cart.getDiscounts: " + cart.getDiscounts());
		double orderDiscountAmountSum = 0d;
		for (DiscountModel dm : cart.getDiscounts())
		{
			LOG.info("discountString: " + dm.getDiscountString() + ", name: " + dm.getName() + ", value: " + dm.getValue());
			orderDiscountAmountSum = Double.sum(orderDiscountAmountSum, Math.abs(dm.getValue().doubleValue()));
		}
		LOG.info("orderDiscountAmountSum: " + orderDiscountAmountSum);
		authorizationRequest.setDiscountAmount(orderDiscountAmountSum);
		for (AbstractOrderEntryModel entry : cart.getEntries())
		{
			BrainTreeLineItemBean lineItem = new BrainTreeLineItemBean();

			String name = entry.getProduct().getName();
			LOG.info("LineItem name: " + name);

			if (paymentInfo instanceof BrainTreePaymentInfoModel)
			{
				String paymentProvider = paymentInfo.getPaymentProvider();
				if (enableLevel2Level3Data && (!PAY_PAL_EXPRESS_CHECKOUT.equals(paymentProvider) || !PAYPAL_PAYMENT.equals(paymentProvider)))
				{
					name = StringUtils.abbreviate(name, MAX_PRODUCT_NAME_FOR_NON_PAYPAL_WITH_LEVEL2LEVEL3_DATA);
				}
			}

			lineItem.setName(name);
			Long qty = entry.getQuantity();
			lineItem.setQuantity(new BigDecimal(qty.longValue()));
			double unitAmount = entry.getBasePrice().doubleValue();
			lineItem.setUnitAmount(roundNumberToTwoDecimalPlaces(unitAmount));

			String unitOfMeasure = entry.getUnit().getUnitType();
			lineItem.setUnitOfMeasure(unitOfMeasure);
			double totalAmountLineItem = entry.getTotalPrice().doubleValue();
			lineItem.setTotalAmount(roundNumberToTwoDecimalPlaces(totalAmountLineItem));

			double itemTaxAmountSum = 0d;
			for (TaxValue taxValue : entry.getTaxValues())
			{
				LOG.info("---> taxValue isAbsolute: " + taxValue.isAbsolute() + ", value: " + taxValue.getValue() + ", appliedValue: "
						+ taxValue.getAppliedValue() + ", toString: " + taxValue);
				itemTaxAmountSum = Double.sum(itemTaxAmountSum, Math.abs(taxValue.getAppliedValue()));
			}
			lineItem.setTaxAmount(roundNumberToTwoDecimalPlaces(itemTaxAmountSum));

			double itemDiscountAmountSum = 0d;
			for (DiscountValue discountValue : entry.getDiscountValues())
			{
				LOG.info("discountValue.isAbsolute: " + discountValue.isAbsolute() + ", isAsTargetPrice: " + discountValue
						.isAsTargetPrice());
				LOG.info("discountValue.getValue: " + discountValue.getValue());
				LOG.info("discountValue.getAppliedValue: " + discountValue.getAppliedValue());

				itemDiscountAmountSum = Double.sum(itemDiscountAmountSum, Math.abs(discountValue.getAppliedValue()));
			}
			lineItem.setDiscountAmount(roundNumberToTwoDecimalPlaces(itemDiscountAmountSum));

			orderDiscountAmountSum = Double.sum(itemDiscountAmountSum, orderDiscountAmountSum);
			LOG.info("orderDiscountAmountSum: " + orderDiscountAmountSum);
			authorizationRequest.setDiscountAmount(orderDiscountAmountSum);

			String productCode = entry.getProduct().getCode();
			lineItem.setProductCode(productCode);

			String commodityCode = "N/A";
			lineItem.setCommodityCode(commodityCode);

			lineItems.add(lineItem);
		}
		authorizationRequest.setLineItems(lineItems);
	}

	private BigDecimal roundNumberToTwoDecimalPlaces(final double number)
	{
		return BigDecimal.valueOf(number).setScale(DEFAULT_CURRENCY_DIGIT, RoundingMode.HALF_UP);
	}

	protected BigDecimal calculateTotalAmount(final AbstractOrderModel cart)
	{
		return BigDecimal.valueOf(cart.getTotalPrice().doubleValue());
	}

	@Override
	public boolean createPaymentMethodTokenForOrderReplenishment()
	{
		brainTreePaymentService.createPaymentMethodTokenForOrderReplenishment();
		return createAuthorizationTransaction();
	}

	@Override
	public PaymentTransactionEntryModel createAuthorizationTransaction(final AbstractOrderModel cart,
			Map<String, String> customFields, BigDecimal totalAmount)
	{
		PaymentTransactionEntryModel transactionEntry = null;
		final BrainTreeAuthorizationResult result = brainTreeAuthorize(cart, customFields,
				getTotalAmount(cart, totalAmount), Boolean.FALSE, null);

		transactionEntry = createTransactionEntry(PaymentTransactionType.AUTHORIZATION, cart, result, null);

		if (!result.isSuccess())
		{
			transactionEntry.setTransactionStatus(TransactionStatus.REJECTED.name());
			transactionEntry.setTransactionStatusDetails(TransactionStatusDetails.BANK_DECLINE.name());
		}

		savePaymentTransaction(transactionEntry, cart);
		brainTreePaymentService.updatePaymentInfo(cart.getPaymentInfo(), result.getCreditCard());
		return transactionEntry;
	}

	@Override
	public PaymentTransactionEntryModel createAuthorizationTransaction(final OrderModel order, BigDecimal totalAmount)
	{
		final BrainTreeAuthorizationResult result = brainTreeAuthorize(order, getCustomFields(),
				getTotalAmount(order, totalAmount), Boolean.TRUE, null);

		return createCaptureTransactionEntry(order, result, null);
	}

	private PaymentTransactionEntryModel createCaptureTransactionEntry(final AbstractOrderModel order,
			final BrainTreeAuthorizationResult result, final BrainTreePaymentInfoModel paymentInfo) {
		final PaymentTransactionEntryModel transactionEntry = createTransactionEntry(
				PaymentTransactionType.CAPTURE, order, result, paymentInfo);

		if (!result.isSuccess())
		{
			transactionEntry.setTransactionStatus(TransactionStatus.REJECTED.name());
			transactionEntry.setTransactionStatusDetails(TransactionStatusDetails.BANK_DECLINE.name());
		}

		savePaymentTransaction(transactionEntry, order, paymentInfo);

		changeOrderStatusForPayment(order, paymentInfo);

		return transactionEntry;
	}

  /**
   * Change order status for payment.
   *
   * @param order the order
   * @param paymentInfo the payment info
   */
  private void changeOrderStatusForPayment(final AbstractOrderModel order, final BrainTreePaymentInfoModel paymentInfo)
  {
    if (Objects.isNull(paymentInfo) || (!paymentInfo.isCreateNewTransaction() && !paymentInfo.isIsDepositPayment()))
    {
      if (getBrainTreePaymentTransactionService().isOrderFullyCaptured(order))
      {

        getBrainTreePaymentTransactionService().setOrderStatus(order, OrderStatus.PAYMENT_CAPTURED);
      }
      else
      {
        getBrainTreePaymentTransactionService().setOrderStatus(order, OrderStatus.PARTIAL_CAPTURE);
      }
    }
  }

	private BigDecimal getTotalAmount(AbstractOrderModel order, BigDecimal totalAmount)
	{
		if (totalAmount != null)
		{
			return totalAmount;
		}
		return calculateTotalAmount(order);
	}

	@Override
	public PaymentTransactionEntryModel createAuthorizationTransaction(final AbstractOrderModel cart)
	{
		return createAuthorizationTransaction(cart, getCustomFields(), null);
	}

	@Override
	public PaymentTransactionEntryModel createAuthorizationTransaction(final AbstractOrderModel cart,
			Map<String, String> customFields)
	{
		return createAuthorizationTransaction(cart, customFields, null);
	}

	@Override
	public PaymentTransactionEntryModel createCancelTransaction(final PaymentTransactionModel transaction,
			final BrainTreeVoidResult voidResult)
	{
		final PaymentTransactionType transactionType = PaymentTransactionType.CANCEL;
		final String newEntryCode = paymentService.getNewPaymentTransactionEntryCode(transaction, transactionType);

		final PaymentTransactionEntryModel entry = modelService.create(PaymentTransactionEntryModel.class);
		entry.setType(transactionType);
		entry.setCode(newEntryCode);
		entry.setRequestId(voidResult.getRequestId());
		entry.setPaymentTransaction(transaction);
		entry.setCurrency(resolveCurrency(voidResult.getCurrencyIsoCode()));
		entry.setAmount(formatAmount(voidResult.getAmount()));
		entry.setTransactionStatus(voidResult.getTransactionStatus().toString());
		entry.setTransactionStatusDetails(voidResult.getTransactionStatusDetails().toString());
		entry.setTime(new Date());
		modelService.saveAll(entry, transaction);

		return entry;
	}

	@Override
	public PaymentTransactionEntryModel createCancelTransaction(final PaymentTransactionModel transaction,
			final BraintreeTransactionEntryData transactionEntryData)
	{
		final PaymentTransactionType transactionType = PaymentTransactionType.CANCEL;
		final String newEntryCode = paymentService.getNewPaymentTransactionEntryCode(transaction, transactionType);

		final PaymentTransactionEntryModel entry = modelService.create(PaymentTransactionEntryModel.class);
		entry.setType(transactionType);
		entry.setCode(newEntryCode);
		entry.setRequestId(transactionEntryData.getId());
		entry.setPaymentTransaction(transaction);
		entry.setCurrency(resolveCurrency(transactionEntryData.getCurrencyIsoCode()));
		entry.setAmount(formatAmount(transactionEntryData.getTotal()));
		entry.setTransactionStatus(TransactionStatus.ACCEPTED.name());
		entry.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL.name());
		entry.setTime(new Date());
		modelService.saveAll(entry, transaction);

		return entry;
	}

	@Override
	public PaymentTransactionEntryModel createRefundTransaction(final PaymentTransactionModel transaction,
			final BrainTreeRefundTransactionResult result)
	{
		final PaymentTransactionType transactionType = PaymentTransactionType.REFUND_STANDALONE;
		return createRefundTransaction(transaction, result, transactionType);
	}

	@Override
	public PaymentTransactionEntryModel createPartialRefundTransaction(final PaymentTransactionModel transaction,
			final BrainTreeRefundTransactionResult result)
	{
		final PaymentTransactionType transactionType = PaymentTransactionType.REFUND_PARTIAL;
		return createRefundTransaction(transaction, result, transactionType);
	}

	private PaymentTransactionEntryModel createRefundTransaction(final PaymentTransactionModel transaction,
			final BrainTreeRefundTransactionResult result, final PaymentTransactionType transactionType)
	{
		final String newEntryCode = paymentService.getNewPaymentTransactionEntryCode(transaction, transactionType);

		final PaymentTransactionEntryModel entry = modelService.create(PaymentTransactionEntryModel.class);
		entry.setType(transactionType);
		entry.setCode(newEntryCode);
		entry.setRequestId(result.getTransactionId());
		entry.setPaymentTransaction(transaction);
		entry.setCurrency(resolveCurrency(result.getCurrencyIsoCode()));
		entry.setAmount(formatAmount(result.getAmount()));
		entry.setTransactionStatus(result.getTransactionStatus().toString());
		entry.setTransactionStatusDetails(result.getTransactionStatusDetails().toString());
		entry.setTime(new Date());
		modelService.saveAll(entry, transaction);
		return entry;
	}

	@Override
	public PaymentTransactionEntryModel createPartialCaptureTransaction(final PaymentTransactionModel transaction,
			final BrainTreeSaleTransactionResult result)
	{
		final PaymentTransactionType transactionType = PaymentTransactionType.PARTIAL_CAPTURE;
		final String newEntryCode = paymentService.getNewPaymentTransactionEntryCode(transaction, transactionType);

		final PaymentTransactionEntryModel entry = modelService.create(PaymentTransactionEntryModel.class);
		entry.setType(transactionType);
		entry.setCode(newEntryCode);
		entry.setRequestId(result.getTransactionId());
		entry.setPaymentTransaction(transaction);
		entry.setCurrency(resolveCurrency(result.getCurrencyIsoCode()));
		entry.setAmount(formatAmount(result.getAmount()));
		entry.setTransactionStatus(result.getTransactionStatus().toString());
		entry.setTransactionStatusDetails(result.getTransactionStatusDetails().toString());
		entry.setTime(new Date());
		modelService.saveAll(entry, transaction);
		return entry;
	}

	@Override
	public PaymentTransactionEntryModel createSubmitForSettlementTransaction(PaymentTransactionModel transaction,
			BrainTreeSubmitForSettlementTransactionResult result)
	{
		final PaymentTransactionType transactionType = PaymentTransactionType.CAPTURE;
		final String newEntryCode = paymentService.getNewPaymentTransactionEntryCode(transaction, transactionType);

		final PaymentTransactionEntryModel entry = modelService.create(PaymentTransactionEntryModel.class);
		entry.setType(transactionType);
		entry.setCode(newEntryCode);
		entry.setRequestId(result.getTransactionId());
		entry.setPaymentTransaction(transaction);
		entry.setCurrency(resolveCurrency(result.getTransaction().getCurrencyIsoCode()));
		entry.setAmount(formatAmount(result.getTransaction().getAmount()));
		entry.setTransactionStatus(result.getTransactionStatus().toString());
		entry.setTransactionStatusDetails(result.getTransactionStatusDetails().toString());
		entry.setTime(new Date());
		modelService.saveAll(entry, transaction);
		return entry;
	}

	protected CurrencyModel resolveCurrency(final String currencyIsoCode)
	{
		return getCommonI18NService().getCurrency(currencyIsoCode);
	}

	protected Currency getCurrencyInstanceByCart(final AbstractOrderModel cart)
	{
		return Currency.getInstance(cart.getCurrency().getIsocode());
	}

	private BigDecimal formatAmount(final BigDecimal amount)
	{
		return amount.setScale(getCurrencyDigit(), RoundingMode.HALF_EVEN);
	}

	private void savePaymentTransaction(final PaymentTransactionEntryModel paymentTransactionEntry)
	{
		final CartModel cart = cartService.getSessionCart();
		savePaymentTransaction(paymentTransactionEntry, cart);
	}

	private void savePaymentTransaction(final PaymentTransactionEntryModel paymentTransactionEntry, final AbstractOrderModel cart)
	{
	  savePaymentTransaction(paymentTransactionEntry, cart, null);
	}
	
	private void savePaymentTransaction(final PaymentTransactionEntryModel paymentTransactionEntry, final AbstractOrderModel cart, 
	    final BrainTreePaymentInfoModel brainTreePaymentInfoModel)
	{
	  final List<PaymentTransactionModel> paymentTransactions;
    List<PaymentTransactionEntryModel> paymentTransactionEntrys;
    PaymentTransactionModel braintreePaymentTransaction = null;

    modelService.refresh(cart);
    
    paymentTransactions = getPaymentTransactionsList(cart, brainTreePaymentInfoModel);

    braintreePaymentTransaction =
        getBraintreePaymentTransaction(paymentTransactionEntry, cart, brainTreePaymentInfoModel, paymentTransactions, braintreePaymentTransaction);    

    if (braintreePaymentTransaction == null)
    {
      braintreePaymentTransaction = modelService.create(PaymentTransactionModel.class);
      braintreePaymentTransaction.setRequestId(paymentTransactionEntry.getRequestId());
      braintreePaymentTransaction.setPaymentProvider(BRAINTREE_PROVIDER_NAME);
      braintreePaymentTransaction.setPlannedAmount(formatAmount(paymentTransactionEntry.getAmount()));

      if(Objects.nonNull(brainTreePaymentInfoModel)) {
      	setTransactionTypeForAdditionalPayment(brainTreePaymentInfoModel, braintreePaymentTransaction);
			}
      if(isPaymentForDeposit(brainTreePaymentInfoModel) || (Objects.nonNull(brainTreePaymentInfoModel)
					&& brainTreePaymentInfoModel.isCreateNewTransaction()))
      {
        braintreePaymentTransaction.setRequestToken(brainTreePaymentInfoModel.getNonce());
        braintreePaymentTransaction.setInfo(brainTreePaymentInfoModel);
      }
      else if (cart.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
      {
        BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
        braintreePaymentTransaction.setRequestToken(paymentInfo.getNonce());
        braintreePaymentTransaction.setInfo(paymentInfo);
      }
      paymentTransactions.add(braintreePaymentTransaction);
    }

    if (CollectionUtils.isNotEmpty(braintreePaymentTransaction.getEntries()))
    {
      paymentTransactionEntrys = Lists.newArrayList(braintreePaymentTransaction.getEntries());
    }
    else
    {
      paymentTransactionEntrys = Lists.newArrayList();
    }
    
    paymentTransactionEntrys.add(paymentTransactionEntry);
    braintreePaymentTransaction.setEntries(paymentTransactionEntrys);
    if(isPaymentForDeposit(brainTreePaymentInfoModel))
    {
      cart.setDepositPaymentTransactions(paymentTransactions);
      final ArrayList<PaymentInfoModel> paymentInfos = Lists.newArrayList(CollectionUtils.emptyIfNull(cart.getDepositPaymentInfo()));
      paymentInfos.add(brainTreePaymentInfoModel);
      cart.setDepositPaymentInfo(paymentInfos);
    }
    else
    {
      cart.setPaymentTransactions(paymentTransactions);
    }
    modelService.saveAll(paymentTransactionEntry, braintreePaymentTransaction, cart);
	}

	/**
	 * It sets the transaction type for every additional payment
	 * @param brainTreePaymentInfoModel braintree payment info model
	 * @param braintreePaymentTransaction braintree payment transaction
	 */
	private void setTransactionTypeForAdditionalPayment(final BrainTreePaymentInfoModel brainTreePaymentInfoModel,
			final PaymentTransactionModel braintreePaymentTransaction) {
  	if(brainTreePaymentInfoModel.isBillPayment()) {
  		braintreePaymentTransaction.setTransactionType(PaymentTransactionTypeEnum.BILL_PAYMENT);
		} else if(brainTreePaymentInfoModel.isModifyPayment()) {
  		braintreePaymentTransaction.setTransactionType(PaymentTransactionTypeEnum.MODIFY_PAYMENT);
		} else if(brainTreePaymentInfoModel.isExtendOrder()) {
  		braintreePaymentTransaction.setTransactionType(PaymentTransactionTypeEnum.EXTEND_ORDER);
		}
	}

	/**
   * Gets the braintree payment transaction.
   *
   * @param paymentTransactionEntry the payment transaction entry
   * @param cart the cart
   * @param brainTreePaymentInfoModel the brain tree payment info model
   * @param paymentTransactions the payment transactions
   * @param braintreePaymentTransaction the braintree payment transaction
   * @return the braintree payment transaction
   */
  private PaymentTransactionModel getBraintreePaymentTransaction(final PaymentTransactionEntryModel paymentTransactionEntry,
      final AbstractOrderModel cart, final BrainTreePaymentInfoModel brainTreePaymentInfoModel,
      final List<PaymentTransactionModel> paymentTransactions, PaymentTransactionModel braintreePaymentTransaction)
  {
    if ((Objects.nonNull(brainTreePaymentInfoModel) && brainTreePaymentInfoModel.isCreateNewTransaction()) || isPaymentForDeposit(brainTreePaymentInfoModel))
    {
      braintreePaymentTransaction = null;
    }
    else
    {
      for (PaymentTransactionModel transaction : paymentTransactions)
      {
        if (BRAINTREE_PROVIDER_NAME.equals(transaction.getPaymentProvider()))
        {
          braintreePaymentTransaction = getPaymentTransactionForOrder(paymentTransactionEntry, cart, paymentTransactions, transaction);
          break;
        }
      }
    }
    return braintreePaymentTransaction;
  }

  /**
   * Gets the payment transaction for order.
   *
   * @param paymentTransactionEntry the payment transaction entry
   * @param cart the cart
   * @param paymentTransactions the payment transactions
   * @param transaction the transaction
   * @return the payment transaction for order
   */
  private PaymentTransactionModel getPaymentTransactionForOrder(final PaymentTransactionEntryModel paymentTransactionEntry,
      final AbstractOrderModel cart, final List<PaymentTransactionModel> paymentTransactions, PaymentTransactionModel transaction)
  {
    PaymentTransactionModel braintreePaymentTransaction;
    if (transaction.getRequestId().equals(FAKE_REQUEST_ID))
    {
      transaction.setRequestId(paymentTransactionEntry.getRequestId());
      transaction.setPaymentProvider(BRAINTREE_PROVIDER_NAME);
      transaction.setPlannedAmount(formatAmount(paymentTransactionEntry.getAmount()));

      if (cart.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
      {
        BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
        transaction.setRequestToken(paymentInfo.getNonce());
        transaction.setInfo(paymentInfo);
      }
      paymentTransactions.add(transaction);
    }
    braintreePaymentTransaction = transaction;
    return braintreePaymentTransaction;
  }

  /**
   * Gets the payment transactions list.
   *
   * @param cart the cart
   * @param brainTreePaymentInfoModel the brain tree payment info model
   * @return the payment transactions list
   */
  private List<PaymentTransactionModel> getPaymentTransactionsList(final AbstractOrderModel cart,
      final BrainTreePaymentInfoModel brainTreePaymentInfoModel)
  {
    final List<PaymentTransactionModel> paymentTransactions;
    if (isPaymentForDeposit(brainTreePaymentInfoModel))
    {
      if (CollectionUtils.isNotEmpty(cart.getDepositPaymentTransactions()))
      {
        paymentTransactions = Lists.newArrayList(cart.getDepositPaymentTransactions());
      }
      else
      {
        paymentTransactions = Lists.newArrayList();
      }
    }
    else
    {
      if (!cart.getPaymentTransactions().isEmpty())
      {
        paymentTransactions = Lists.newArrayList(cart.getPaymentTransactions());
      }
      else
      {
        paymentTransactions = Lists.newArrayList();
      }
    }
    return paymentTransactions;
  }

  /**
   * Checks if is payment for deposit.
   *
   * @param brainTreePaymentInfoModel the brain tree payment info model
   * @return true, if is payment for deposit
   */
  private boolean isPaymentForDeposit(final BrainTreePaymentInfoModel brainTreePaymentInfoModel)
  {
    return Objects.nonNull(brainTreePaymentInfoModel) && brainTreePaymentInfoModel.isIsDepositPayment();
  }

	private PaymentTransactionEntryModel createTransactionEntry(final PaymentTransactionType type, final AbstractOrderModel cart,
			final BrainTreeAuthorizationResult result, BrainTreePaymentInfoModel paymentInfo)
	{
		PaymentTransactionEntryModel paymentTransactionEntry = null;
		
		for (PaymentTransactionModel paymentTransactionModel : cart.getPaymentTransactions()) {
      for (PaymentTransactionEntryModel paymentTransactionEntryModel : paymentTransactionModel
          .getEntries()) {
        if (paymentTransactionEntryModel.getRequestId().equals(FAKE_REQUEST_ID)) {
          paymentTransactionEntry = paymentTransactionEntryModel;
          break;
        }
      }
    }
		
		if(isPaymentForDeposit(paymentInfo))
		{
		  paymentTransactionEntry = null;
		}

		if (paymentTransactionEntry == null)
		{
			paymentTransactionEntry = modelService.create(PaymentTransactionEntryModel.class);
		}

		if(null == paymentInfo) {
			paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
		}
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			paymentTransactionEntry.setRequestToken(paymentInfo.getNonce());
		}

		paymentTransactionEntry.setType(type);
		paymentTransactionEntry.setTransactionStatus(result.getTransactionStatus().toString());

		String statusDetails = "";
		if (null == result.getAuthorizationExpiresAt())
		{
			statusDetails = result.getTransactionStatusDetails().toString();
		}
		else
		{
			statusDetails =
					result.getTransactionStatusDetails().toString() + ", Expires at: " + result.getAuthorizationExpiresAt().getTime();
		}
		paymentTransactionEntry.setTransactionStatusDetails(statusDetails);

		paymentTransactionEntry.setRequestId(result.getRequestId());
		if (null != result.getCurrency())
		{
			paymentTransactionEntry.setCurrency(resolveCurrency(result.getCurrency().getCurrencyCode()));
		}


		final String code = BRAINTREE_PROVIDER_NAME + "_cart_" + cart.getCode() + "_stamp_" + System.currentTimeMillis();
		paymentTransactionEntry.setCode(code);

		paymentTransactionEntry.setAmount(formatAmount(result.getTotalAmount()));
		paymentTransactionEntry.setTime(result.getAuthorizationTime());

		return paymentTransactionEntry;
	}

	private int getCurrencyDigit()
	{
		final CurrencyModel currency = commonI18NService.getCurrentCurrency();
		if (currency != null)
		{
			final Integer digits = currency.getDigits();
			return digits != null ? digits.intValue() : DEFAULT_CURRENCY_DIGIT;
		}
		return DEFAULT_CURRENCY_DIGIT;
	}

	@Override
	public BrainTreePaymentInfoModel createSubscription(final AddressModel billingAddress, final CustomerModel customer,
			final BraintreeInfo braintreeInfo)
	{
		validateParameterNotNull(braintreeInfo.getNonce(), "nonce cannot be null");
		validateParameterNotNull(billingAddress, "address cannot be null");
		validateParameterNotNull(customer, "customer cannot be null");
		final BrainTreePaymentInfoModel cardPaymentInfoModel = createCreditCardPaymentInfo(billingAddress, customer, braintreeInfo,
				getCartService().getSessionCart());

		billingAddress.setOwner(cardPaymentInfoModel);

		modelService.save(billingAddress);
		modelService.refresh(customer);

		LOG.info("[BT PaymentInfo] Created card payment info with id: " + cardPaymentInfoModel.getCode());
		LOG.info("[BT PaymentInfo] Created billing address with id: " + billingAddress.getPk());

		final List<PaymentInfoModel> paymentInfoModels = new ArrayList<>(customer.getPaymentInfos());

		if (!paymentInfoModels.contains(cardPaymentInfoModel))
		{
			paymentInfoModels.add(cardPaymentInfoModel);
			if (braintreeInfo.isSavePaymentInfo() && !braintreeInfo.isDuplicatedPayment())
			{
				customer.setPaymentInfos(paymentInfoModels);
				getModelService().save(customer);
			}
		}

		setDefaultCard(customer, cardPaymentInfoModel);

		return cardPaymentInfoModel;
	}

	

	@Override
	public BrainTreePaymentInfoModel createSubscription(final AddressModel billingAddress, final CustomerModel customer,
			final BraintreeInfo braintreeInfo, final AbstractOrderModel abstractOrderModel)
	{
		validateParameterNotNull(braintreeInfo.getNonce(), "nonce cannot be null");
		validateParameterNotNull(billingAddress, "address cannot be null");
		validateParameterNotNull(customer, "customer cannot be null");

		final BrainTreePaymentInfoModel cardPaymentInfoModel = createCreditCardPaymentInfo(billingAddress, customer, braintreeInfo,
				abstractOrderModel);

		if(StringUtils.isNotBlank(braintreeInfo.getBraintreeAddressId()))
		{
		  billingAddress.setBrainTreeAddressId(braintreeInfo.getBraintreeAddressId());
		}
		billingAddress.setOwner(cardPaymentInfoModel);
		billingAddress.setPickStoreAddress(Boolean.FALSE);
		billingAddress.setUpsStoreAddress(Boolean.FALSE);
		modelService.save(billingAddress);
		modelService.refresh(customer);

		LOG.info("[BT PaymentInfo] Created card payment info with id: " + cardPaymentInfoModel.getCode());
		LOG.info("[BT PaymentInfo] Created billing address with id: " + billingAddress.getPk());

		final List<PaymentInfoModel> paymentInfoModels = new ArrayList<>(customer.getPaymentInfos());

		if (!paymentInfoModels.contains(cardPaymentInfoModel))
		{
			paymentInfoModels.add(cardPaymentInfoModel);
			if (braintreeInfo.isSavePaymentInfo() && !braintreeInfo.isDuplicatedPayment())
			{
				customer.setPaymentInfos(paymentInfoModels);
				getModelService().save(customer);
			}
		}
		setDefaultCard(customer, cardPaymentInfoModel);

		if(braintreeInfo.isDepositPayment()){
			cardPaymentInfoModel.setIsDepositPayment(Boolean.TRUE);
			cardPaymentInfoModel.setDepositAmount(braintreeInfo.getDepositAmount());
		}

		return cardPaymentInfoModel;
	}

	@Override
	public void createOrderTransaction(AbstractOrderModel cart, BrainTreeCreatePaymentMethodResult result)
	{
	  final PaymentTransactionEntryModel paymentTransactionEntryModel = modelService.create(PaymentTransactionEntryModel.class);
    paymentTransactionEntryModel.setType(PaymentTransactionType.ORDER);
    paymentTransactionEntryModel.setTransactionStatus(TransactionStatus.ACCEPTED.name());
    paymentTransactionEntryModel.setTransactionStatusDetails(TransactionStatusDetails.SUCCESFULL.name());
    paymentTransactionEntryModel.setRequestId(result.getPaymentMethodToken());
    paymentTransactionEntryModel.setTime(new Date());
    paymentTransactionEntryModel.setAmount(new BigDecimal(cart.getTotalPrice())
        .setScale(DEFAULT_CURRENCY_DIGIT, RoundingMode.HALF_UP));
    paymentTransactionEntryModel.setCurrency(cart.getCurrency());
    paymentTransactionEntryModel.setRequestToken(result.getRequestToken());
    final String code = BRAINTREE_PROVIDER_NAME + "_cart_" + cart.getCode() + "_stamp_" + System.currentTimeMillis();
    paymentTransactionEntryModel.setCode(code);

    savePaymentTransactionIntentOrder(paymentTransactionEntryModel, cart);
	}

	private BrainTreePaymentInfoModel createCreditCardPaymentInfo(final AddressModel billingAddress,
			final CustomerModel customerModel, final BraintreeInfo braintreeInfo)
	{
		validateParameterNotNull(billingAddress, "billingAddress cannot be null");

		final BrainTreePaymentInfoModel cardPaymentInfoModel = modelService.create(BrainTreePaymentInfoModel.class);

		resolveBillingAddress(billingAddress, customerModel, braintreeInfo, cardPaymentInfoModel);
		billingAddress.setOwner(cardPaymentInfoModel);
		cardPaymentInfoModel.setBillingAddress(billingAddress);
		if (userService.isAnonymousUser(customerModel))
		{
			cardPaymentInfoModel.setCode(customerModel.getUid());
		}
		else
		{
			cardPaymentInfoModel.setCode(customerModel.getUid() + "_" + UUID.randomUUID());
		}
		cardPaymentInfoModel.setUser(customerModel);
		cardPaymentInfoModel.setPaymentMethodToken(braintreeInfo.getPaymentMethodToken());
		cardPaymentInfoModel.setNonce(braintreeInfo.getNonce());
		cardPaymentInfoModel.setDeviceData(braintreeInfo.getDeviceData());
		cardPaymentInfoModel.setImageSource(braintreeInfo.getImageSource());
		cardPaymentInfoModel.setExpirationMonth(braintreeInfo.getExpirationMonth());
		cardPaymentInfoModel.setExpirationYear(braintreeInfo.getExpirationYear());
		
		//Added check to handle add New CC from payment page
		if(braintreeInfo.getIsDefault() == null) {
			cardPaymentInfoModel.setIsDefault(false);
		}else{
			cardPaymentInfoModel.setIsDefault(braintreeInfo.getIsDefault());
		}

		if (StringUtils.isNotEmpty(customerModel.getBraintreeCustomerId()))
		{
			cardPaymentInfoModel.setCustomerId(customerModel.getBraintreeCustomerId());
		}
		if (braintreeInfo.getLiabilityShifted() != null)
		{
			cardPaymentInfoModel.setLiabilityShifted(braintreeInfo.getLiabilityShifted());
		}
		cardPaymentInfoModel.setPaymentProvider(braintreeInfo.getPaymentProvider());
		cardPaymentInfoModel.setShouldBeSaved(braintreeInfo.isShouldBeSaved());
		cardPaymentInfoModel.setSaved(braintreeInfo.isSavePaymentInfo());
		cardPaymentInfoModel.setUsePaymentMethodToken(StringUtils.isNotBlank(cardPaymentInfoModel.getPaymentMethodToken()));
		if (PAYPAL_PAYMENT.equals(braintreeInfo.getPaymentProvider()) || PAY_PAL_EXPRESS_CHECKOUT
				.equals(braintreeInfo.getPaymentProvider()))
		{
			cardPaymentInfoModel.setDuplicate(braintreeInfo.isDuplicatedPayment());
		}
		else
		{
			// for card payment if user don't want to save card we'll mark card as duplicated, so it will be removed after place order
			// false should be set for replenishment
			cardPaymentInfoModel.setDuplicate(!braintreeInfo.isShouldBeSaved());
		}
		cardPaymentInfoModel.setCardNumber(braintreeInfo.getCardNumber());
		cardPaymentInfoModel.setCardholderName(braintreeInfo.getCardholderName());

		if (isNotEmpty(braintreeInfo.getCardType()))
		{
			final String brainTreeCardType = braintreeInfo.getCardType().replace("_", " ");
			cardPaymentInfoModel.setCardType(getBrainTreeCardTypeByName(brainTreeCardType));
		}

		cardPaymentInfoModel.setThreeDSecureConfiguration(getBrainTreeConfigService().get3dSecureConfiguration());
		cardPaymentInfoModel.setAdvancedFraudTools(getBrainTreeConfigService().getAdvancedFraudTools());
		cardPaymentInfoModel.setIsSkip3dSecureLiabilityResult(getBrainTreeConfigService().getIsSkip3dSecureLiabilityResult());
		cardPaymentInfoModel.setCreditCardStatementName(getBrainTreeConfigService().getCreditCardStatementName());
		cardPaymentInfoModel.setBrainTreeChannel(getBrainTreeConfigService().getBrainTreeChannel());

		return cardPaymentInfoModel;
	}

	private BrainTreePaymentInfoModel createCreditCardPaymentInfo(final AddressModel billingAddress,
			final CustomerModel customerModel, final BraintreeInfo braintreeInfo, final AbstractOrderModel abstractOrderModel)
	{


		BrainTreePaymentInfoModel cardPaymentInfoModel = createCreditCardPaymentInfo(billingAddress, customerModel, braintreeInfo);

		if (!(abstractOrderModel instanceof OrderModel))
		{
			cardPaymentInfoModel
					.setMerchantAccountIdForCurrentSite(getBrainTreeConfigService().getMerchantAccountIdForCurrentSiteAndCurrency());
		}
		else
		{
			BrainTreePaymentInfoModel brainTreePaymentInfoModel = (BrainTreePaymentInfoModel) abstractOrderModel.getPaymentInfo();
			cardPaymentInfoModel.setMerchantAccountIdForCurrentSite(brainTreePaymentInfoModel.getMerchantAccountIdForCurrentSite());
		}
		return cardPaymentInfoModel;
	}

	private void resolveBillingAddress(final AddressModel billingAddress, final CustomerModel customerModel,
			final BraintreeInfo braintreeInfo, BrainTreePaymentInfoModel cardPaymentInfoModel)
	{
		if (CustomerType.GUEST.equals(customerModel.getType()) && (PAYPAL_PAYMENT.equals(braintreeInfo.getPaymentProvider())
				|| BRAINTREE_PAYMENT.equals(braintreeInfo.getPaymentProvider())))
		{
			cardPaymentInfoModel.setPaymentInfo(billingAddress.getEmail());
			final String email = StringUtils.substringAfter(customerModel.getUid(), "|");
			billingAddress.setEmail(StringUtils.isNotEmpty(email) ? email : customerModel.getContactEmail());
		}
	}

	private void savePaymentTransactionIntentOrder(final PaymentTransactionEntryModel paymentTransactionEntry,
			final AbstractOrderModel cart)
	{
		final List<PaymentTransactionModel> paymentTransactions;
		List<PaymentTransactionEntryModel> paymentTransactionEntrys;
		PaymentTransactionModel braintreePaymentTransaction = null;
		paymentTransactions = Lists.newArrayList();

		braintreePaymentTransaction = modelService.create(PaymentTransactionModel.class);
		braintreePaymentTransaction.setRequestId(paymentTransactionEntry.getRequestId());
		braintreePaymentTransaction.setPaymentProvider(BRAINTREE_PROVIDER_NAME);
		braintreePaymentTransaction
				.setPlannedAmount(paymentTransactionEntry.getAmount().setScale(DEFAULT_CURRENCY_DIGIT, RoundingMode.HALF_UP));

		if (cart.getPaymentInfo() != null) {
			BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
			braintreePaymentTransaction.setInfo(paymentInfo);
		}
		modelService.save(braintreePaymentTransaction);
		paymentTransactions.add(braintreePaymentTransaction);

		paymentTransactionEntrys = Lists.newArrayList();

		paymentTransactionEntrys.add(paymentTransactionEntry);
		braintreePaymentTransaction.setEntries(paymentTransactionEntrys);
		cart.setPaymentTransactions(paymentTransactions);

		modelService.saveAll(paymentTransactionEntry, braintreePaymentTransaction, cart);
	}

	protected BrainTreeCardType getBrainTreeCardTypeByName(final String brainTreeCardType)
	{
		return BrainTreeCardType.valueOf(brainTreeCardType);
	}

	private Map<String, String> getCustomFields()
	{
		return customFieldsService.getDefaultCustomFieldsMap();
	}

	/**
	 * It gets the custom fields
	 * @param order the order model
	 * @return custom fields
	 */
	private Map<String, String> getCustomFields(final AbstractOrderModel order)
	{
		return customFieldsService.getDefaultCustomFieldsMap(order);
	}
	
	/**
	 * This method is used to set Default card 
	 * @param customer
	 * @param cardPaymentInfoModel
	 */
	private void setDefaultCard(final CustomerModel customer, final BrainTreePaymentInfoModel cardPaymentInfoModel) {
		if (BooleanUtils.isTrue(cardPaymentInfoModel.isIsDefault()))
		{
			getCustomerAccountService().setDefaultPaymentInfo(customer, cardPaymentInfoModel);
		}
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
	 * @return the userService
	 */
	public UserService getUserService()
	{
		return userService;
	}

	/**
	 * @param userService the userService to set
	 */
	public void setUserService(final UserService userService)
	{
		this.userService = userService;
	}

	/**
	 * @return the brainTreePaymentService
	 */
	public BrainTreePaymentService getBrainTreePaymentService()
	{
		return brainTreePaymentService;
	}

	/**
	 * @param brainTreePaymentService the brainTreePaymentService to set
	 */
	public void setBrainTreePaymentService(final BrainTreePaymentService brainTreePaymentService)
	{
		this.brainTreePaymentService = brainTreePaymentService;
	}

	public BrainTreePaymentTransactionService getBrainTreePaymentTransactionService() {
		return brainTreePaymentTransactionService;
	}

	public void setBrainTreePaymentTransactionService(BrainTreePaymentTransactionService brainTreePaymentTransactionService) {
		this.brainTreePaymentTransactionService = brainTreePaymentTransactionService;
	}

	/**
	 * @return the paymentService
	 */
	public PaymentService getPaymentService()
	{
		return paymentService;
	}

	/**
	 * @param paymentService the paymentService to set
	 */
	public void setPaymentService(final PaymentService paymentService)
	{
		this.paymentService = paymentService;
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

	/**
	 * @return the commonI18NService
	 */
	public CommonI18NService getCommonI18NService()
	{
		return commonI18NService;
	}

	/**
	 * @param commonI18NService the commonI18NService to set
	 */
	public void setCommonI18NService(final CommonI18NService commonI18NService)
	{
		this.commonI18NService = commonI18NService;
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

	public CustomFieldsService getCustomFieldsService()
	{
		return customFieldsService;
	}

	public void setCustomFieldsService(CustomFieldsService customFieldsService)
	{
		this.customFieldsService = customFieldsService;
	}

	public BraintreeSubmitForSettlementService getBraintreeSubmitForSettlementService() {
		return braintreeSubmitForSettlementService;
	}

	public void setBraintreeSubmitForSettlementService(
			BraintreeSubmitForSettlementService braintreeSubmitForSettlementService) {
		this.braintreeSubmitForSettlementService = braintreeSubmitForSettlementService;
	}

	public CustomerAccountService getCustomerAccountService() {
		return customerAccountService;
	}

	public void setCustomerAccountService(CustomerAccountService customerAccountService) {
		this.customerAccountService = customerAccountService;
	}

  @Override
  public boolean doCapturePaymentForModifiedOrder(final AbstractOrderModel orderModel, final BigDecimal amount, final boolean submitForSettlement,
      final BrainTreePaymentInfoModel paymentInfo) throws BraintreeErrorException
  {
    BigDecimal amountRefunded = amount;
    if (Objects.nonNull(orderModel) && (StringUtils.isNotBlank(orderModel.getPoNumber()) || CollectionUtils.isNotEmpty(orderModel.getPaymentTransactions())))
    {
      BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Amount to Capture : {} for Order : {}", amountRefunded.doubleValue(), orderModel.getCode());
      return createAuthorizationTransactionOfOrder(orderModel, amountRefunded, submitForSettlement, paymentInfo);
    }
    return Boolean.FALSE;
  }
  
  @Override
  public boolean initiateRefundProcess(final AbstractOrderModel orderModel, final BigDecimal amountToRefund)
  {
    Validate.notNull(orderModel, ORDER_MUST_NOT_BE_NULL, StringUtils.EMPTY);
    Validate.notNull(amountToRefund, "Amount Must not be null", StringUtils.EMPTY);
    try
    {
      BigDecimal remainingRefundAmount = amountToRefund;
      if(CollectionUtils.isNotEmpty(orderModel.getPaymentTransactions()))
      {
        for(final PaymentTransactionModel paymentTransaction : orderModel.getPaymentTransactions())
        {
          if(remainingRefundAmount.compareTo(BigDecimal.ZERO) == 0)
          {
            return Boolean.TRUE;
          }
          remainingRefundAmount = doRefund(orderModel, paymentTransaction, remainingRefundAmount);
        }
      }
      if(remainingRefundAmount.compareTo(BigDecimal.ZERO) == 0)
      {
        return Boolean.TRUE;
      }
    }
    catch(final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
          "Error Occurred while refunding order : {} for Amount : {}", orderModel.getCode(), amountToRefund);
    }
    return Boolean.FALSE;
  }
  
  /**
   * Do refund.
   *
   * @param orderModel the order model
   * @param paymentTransaction the payment transaction
   * @param amountToRefund the amount to refund
   * @return true, if successful
   */
  private BigDecimal doRefund(final AbstractOrderModel orderModel, final PaymentTransactionModel paymentTransaction, final BigDecimal amountToRefund)
  {
    BigDecimal refundToMake = amountToRefund;
    try
    {      
      if(CollectionUtils.isNotEmpty(paymentTransaction.getEntries()))
      {
        for(PaymentTransactionEntryModel paymentTransactionEntryModel : paymentTransaction.getEntries())
        {
          refundToMake = performRefund(orderModel, paymentTransaction, amountToRefund, refundToMake, paymentTransactionEntryModel);
          if(refundToMake.compareTo(BigDecimal.ZERO) == 0)
          {
            break;
          }
        }  
      }
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
          "Error Occurred while refunding order : {} for Amount : {} on transaction with code : {}", orderModel.getCode(), amountToRefund.doubleValue(),
          paymentTransaction.getCode());
    }
    return refundToMake;
  }

  /**
   * Perform refund.
   *
   * @param orderModel the order model
   * @param paymentTransaction the payment transaction
   * @param amountToRefund the amount to refund
   * @param refundToMake the refund to make
   * @param paymentTransactionEntryModel the payment transaction entry model
   * @return the big decimal
   */
  private BigDecimal performRefund(final AbstractOrderModel orderModel, final PaymentTransactionModel paymentTransaction,
      final BigDecimal amountToRefund, BigDecimal refundToMake, PaymentTransactionEntryModel paymentTransactionEntryModel)
  {
    BrainTreeRefundTransactionResult result;
    BigDecimal remainingAmountRefund = BigDecimal.ZERO;
    try
    {
      if (getBraintreePartialRefundService().eligibleForPartialRefund(paymentTransactionEntryModel))
      {
        remainingAmountRefund = getRemainingAmountRefund(BigDecimal.ZERO, paymentTransactionEntryModel);
        
        if(remainingAmountRefund.compareTo(BigDecimal.ZERO) == 0)
        {
          return refundToMake;
        }
        if (refundToMake.compareTo(remainingAmountRefund) <= 0)
        {
          result = getBraintreePartialRefundService().partialRefundTransaction(((OrderModel) orderModel), paymentTransactionEntryModel, refundToMake);
          BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Performed Refund for Amount : {} on transaction entry with code : {}",
              refundToMake.doubleValue(), paymentTransactionEntryModel.getCode());
          if (Objects.nonNull(result) && result.isSuccess())
          {
            refundToMake = BigDecimal.ZERO;
          }
        }
        else
        {
          result =
              getBraintreePartialRefundService().partialRefundTransaction(((OrderModel) orderModel), paymentTransactionEntryModel, remainingAmountRefund);
          BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Performed Refund for Amount : {} on transaction entry with code : {}",
              remainingAmountRefund.doubleValue(), paymentTransactionEntryModel.getCode());
          if (Objects.nonNull(result) && result.isSuccess())
          {
            refundToMake = refundToMake.subtract(remainingAmountRefund);
          }
        }
      }
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
          "Error Occurred while refunding order : {} for Amount : {} on transaction entry with code : {} with Message : {}", orderModel.getCode(),
          amountToRefund.doubleValue(), paymentTransaction.getCode(), exception.getMessage());
    }
    return refundToMake;
  }

  /**
   * Check refund possibility and get amount.
   *
   * @param paymentTransaction the payment transaction
   * @return the big decimal
   */
  private BigDecimal checkRefundPossibilityAndGetAmount(final PaymentTransactionModel paymentTransaction)
  {
    BigDecimal capturedAmount = BigDecimal.ZERO;
    if (CollectionUtils.isNotEmpty(paymentTransaction.getEntries()))
    {
      for (PaymentTransactionEntryModel transactionEntryModel : paymentTransaction.getEntries())
      {
        capturedAmount = getRemainingAmountRefund(capturedAmount, transactionEntryModel);
      }
    }
    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Amount Remaining to Refund : {} for PaymentTransactionModel : {}", capturedAmount.doubleValue(),
        paymentTransaction.getCode());
    return capturedAmount;
  }

  /**
   * Gets the remaining amount refund from Captured Transaction Entry Model.
   *
   * @param capturedAmount the captured amount
   * @param transactionEntryModel the transaction entry model
   * @return the remaining amount refund
   */
  private BigDecimal getRemainingAmountRefund(BigDecimal capturedAmount, final PaymentTransactionEntryModel transactionEntryModel)
  {
    if (getBraintreePartialRefundService().eligibleForPartialRefund(transactionEntryModel))
    {
      if (Objects.nonNull(transactionEntryModel.getRefundedAmount()) && transactionEntryModel.getRefundedAmount().compareTo(BigDecimal.ZERO) > 0)
      {
        capturedAmount = capturedAmount.add(transactionEntryModel.getAmount().subtract(transactionEntryModel.getRefundedAmount()));
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Amount in PaymentTransactionEntryModel with Code : {} is {} and Refunded Amount is {}",
            transactionEntryModel.getCode(), transactionEntryModel.getAmount().doubleValue(),
            transactionEntryModel.getRefundedAmount().doubleValue());
      }
      else
      {
        capturedAmount = capturedAmount.add(transactionEntryModel.getAmount());
        BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Amount in PaymentTransactionEntryModel with Code : {} is {}",
            transactionEntryModel.getCode(), transactionEntryModel.getAmount().doubleValue());
      }
    }
    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total Remaining Amount to Refund is {} in PaymentTransactionEntryModel with Code : {}",
        capturedAmount.doubleValue(),transactionEntryModel.getCode());
    return capturedAmount;
  }
  
  @Override
  public BigDecimal getRemainingAmountToRefund(final AbstractOrderModel order)
  {
    Validate.notNull(order, ORDER_MUST_NOT_BE_NULL, StringUtils.EMPTY);
    BigDecimal remainingAmoutToRefund = BigDecimal.ZERO;
    if(CollectionUtils.isNotEmpty(order.getPaymentTransactions()))
    {
      for(final PaymentTransactionModel paymentTransaction : order.getPaymentTransactions())
      {
        remainingAmoutToRefund = remainingAmoutToRefund.add(checkRefundPossibilityAndGetAmount(paymentTransaction));
      }
    }
    BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Total remaining amount to refund  is {} for order with Code : {}",
        remainingAmoutToRefund.doubleValue(), order.getCode());
    return remainingAmoutToRefund;
  }
  
  @Override
  public boolean doModifiedOrderPoPayment(final AbstractOrderModel order, final String poNumber, final String poNote, final BigDecimal poAmount)
  {
    try
    {
      Validate.notNull(order, ORDER_MUST_NOT_BE_NULL, StringUtils.EMPTY);
      Validate.notNull(poAmount, "Po Amount Must not be null", StringUtils.EMPTY);
      Validate.notBlank(poNumber, "Po Number must not be blank", StringUtils.EMPTY);

      order.setModifiedOrderPoNumber(poNumber);
      order.setModifiedOrderPoNotes(poNote);
      order.setModifiedOrderPoAmount(poAmount.doubleValue());
      getModelService().save(order);
      getModelService().refresh(order);
      BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Payment for Modified Order : {} with Po number : {} and Po Amount : {} is done",
          order.getCode(), poNumber, poAmount);
      return Boolean.TRUE;
    }
    catch (final Exception exception)
    {
      BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
          "Error Occurred while making payment with PO number : {} on modified order : {} for Amount : {}", poNumber, order, poAmount);
    }
    return Boolean.FALSE;
  }

  /**
   * @return the braintreePartialRefundService
   */
  public BraintreePartialRefundService getBraintreePartialRefundService()
  {
    return braintreePartialRefundService;
  }

  /**
   * @param braintreePartialRefundService the braintreePartialRefundService to set
   */
  public void setBraintreePartialRefundService(BraintreePartialRefundService braintreePartialRefundService)
  {
    this.braintreePartialRefundService = braintreePartialRefundService;
  }
	
	
}
