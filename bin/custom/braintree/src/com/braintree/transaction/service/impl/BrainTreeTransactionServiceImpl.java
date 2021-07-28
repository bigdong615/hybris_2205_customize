package com.braintree.transaction.service.impl;

import static com.braintree.constants.BraintreeConstants.BRAINTREE_PAYMENT;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_PROVIDER_NAME;
import static com.braintree.constants.BraintreeConstants.FAKE_REQUEST_ID;
import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_ORDER;
import static com.braintree.constants.BraintreeConstants.PAYPAL_PAYMENT;
import static com.braintree.constants.BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT;
import static com.braintree.constants.BraintreeConstants.PROPERTY_LEVEL2_LEVEL3;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNull;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;
import static org.apache.commons.lang.StringUtils.isNotEmpty;

import com.bl.logging.BlLogger;
import com.braintree.command.request.BrainTreeAuthorizationRequest;
import com.braintree.command.request.BrainTreeCreatePaymentMethodRequest;
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
import com.braintree.order.submitForSettlement.service.BraintreeSubmitForSettlementService;
import com.braintree.payment.dto.BraintreeInfo;
import com.braintree.paypal.converters.impl.BillingAddressConverter;
import com.braintree.transaction.service.BrainTreePaymentTransactionService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.braintreegateway.PayPalAccount;
import com.google.common.collect.Lists;

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
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.PaymentService;
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
import java.util.Currency;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;



public class BrainTreeTransactionServiceImpl implements BrainTreeTransactionService
{

	private static final Logger LOG = Logger.getLogger(BrainTreeTransactionServiceImpl.class);

	private final static int DEFAULT_CURRENCY_DIGIT = 2;
	private final static int MAX_PRODUCT_NAME_FOR_NON_PAYPAL_WITH_LEVEL2LEVEL3_DATA = 35;

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
			final BrainTreeAuthorizationResult result = brainTreeAuthorize(cart, customFields,
				getBrainTreeConfigService().getAuthAMountToVerifyCard(), Boolean.FALSE);
			return handleAuthorizationResult(result, cart);
		} catch(final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR,
				"Error occurred while creating authorization for the cart {} while placing an order", cart.getCode(), ex);
		}
		return false;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean createAuthorizationTransactionOfOrder(final AbstractOrderModel orderModel)
	{
		try {
			final BrainTreeAuthorizationResult result = brainTreeAuthorize(orderModel, getCustomFields(),
					getTotalAmount(orderModel, null), Boolean.FALSE);
			return handleAuthorizationResult(result, orderModel);
		} catch(final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR,
					"Error occurred while creating authorization for the order {}", orderModel.getCode(), ex);
		}
			return false;
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

	private boolean handleAuthorizationResult(BrainTreeAuthorizationResult result, AbstractOrderModel cart)
	{
		PaymentTransactionEntryModel paymentTransactionEntry = null;
		if (result.isSuccess())
		{
			paymentTransactionEntry = createTransactionEntry(PaymentTransactionType.AUTHORIZATION, cart, result);
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
			BigDecimal totalAmount, final Boolean submitForSettlement)
	{

		final CustomerModel customer = (CustomerModel) cart.getUser();

		BrainTreeAuthorizationRequest authorizationRequest = prepareAuthorizationRequest(cart, customer, customFields,
				totalAmount, submitForSettlement);

		final BrainTreeAuthorizationResult brainTreeAuthorizationResult = (BrainTreeAuthorizationResult) brainTreePaymentService
				.authorize(authorizationRequest, customer);

		if(!StringUtils.isBlank(brainTreeAuthorizationResult.getPaymentMethodToken())){
			savePaymentMethodFromBTByToken(brainTreeAuthorizationResult.getPaymentMethodToken(), cart);
		}

		return brainTreeAuthorizationResult;
	}

	private void savePaymentMethodFromBTByToken(final String paymentMethodToken, final AbstractOrderModel cart)
	{
		final PayPalAccount paymentMethod = brainTreePaymentService.getPaymentMethodFromBTByToken(paymentMethodToken);

		brainTreePaymentService.updatePaymentInfo(cart.getPaymentInfo(), paymentMethod);

		LOG.warn(paymentMethod);

	}

	private BrainTreeAuthorizationRequest prepareAuthorizationRequest(AbstractOrderModel cart, CustomerModel customer,
			Map<String, String> customFields, BigDecimal authAmount, final Boolean submitForSettlement)
	{
		final PaymentInfoModel paymentInfo = cart.getPaymentInfo();

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

		LOG.error("prepareAuthorizationRequest, order number: " + cart.getCode());
		LOG.error(
				"cart.totalPrice: " + cart.getTotalPrice() + ", authAmount: " + authAmount + ", total tax: " + cart.getTotalTax());

		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			methodNonce = ((BrainTreePaymentInfoModel) paymentInfo).getNonce();
			deviceData = ((BrainTreePaymentInfoModel) paymentInfo).getDeviceData();
			paymentType = ((BrainTreePaymentInfoModel) paymentInfo).getPaymentProvider();
			usePaymentMethodToken = ((BrainTreePaymentInfoModel) paymentInfo).getUsePaymentMethodToken();
			paymentMethodToken = ((BrainTreePaymentInfoModel) paymentInfo).getPaymentMethodToken();
			if (PAYPAL_INTENT_ORDER.equals(getBrainTreeConfigService().getIntent()))
			{
				storeInVault = false;
			}
			else
			{
				storeInVault = getBrainTreeConfigService().getStoreInVaultIgnoringIntent();
			}

			if (((BrainTreePaymentInfoModel) paymentInfo).getLiabilityShifted() != null)
			{
				liabilityShifted = ((BrainTreePaymentInfoModel) paymentInfo).getLiabilityShifted();
			}
			if (BraintreeConstants.VENMO_CHECKOUT.equalsIgnoreCase(paymentType) &&
					StringUtils.isNotBlank(getBrainTreeConfigService().getVenmoProfileId()))
			{
				venmoProfileId = getBrainTreeConfigService().getVenmoProfileId();
			}
			threeDSecureConfiguration = ((BrainTreePaymentInfoModel) paymentInfo).getThreeDSecureConfiguration();
			advancedFraudTools = ((BrainTreePaymentInfoModel) paymentInfo).getAdvancedFraudTools();
			isSkip3dSecureLiabilityResult = ((BrainTreePaymentInfoModel) paymentInfo).getIsSkip3dSecureLiabilityResult();
			creditCardStatementName = ((BrainTreePaymentInfoModel) paymentInfo).getCreditCardStatementName();
			merchantAccountIdForCurrentSite = ((BrainTreePaymentInfoModel) paymentInfo).getMerchantAccountIdForCurrentSite();
			brainTreeChannel = ((BrainTreePaymentInfoModel) paymentInfo).getBrainTreeChannel();
			shipsFromPostalCode = ((BrainTreePaymentInfoModel) paymentInfo).getShipsFromPostalCode();
		}

		final AddressModel shippingAddress;
		final AddressModel billingAddress;
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

		if (shippingAddress != null)
		{
			authorizationRequest.setBrainTreeAddressId(shippingAddress.getBrainTreeAddressId());
		}

		if (cart.getPaymentInfo().getBillingAddress() != null)
		{
			billingAddress = cart.getPaymentInfo().getBillingAddress();
			authorizationRequest.setBrainTreeBilligAddressId(billingAddress.getBrainTreeAddressId());
			final BillingInfo billingInfo = billingAddressConverter.convert(billingAddress);
			authorizationRequest.setBillingInfo(billingInfo);
		}

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

		//		calc taxAmount via percentage of AUTH-amount
		BigDecimal authPercentage = roundNumberToTwoDecimalPlaces(
				authAmount.doubleValue() * 100 / (cart.getTotalPrice().doubleValue()));
		LOG.info("authPercentage: " + authPercentage + ", as double: " + authPercentage.doubleValue());

		BigDecimal taxSupposed = roundNumberToTwoDecimalPlaces(
				cart.getTotalTax().doubleValue() * authPercentage.doubleValue() / 100);
		LOG.info("taxSupposed: " + taxSupposed);
		authorizationRequest.setTaxAmountAuthorize(new Double(taxSupposed.doubleValue()));

		//		add Level 3 data
		LOG.info("cart.getDeliveryCost: " + cart.getDeliveryCost());
		authorizationRequest.setShippingAmount(cart.getDeliveryCost());

		LOG.info("cart.getDiscounts: " + cart.getDiscounts());
		Double discountAmount = new Double(0d);
		double orderDiscountAmountSum = 0d;
		for (DiscountModel dm : cart.getDiscounts())
		{
			LOG.error("discountString: " + dm.getDiscountString() + ", name: " + dm.getName() + ", value: " + dm.getValue());
			orderDiscountAmountSum = Double.sum(orderDiscountAmountSum, Math.abs(dm.getValue().doubleValue()));
		}
		LOG.info("orderDiscountAmountSum: " + orderDiscountAmountSum);
		authorizationRequest.setDiscountAmount(new Double(orderDiscountAmountSum));

		LOG.info("shipsFromPostalCode: " + shipsFromPostalCode);
		authorizationRequest.setShipsFromPostalCode(shipsFromPostalCode);

		if (cart.getDeliveryAddress() != null)
		{
			LOG.info("Delivery PostalCode: " + cart.getDeliveryAddress().getPostalcode());
			authorizationRequest.setShippingPostalCode(cart.getDeliveryAddress().getPostalcode());

			if (cart.getDeliveryAddress().getCountry() != null)
			{
				LOG.info("Delivery CountryISO: " + cart.getDeliveryAddress().getCountry().getIsocode());
				String alpha3Country = new Locale("en", cart.getDeliveryAddress().getCountry().getIsocode()).getISO3Country();
				authorizationRequest.setShippingCountryCodeAlpha3(alpha3Country);
			}
		}


		List<BrainTreeLineItemBean> lineItems = new ArrayList<>();
		boolean enableLevel2Level3Data = getBrainTreeConfigService().getConfigurationService().getConfiguration()
				.getBoolean(PROPERTY_LEVEL2_LEVEL3);
		for (AbstractOrderEntryModel entry : cart.getEntries())
		{
			BrainTreeLineItemBean lineItem = new BrainTreeLineItemBean();

			String name = entry.getProduct().getName();
			LOG.info("LineItem name: " + name);

			if (paymentInfo instanceof BrainTreePaymentInfoModel)
			{
				String paymentProvider = ((BrainTreePaymentInfoModel) paymentInfo).getPaymentProvider();
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
			authorizationRequest.setDiscountAmount(new Double(orderDiscountAmountSum));

			String productCode = entry.getProduct().getCode();
			lineItem.setProductCode(productCode);

			String commodityCode = "N/A";
			lineItem.setCommodityCode(commodityCode);

			lineItems.add(lineItem);
		}
		authorizationRequest.setLineItems(lineItems);

		return authorizationRequest;
	}

	private BigDecimal roundNumberToTwoDecimalPlaces(final double number)
	{
		return new BigDecimal(number).setScale(2, RoundingMode.HALF_UP);
	}

	protected BigDecimal calculateTotalAmount(final AbstractOrderModel cart)
	{
		return BigDecimal.valueOf(cart.getTotalPrice().doubleValue());
	}

	@Override
	public boolean createPaymentMethodTokenForOrderReplenishment()
	{
		final CustomerModel customer = checkoutCustomerStrategy.getCurrentUserForCheckout();
		final PaymentInfoModel paymentInfo = cartService.getSessionCart().getPaymentInfo();
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			if (!((BrainTreePaymentInfoModel) paymentInfo).getUsePaymentMethodToken().booleanValue())
			{
				final BrainTreeCreatePaymentMethodRequest request = new BrainTreeCreatePaymentMethodRequest(null,
						((BrainTreePaymentInfoModel) paymentInfo).getNonce(), customer.getBraintreeCustomerId());

				final BrainTreeCreatePaymentMethodResult result = brainTreePaymentService.createPaymentMethod(request);
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

		return createAuthorizationTransaction();

	}

	@Override
	public PaymentTransactionEntryModel createAuthorizationTransaction(final AbstractOrderModel cart,
			Map<String, String> customFields, BigDecimal totalAmount)
	{
		PaymentTransactionEntryModel transactionEntry = null;
		final BrainTreeAuthorizationResult result = brainTreeAuthorize(cart, customFields,
				getTotalAmount(cart, totalAmount), Boolean.FALSE);

		transactionEntry = createTransactionEntry(PaymentTransactionType.AUTHORIZATION, cart, result);

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
		PaymentTransactionEntryModel transactionEntry = null;
		final BrainTreeAuthorizationResult result = brainTreeAuthorize(order, getCustomFields(),
				getTotalAmount(order, totalAmount), Boolean.TRUE);

		transactionEntry = createTransactionEntry(PaymentTransactionType.CAPTURE, order, result);

		if (!result.isSuccess())
		{
			transactionEntry.setTransactionStatus(TransactionStatus.REJECTED.name());
			transactionEntry.setTransactionStatusDetails(TransactionStatusDetails.BANK_DECLINE.name());
		}

		savePaymentTransaction(transactionEntry, order);

		if (getBrainTreePaymentTransactionService().isOrderFullyCaptured(order))
		{
			getBrainTreePaymentTransactionService().setOrderStatus(order, OrderStatus.PAYMENT_CAPTURED);
			getBrainTreePaymentTransactionService().continueOrderProcess(order);
		}
		else
		{
			getBrainTreePaymentTransactionService().setOrderStatus(order, OrderStatus.PARTIAL_CAPTURE);
		}

		return transactionEntry;
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
		final List<PaymentTransactionModel> paymentTransactions;
		List<PaymentTransactionEntryModel> paymentTransactionEntrys;
		PaymentTransactionModel braintreePaymentTransaction = null;

		modelService.refresh(cart);
		if (!cart.getPaymentTransactions().isEmpty())
		{
			paymentTransactions = Lists.newArrayList(cart.getPaymentTransactions());
		}
		else
		{
			paymentTransactions = Lists.newArrayList();
		}

		for (PaymentTransactionModel transaction : paymentTransactions)
		{
			if (BRAINTREE_PROVIDER_NAME.equals(transaction.getPaymentProvider()))
			{
				if (transaction.getRequestId().equals(FAKE_REQUEST_ID))
				{
					transaction.setRequestId(paymentTransactionEntry.getRequestId());
					transaction.setPaymentProvider(BRAINTREE_PROVIDER_NAME);
					transaction.setPlannedAmount(formatAmount(paymentTransactionEntry.getAmount()));

					if (cart.getPaymentInfo() != null && cart.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
					{
						BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
						transaction.setRequestToken(paymentInfo.getNonce());
						transaction.setInfo(paymentInfo);
					}
					paymentTransactions.add(transaction);
				}
				braintreePaymentTransaction = transaction;
				break;
			}
		}

		if (braintreePaymentTransaction == null)
		{
			braintreePaymentTransaction = modelService.create(PaymentTransactionModel.class);
			braintreePaymentTransaction.setRequestId(paymentTransactionEntry.getRequestId());
			braintreePaymentTransaction.setPaymentProvider(BRAINTREE_PROVIDER_NAME);
			braintreePaymentTransaction.setPlannedAmount(formatAmount(paymentTransactionEntry.getAmount()));

			if (cart.getPaymentInfo() != null && cart.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
			{
				BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
				braintreePaymentTransaction.setRequestToken(paymentInfo.getNonce());
				braintreePaymentTransaction.setInfo(paymentInfo);
			}
			paymentTransactions.add(braintreePaymentTransaction);
		}

		if (braintreePaymentTransaction.getEntries() != null)
		{
			paymentTransactionEntrys = Lists.newArrayList(braintreePaymentTransaction.getEntries());
		}
		else
		{
			paymentTransactionEntrys = Lists.newArrayList();
		}

		paymentTransactionEntrys.add(paymentTransactionEntry);
		braintreePaymentTransaction.setEntries(paymentTransactionEntrys);
		cart.setPaymentTransactions(paymentTransactions);

		modelService.saveAll(paymentTransactionEntry, braintreePaymentTransaction, cart);
	}

	private PaymentTransactionEntryModel createTransactionEntry(final PaymentTransactionType type, final AbstractOrderModel cart,
			final BrainTreeAuthorizationResult result)
	{
		PaymentTransactionEntryModel paymentTransactionEntry = null;
		for (PaymentTransactionModel paymentTransactionModel : cart.getPaymentTransactions())
		{
			if (!paymentTransactionModel.getEntries().isEmpty())
			{
				for (PaymentTransactionEntryModel paymentTransactionEntryModel : paymentTransactionModel.getEntries())
				{
					if (paymentTransactionEntryModel.getRequestId().equals(FAKE_REQUEST_ID))
					{
						paymentTransactionEntry = paymentTransactionEntryModel;
						break;
					}
				}
			}
		}

		if (paymentTransactionEntry == null)
		{
			paymentTransactionEntry = modelService.create(PaymentTransactionEntryModel.class);
		}

		final PaymentInfoModel paymentInfo = cart.getPaymentInfo();
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			paymentTransactionEntry.setRequestToken(((BrainTreePaymentInfoModel) paymentInfo).getNonce());
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

		final List<PaymentInfoModel> paymentInfoModels = new ArrayList<PaymentInfoModel>(customer.getPaymentInfos());

		if (!paymentInfoModels.contains(cardPaymentInfoModel))
		{
			paymentInfoModels.add(cardPaymentInfoModel);
			if (braintreeInfo.isSavePaymentInfo() && !braintreeInfo.isDuplicatedPayment())
			{
				customer.setPaymentInfos(paymentInfoModels);
				getModelService().save(customer);
			}
		}

		if (cardPaymentInfoModel != null && BooleanUtils.isTrue(cardPaymentInfoModel.isIsDefault()))
		{
			getCustomerAccountService().setDefaultPaymentInfo(customer, cardPaymentInfoModel);
		}

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

		final List<PaymentInfoModel> paymentInfoModels = new ArrayList<PaymentInfoModel>(customer.getPaymentInfos());

		if (!paymentInfoModels.contains(cardPaymentInfoModel))
		{
			paymentInfoModels.add(cardPaymentInfoModel);
			if (braintreeInfo.isSavePaymentInfo() && !braintreeInfo.isDuplicatedPayment())
			{
				customer.setPaymentInfos(paymentInfoModels);
				getModelService().save(customer);
			}
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
		paymentTransactionEntryModel.setAmount(new BigDecimal(cart.getTotalPrice()).setScale(2, RoundingMode.HALF_UP));
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
		cardPaymentInfoModel.setIsDefault(braintreeInfo.getIsDefault());

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

		if (braintreePaymentTransaction == null)
		{
			braintreePaymentTransaction = modelService.create(PaymentTransactionModel.class);
			braintreePaymentTransaction.setRequestId(paymentTransactionEntry.getRequestId());
			braintreePaymentTransaction.setPaymentProvider(BRAINTREE_PROVIDER_NAME);
			braintreePaymentTransaction.setPlannedAmount(paymentTransactionEntry.getAmount().setScale(2, RoundingMode.HALF_UP));

			if (cart.getPaymentInfo() != null && cart.getPaymentInfo() instanceof BrainTreePaymentInfoModel)
			{
				BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
				braintreePaymentTransaction.setInfo(paymentInfo);
			}
			modelService.save(braintreePaymentTransaction);
			paymentTransactions.add(braintreePaymentTransaction);
		}

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

	
	
	
}
