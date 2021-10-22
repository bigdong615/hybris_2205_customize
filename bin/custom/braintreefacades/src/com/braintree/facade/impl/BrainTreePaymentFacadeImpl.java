package com.braintree.facade.impl;

import static com.braintree.constants.BraintreeConstants.ANDROID_PAY_CARD;
import static com.braintree.constants.BraintreeConstants.APPLE_PAY_CARD;
import static com.braintree.constants.BraintreeConstants.BRAINTREE_CREDITCARD_PAYMENT;
import static com.braintree.constants.BraintreeConstants.CARD_NUMBER_MASK;
import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_ORDER;
import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_SALE;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;
import static org.apache.commons.lang.StringUtils.isNotEmpty;

import com.bl.logging.BlLogger;
import com.braintree.command.result.BrainTreeCreatePaymentMethodResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.customer.service.BrainTreeCustomerAccountService;
import com.braintree.enums.BrainTreePaymentMethod;
import com.braintree.exceptions.BraintreeCreditCardValidationException;
import com.braintree.facade.BrainTreeUserFacade;
import com.braintree.hybris.data.BrainTreePaymentInfoData;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.model.BraintreeLocalPaymentMethodsModel;
import com.braintree.payment.dto.BraintreeInfo;
import com.braintree.payment.local.methods.service.BraintreeLocalPaymentMethodsService;
import com.braintree.paypal.converters.impl.BillingAddressConverter;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.braintreegateway.WebhookNotification;
import de.hybris.platform.acceleratorfacades.payment.impl.DefaultPaymentFacade;
import de.hybris.platform.braintree.data.BrainTreeWebhookNotificationRequest;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commerceservices.customer.CustomerEmailResolutionService;
import de.hybris.platform.commerceservices.order.CommerceCartService;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.store.BaseStoreModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

public class BrainTreePaymentFacadeImpl extends DefaultPaymentFacade
{
	private static final String CVV_MATCH_CODE = "M";
  private final static Logger LOG = Logger.getLogger(BrainTreePaymentFacadeImpl.class);
	private BrainTreeUserFacade brainTreeUserFacade;
	private BrainTreePaymentService brainTreePaymentService;
	private CartService cartService;
	private BaseStoreService baseStoreService;
	private BrainTreeCustomerAccountService brainTreeCustomerAccountService;
	private CommerceCartService commerceCartService;
	private ModelService modelService;
	private UserService userService;
	private CustomerEmailResolutionService customerEmailResolutionService;
	private BrainTreeTransactionService brainTreeTransactionService;

	private Converter<BrainTreePaymentInfoModel, BrainTreePaymentInfoData> brainTreePaymentInfoDataConverter;
	private Converter<AddressModel, AddressData> addressConverter;
	private Converter<AddressData, AddressModel> addressReverseConverter;
	private BillingAddressConverter billingAddressConverter;
	private Converter<BrainTreeSubscriptionInfoData, BraintreeInfo> brainTreeSubscriptionInfoConverter;
	private BrainTreeConfigService brainTreeConfigService;
	private BraintreeLocalPaymentMethodsService braintreeLocalPaymentMethodsService;
	private Converter<OrderModel, OrderData> orderConverter;

	public BrainTreeSubscriptionInfoData buildVenmoSubscriptionInfo(final String nonce, final String paymentProvider,
			final boolean shouldBeSaved, final String email, final String deviceData)
	{
		final BrainTreeSubscriptionInfoData subscriptionInfo = new BrainTreeSubscriptionInfoData();
		final AddressModel billingAddress = getModelService().clone(getCartService().getSessionCart().getDeliveryAddress());
		subscriptionInfo.setPaymentProvider(paymentProvider);
		subscriptionInfo.setDeviceData(deviceData);
		subscriptionInfo.setShouldBeSaved(shouldBeSaved);
		subscriptionInfo.setNonce(nonce);
		subscriptionInfo.setEmail(email);
		subscriptionInfo.setAddressData(addressConverter.convert(billingAddress));

		return subscriptionInfo;
	}

	public void createLocalPaymentMethodSubscription(String paymentId)
	{
		final CartModel cart = getCartService().getSessionCart();
		completeCreateLocalPaymentSubscription(paymentId, cart);

	}

	public void updateLocalPaymentMethodSubscription(final String nonce, final String deviceData, final String payerEmail)
	{
		validateParameterNotNullStandardMessage("nonce", nonce);
		validateParameterNotNullStandardMessage("deviceData", deviceData);
		validateParameterNotNullStandardMessage("payerEmail", payerEmail);

		PaymentInfoModel paymentInfo = getCartService().getSessionCart().getPaymentInfo();
		AddressModel billingAddress = getModelService().clone(getCartService().getSessionCart().getDeliveryAddress());

		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{
			((BrainTreePaymentInfoModel) paymentInfo).setPayer(payerEmail);
			((BrainTreePaymentInfoModel) paymentInfo).setNonce(nonce);
			((BrainTreePaymentInfoModel) paymentInfo).setDeviceData(deviceData);
			billingAddress.setOwner(paymentInfo);
			paymentInfo.setBillingAddress(billingAddress);
			getModelService().saveAll(paymentInfo, billingAddress);
		}
	}


	private void completeCreateLocalPaymentSubscription(String paymentId, CartModel cart)
	{
		validateParameterNotNullStandardMessage("paymentId", paymentId);
		validateParameterNotNullStandardMessage("cart", cart);

		final BrainTreePaymentInfoModel paymentInfo = getModelService().create(BrainTreePaymentInfoModel.class);
		final CustomerModel customerModel = getCurrentUserForCheckout();

		paymentInfo.setUser(customerModel);
		paymentInfo.setCode(customerModel.getUid() + "_" + UUID.randomUUID().toString());
		paymentInfo.setThreeDSecureConfiguration(getBrainTreeConfigService().get3dSecureConfiguration());
		paymentInfo.setPaymentId(paymentId);
		paymentInfo.setUsePaymentMethodToken(false);
		paymentInfo.setPayPalIntent(PAYPAL_INTENT_SALE);
		paymentInfo.setPaymentProvider(BrainTreePaymentMethod.LOCALPAYMENT.toString());
		paymentInfo.setOwner(cart);

		cart.setPaymentInfo(paymentInfo);
		getModelService().saveAll(paymentInfo, cart);
	}

	public void completeCreateSubscription(final BrainTreeSubscriptionInfoData subscriptionInfo)
	{
		final boolean isCreditEnabled = brainTreeConfigService.getCreditEnabled();
		completeCreateSubscription(subscriptionInfo, isCreditEnabled);
	}

	public void completeCreateSubscription(final BrainTreeSubscriptionInfoData subscriptionInfo, final  boolean isCreditEnabled)
	{
		final CustomerModel customer = getCurrentUserForCheckout();
		final CartModel cart = getCartService().getSessionCart();
		completeCreateSubscription(subscriptionInfo, customer, cart, isCreditEnabled, true);
	}

	public BrainTreePaymentInfoModel completeCreateSubscription(final BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData,
			final CustomerModel customer, final AbstractOrderModel cart, final  boolean isCreditEnabled, final boolean isCheckout)
	{
		return completeCreateSubscription(brainTreeSubscriptionInfoData,customer, cart, isCreditEnabled, isCheckout, Boolean.FALSE, Double.valueOf(0.0d));
	}
	
  public BrainTreePaymentInfoModel completeCreateSubscription(final BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData,
      final CustomerModel customer, final AbstractOrderModel cart, final boolean isCreditEnabled, final boolean isCheckout,
      final boolean isDepositPayment, final Double depositAmount)
  {
    return completeCreateSubscription(brainTreeSubscriptionInfoData, customer, cart, isCreditEnabled, isCheckout, isDepositPayment, depositAmount,
        Boolean.FALSE);
  }

	public BrainTreePaymentInfoModel completeCreateSubscription(final BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData,
			final CustomerModel customer, final AbstractOrderModel cart, final  boolean isCreditEnabled, final boolean isCheckout,
			final boolean isDepositPayment, final Double depositAmount, final boolean isModifyOrderPaymentPage)
	{
		BrainTreePaymentInfoModel paymentInfo = null;
		final AddressData addressData = brainTreeSubscriptionInfoData.getAddressData();
		if (isNotEmpty(brainTreeSubscriptionInfoData.getCardNumber())) {
			brainTreeSubscriptionInfoData
					.setCardNumber(String.format(CARD_NUMBER_MASK, brainTreeSubscriptionInfoData.getCardNumber()));
		}

		final AddressModel billingAddress = resolveBillingAddress(addressData, cart, customer, brainTreeSubscriptionInfoData);
		//get value from form if we should show that payment info
		final boolean isStoreInVault = brainTreeConfigService.getVaultingForCurrentUser(brainTreeSubscriptionInfoData.getPaymentProvider());
		BrainTreeCreatePaymentMethodResult result = null;
		// The below code has been commented out because the credit card details needs to be stored in vault //NOSONAR
		//		if (isAvailableCreatingNewPaymentMethod(brainTreeSubscriptionInfoData, isStoreInVault, isCreditEnabled)) { //NOSONAR
		final BraintreeInfo paymentMethodBrainTreeInfo = getBrainTreeSubscriptionInfoConverter().convert(brainTreeSubscriptionInfoData);
		result = getBrainTreePaymentService().createPaymentMethodForCustomer(customer,
				billingAddress, paymentMethodBrainTreeInfo);
		BlLogger.logFormatMessageInfo(LOG, Level.INFO, "Braintree Payment Method Result : {}", result);

		checkBraintreeResult(result);

		addAdditionalPaymentMethodFields(brainTreeSubscriptionInfoData, result);
//		} //NOSONAR
		if (isStoreInVault && StringUtils.isEmpty(customer.getBraintreeCustomerId())) {
			LOG.debug("... creating customer on the braintree side");
			getBrainTreePaymentService().createCustomer(customer, billingAddress);
		} else {
			LOG.debug("... should't create new customer account on the braintree side");
		}
		final BraintreeInfo braintreeInfo = getBrainTreeSubscriptionInfoConverter().convert(brainTreeSubscriptionInfoData);
		boolean isDuplicate = false;
		if (isDuplicateCheckPossible(brainTreeSubscriptionInfoData)) {
			isDuplicate = isPaymentMethodDuplicate(brainTreeSubscriptionInfoData, cart, billingAddress);
		}
		braintreeInfo.setDepositPayment(isDepositPayment);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, " Is Deposit Payment : {}", isDepositPayment);
		braintreeInfo.setDepositAmount(depositAmount);
		BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Deposit Payment Amount : {}", depositAmount);
		paymentInfo = getBrainTreeTransactionService().createSubscription(billingAddress, customer, braintreeInfo, cart);


		if ((cart instanceof OrderModel) && (null != cart.getPaymentInfo())) {
			String value = ((BrainTreePaymentInfoModel) cart.getPaymentInfo()).getShipsFromPostalCode();
			paymentInfo.setShipsFromPostalCode(value);
		}
		paymentInfo.setPayer(brainTreeSubscriptionInfoData.getEmail());
		paymentInfo.setDuplicate(isDuplicate);
		if(isModifyOrderPaymentPage)
		{
			paymentInfo.setModifyPayment(Boolean.TRUE);
		  paymentInfo.setCreateNewTransaction(Boolean.TRUE);
		}
		modelService.save(paymentInfo);
		setPaymentInfoInCart(cart, paymentInfo, isCheckout);
		if ((!isModifyOrderPaymentPage || !paymentInfo.isIsDepositPayment()) && BraintreeConstants.PAYPAL_INTENT_ORDER.equalsIgnoreCase(getBrainTreeConfigService().getIntent())
				&& result != null && isPayPalCheckout(brainTreeSubscriptionInfoData))
		{
			brainTreeTransactionService.createOrderTransaction(cart, result);
		}
		return paymentInfo;
	}

	/**
	 * It saves the cart after setting the payment info
	 * @param cart
	 * @param paymentInfo
	 * @param isCheckout
	 */
	private void setPaymentInfoInCart(AbstractOrderModel cart, BrainTreePaymentInfoModel paymentInfo, boolean isCheckout) {
		if((!paymentInfo.isIsDepositPayment()) && (isCheckout || (cart instanceof OrderModel))) {
			cart.setPaymentInfo(paymentInfo);
			modelService.save(cart);
		}
	}

	private boolean isPayPalCheckout(BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData)
	{
		return BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT.equals(brainTreeSubscriptionInfoData.getPaymentProvider())
				|| BraintreeConstants.PAYPAL_PAYMENT.equals(brainTreeSubscriptionInfoData
				.getPaymentProvider());
	}

	private boolean isVenmoCheckout(BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData)
	{
		return BraintreeConstants.VENMO_CHECKOUT.equals(brainTreeSubscriptionInfoData.getPaymentProvider());
	}

	private boolean isGoogleCheckout(BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData)
	{
		return BraintreeConstants.ANDROID_PAY_CARD.equals(brainTreeSubscriptionInfoData.getPaymentProvider());
	}

	private boolean isAvailableCreatingNewPaymentMethod(BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData,
			boolean isStoreInVault, final  boolean isCreditEnabled)
	{
		final boolean isOrderIntent = brainTreeSubscriptionInfoData.getIntent() != null && brainTreeSubscriptionInfoData.getIntent()
				.equals(PAYPAL_INTENT_ORDER);

		boolean isCreditEnabledForPayPal = isOrderIntent ? false : isCreditEnabled;

		return ((isPayPalCheckout(brainTreeSubscriptionInfoData) && !isCreditEnabledForPayPal) || isVenmoCheckout(brainTreeSubscriptionInfoData)
				|| isGoogleCheckout(brainTreeSubscriptionInfoData)) && (isStoreInVault || isOrderIntent);
	}

	private boolean isDuplicateCheckPossible(BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData)
	{
		if (brainTreeConfigService.getStoreInVaultIgnoringIntent())
		{
			return !brainTreeConfigService.getIntent().equalsIgnoreCase(PAYPAL_INTENT_ORDER)
					|| brainTreeSubscriptionInfoData.getPaymentProvider().equals(BRAINTREE_CREDITCARD_PAYMENT)
					|| isVenmoCheckout(brainTreeSubscriptionInfoData)
					|| isGoogleCheckout(brainTreeSubscriptionInfoData);
		}
		return false;
	}

	public void setPaymentMethodNonce(final CCPaymentInfoData paymentInfoData)
	{
		final String token = paymentInfoData.getPaymentMethodToken();
		if (token != null)
		{
			final String nonce = getBrainTreePaymentService().createPaymentMethodNonce(token);
			paymentInfoData.setPaymentMethodNonce(nonce);
		}
	}

	public void updateLocalPaymentMethodSubscription(final String nonce, final CartModel cart){
		PaymentInfoModel paymentInfo = cart.getPaymentInfo();
		AddressModel billingAddress = getModelService().clone(cart.getDeliveryAddress());
		if (paymentInfo instanceof BrainTreePaymentInfoModel){
			((BrainTreePaymentInfoModel) paymentInfo).setNonce(nonce);
			paymentInfo.setBillingAddress(billingAddress);
			billingAddress.setOwner(paymentInfo);
			getModelService().saveAll(paymentInfo, cart, billingAddress);
		}
	}

	private boolean isPaymentMethodDuplicate(BrainTreeSubscriptionInfoData paymentInfo, AbstractOrderModel cart, AddressModel billingAddress)
	{
		final String paymentProvider = paymentInfo.getPaymentProvider();
		final List<BrainTreePaymentInfoModel> customerPaymentInfos =
				brainTreeCustomerAccountService.getBrainTreePaymentInfos((CustomerModel) cart.getUser(), true)
						.stream()
						.filter(p -> p.getPaymentProvider().equals(paymentProvider))
						.collect(Collectors.toList());

		return customerPaymentInfos.stream()
				.filter(payment -> null != payment.getBillingAddress().getLine1() &&
						payment.getBillingAddress().getLine1().equals(billingAddress.getLine1())
						&& payment.getBillingAddress().getPostalcode().equals(billingAddress.getPostalcode()))
				.anyMatch(payment -> {
					if ((isPayPalCheckout(paymentInfo) || paymentProvider.equals(APPLE_PAY_CARD)
							|| paymentProvider.equals(ANDROID_PAY_CARD))
							&& payment.getPayer().equals(billingAddress.getEmail()))
					{
						return true;
					}
					else if (isVenmoCheckout(paymentInfo) && payment.getPayer().equals(paymentInfo.getEmail()))
					{
						return true;
					}
					else
					{
						if (null != payment.getCardholderName() && null != paymentInfo.getCardholder() &&
								payment.getCardholderName().equals(paymentInfo.getCardholder()))
						{
							return true;
						}
					}
					return false;
				});
	}

	private void checkBraintreeResult(final BrainTreeCreatePaymentMethodResult paymentMethodResult) {
		if (!paymentMethodResult.isSuccess()) {
		  if(StringUtils.isNotBlank(paymentMethodResult.getCvvValidationCode()) && !paymentMethodResult.getCvvValidationCode().equalsIgnoreCase(CVV_MATCH_CODE))
		  {
		    throw new BraintreeCreditCardValidationException("Credit Card CVV Check Validation Fail", 
		        paymentMethodResult.getCvvValidationCode());
		  }
			throw new AdapterException(paymentMethodResult.getErrorMessage());
		}
	}

	private AddressModel resolveBillingAddress(final AddressData addressBillingData, final AbstractOrderModel cart,
			final CustomerModel customer, final BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData)
	{
		AddressModel billingAddress = getModelService().create(AddressModel.class);

		if (addressBillingData != null)
		{
			billingAddress = buildBillingAddress(addressBillingData, customer);
		}
		else
		{
			// double convert instead of cloning
			final AddressData deliveryAddress = getAddressConverter().convert(cart.getDeliveryAddress());
			getAddressReverseConverter().convert(deliveryAddress, billingAddress);

			billingAddress.setBrainTreeAddressId(cart.getDeliveryAddress().getBrainTreeAddressId());
			if ( billingAddress.getEmail() == null && brainTreeSubscriptionInfoData.getEmail() != null){
				billingAddress.setEmail(brainTreeSubscriptionInfoData.getEmail());
			}
		}
		return billingAddress;
	}

	private AddressModel buildBillingAddress(final AddressData addressData, final CustomerModel customer)
	{
		final AddressModel billingAddress = getModelService().create(AddressModel.class);
		getAddressReverseConverter().convert(addressData, billingAddress);

		billingAddress.setBrainTreeAddressId(addressData.getBrainTreeAddressId());

		if (isNotEmpty(addressData.getEmail()) && !BraintreeConstants.GUEST_USER_TYPE.equals(customer.getType().getCode()))
		{
			billingAddress.setEmail(addressData.getEmail());
		}
		else
		{
			billingAddress.setEmail(customerEmailResolutionService.getEmailForCustomer(customer));
		}

		return billingAddress;
	}

	private void addAdditionalPaymentMethodFields(final BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData,
			final BrainTreeCreatePaymentMethodResult createPaymentMethodResult)
	{
		if (createPaymentMethodResult != null)
		{

			brainTreeSubscriptionInfoData.setPaymentMethodToken(createPaymentMethodResult.getPaymentMethodToken());
			brainTreeSubscriptionInfoData.setExpirationMonth(createPaymentMethodResult.getExpirationMonth());
			brainTreeSubscriptionInfoData.setExpirationYear(createPaymentMethodResult.getExpirationYear());
			brainTreeSubscriptionInfoData.setImageSource(createPaymentMethodResult.getImageSource());
			brainTreeSubscriptionInfoData.setCardNumber(createPaymentMethodResult.getCardNumber());
			brainTreeSubscriptionInfoData.setCardType(createPaymentMethodResult.getCardType());
			brainTreeSubscriptionInfoData.setCardholder(createPaymentMethodResult.getCardholderName());
			if (StringUtils.isNotBlank(createPaymentMethodResult.getEmail()))
			{
				brainTreeSubscriptionInfoData.setEmail(createPaymentMethodResult.getEmail());
			}
			if(StringUtils.isNotBlank(createPaymentMethodResult.getBraintreeAddressId()))
			{
			  brainTreeSubscriptionInfoData.setBraintreeAddressId(createPaymentMethodResult.getBraintreeAddressId());
			}
			
			brainTreeSubscriptionInfoData.setIsDefault(createPaymentMethodResult.getIsDefault());
		}
	}

	public BrainTreePaymentInfoData getBrainTreePaymentInfoData()
	{
		final PaymentInfoModel paymentInfo = getCartService().getSessionCart().getPaymentInfo();
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{

			validateParameterNotNullStandardMessage("paymentInfo", paymentInfo);

			final BrainTreePaymentInfoData paymentData = getBrainTreePaymentInfoDataConverter()
					.convert((BrainTreePaymentInfoModel) paymentInfo);
			return paymentData;
		}
		return null;
	}

	public BrainTreePaymentInfoData getBrainTreePaymentInfoData(final String orderCode)
	{
		final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
		final OrderModel orderModel = getBrainTreeCustomerAccountService().getOrderForCode(orderCode, baseStoreModel);
		final PaymentInfoModel paymentInfo = orderModel.getPaymentInfo();
		if (paymentInfo instanceof BrainTreePaymentInfoModel)
		{

			validateParameterNotNullStandardMessage("paymentInfo", paymentInfo);

			final BrainTreePaymentInfoData paymentData = getBrainTreePaymentInfoDataConverter()
					.convert((BrainTreePaymentInfoModel) paymentInfo);
			return paymentData;
		}
		return null;
	}

	public BrainTreePaymentInfoData getBrainTreePaymentInfoDataByCart(final String cartCode)
	{
		final CartModel cartModel = getCommerceCartService().getCartForCodeAndUser(cartCode, getUserService().getCurrentUser());
		final BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cartModel.getPaymentInfo();
		validateParameterNotNullStandardMessage("paymentInfo", paymentInfo);
		final BrainTreePaymentInfoData paymentData = getBrainTreePaymentInfoDataConverter().convert(paymentInfo);
		return paymentData;
	}

	public void forceUnduplicateCartForReplenishment(final String cartCode) {
		final CartModel cartModel = getCommerceCartService().getCartForCodeAndUser(cartCode, getUserService().getCurrentUser());
		final BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cartModel.getPaymentInfo();
		paymentInfo.setDuplicate(false);
		getModelService().save(paymentInfo);
	}

	public List<BraintreeLocalPaymentMethodsModel> getLocalPaymentMethods(){
		return braintreeLocalPaymentMethodsService.getAllLocalPaymentMethods();
	}

	public OrderData getOrderByPaymentId(String paymentId)
	{
		final OrderData orderData = new OrderData();
		OrderModel orderModel = braintreeLocalPaymentMethodsService.getOrderByPaymentId(paymentId);
		return (orderModel == null) ?
				null :
				getOrderConverter().convert(braintreeLocalPaymentMethodsService.getOrderByPaymentId(paymentId), orderData);
	}

	public  CartModel getCartByPaymentId(String paymentId){
		return braintreeLocalPaymentMethodsService.getCartByPaymentId(paymentId);
	}

	public WebhookNotification getWebhookNotification(Map<String, String[]> parameterMap){
		BrainTreeWebhookNotificationRequest webhookNotificationRequest = new BrainTreeWebhookNotificationRequest();
		webhookNotificationRequest.setBtPayload(parameterMap.get("bt_payload")[0]);
		webhookNotificationRequest.setBtSignature(parameterMap.get("bt_signature")[0]);

		return brainTreePaymentService.getWebhookNotification(webhookNotificationRequest);
	}

	public List<CCPaymentInfoData> getAvailablePayments(List<CCPaymentInfoData> brainTreeCCPaymentInfos){
		return brainTreeCCPaymentInfos.stream().filter(payment -> payment.getPaymentMethodNonce() != null).collect(Collectors.toList());
	}


	/**
	 * This method created to get extend order from order code
	 * @param orderCode
	 * @return
	 */
	public OrderModel gerExtendOrderFromOrderCode(final String orderCode){
		final BaseStoreModel baseStoreModel = getBaseStoreService().getCurrentBaseStore();
		final OrderModel orderModel = getCustomerAccountService()
				.getOrderForCode((CustomerModel) getUserService().getCurrentUser(), orderCode,
						baseStoreModel);
		if (BooleanUtils.isFalse(orderModel.getIsExtendedOrder()) && null != orderModel
				.getExtendedOrderCopy()) {
			return orderModel.getExtendedOrderCopy();
		}
		return orderModel;
	}

	/**
	 * @return the brainTreePaymentService
	 */
	public BrainTreePaymentService getBrainTreePaymentService()
	{
		return brainTreePaymentService;
	}

	/**
	 * @param brainTreePaymentService
	 *           the brainTreePaymentService to set
	 */
	public void setBrainTreePaymentService(final BrainTreePaymentService brainTreePaymentService)
	{
		this.brainTreePaymentService = brainTreePaymentService;
	}

	/**
	 * @return the cartService
	 */
	public CartService getCartService()
	{
		return cartService;
	}

	/**
	 * @param cartService
	 *           the cartService to set
	 */
	public void setCartService(final CartService cartService)
	{
		this.cartService = cartService;
	}

	/**
	 * @return the baseStoreService
	 */
	public BaseStoreService getBaseStoreService()
	{
		return baseStoreService;
	}

	/**
	 * @param baseStoreService
	 *           the baseStoreService to set
	 */
	public void setBaseStoreService(final BaseStoreService baseStoreService)
	{
		this.baseStoreService = baseStoreService;
	}

	/**
	 * @return the customerAccountService
	 */
	public BrainTreeCustomerAccountService getBrainTreeCustomerAccountService()
	{
		return brainTreeCustomerAccountService;
	}

	/**
	 * @param brainTreeCustomerAccountService
	 *           the customerAccountService to set
	 */
	public void setBrainTreeCustomerAccountService(final BrainTreeCustomerAccountService brainTreeCustomerAccountService)
	{
		this.brainTreeCustomerAccountService = brainTreeCustomerAccountService;
	}

	/**
	 * @return the commerceCartService
	 */
	public CommerceCartService getCommerceCartService()
	{
		return commerceCartService;
	}

	/**
	 * @param commerceCartService
	 *           the commerceCartService to set
	 */
	public void setCommerceCartService(final CommerceCartService commerceCartService)
	{
		this.commerceCartService = commerceCartService;
	}

	/**
	 * @return the modelService
	 */
	public ModelService getModelService()
	{
		return modelService;
	}

	/**
	 * @param modelService
	 *           the modelService to set
	 */
	public void setModelService(final ModelService modelService)
	{
		this.modelService = modelService;
	}

	/**
	 * @return the addressConverter
	 */
	public Converter<AddressModel, AddressData> getAddressConverter()
	{
		return addressConverter;
	}

	/**
	 * @param addressConverter
	 *           the addressConverter to set
	 */
	public void setAddressConverter(final Converter<AddressModel, AddressData> addressConverter)
	{
		this.addressConverter = addressConverter;
	}

	/**
	 * @return the billingAddressConverter
	 */
	public BillingAddressConverter getBillingAddressConverter()
	{
		return billingAddressConverter;
	}

	/**
	 * @param billingAddressConverter
	 *           the billingAddressConverter to set
	 */
	public void setBillingAddressConverter(final BillingAddressConverter billingAddressConverter)
	{
		this.billingAddressConverter = billingAddressConverter;
	}

	/**
	 * @return the userService
	 */
	@Override
	public UserService getUserService()
	{
		return userService;
	}

	/**
	 * @param userService
	 *           the userService to set
	 */
	@Override
	public void setUserService(final UserService userService)
	{
		this.userService = userService;
	}


	/**
	 * @return the brainTreeTransactionService
	 */
	public BrainTreeTransactionService getBrainTreeTransactionService()
	{
		return brainTreeTransactionService;
	}

	/**
	 * @param brainTreeTransactionService
	 *           the brainTreeTransactionService to set
	 */
	public void setBrainTreeTransactionService(final BrainTreeTransactionService brainTreeTransactionService)
	{
		this.brainTreeTransactionService = brainTreeTransactionService;
	}

	/**
	 * @return the customerEmailResolutionService
	 */
	public CustomerEmailResolutionService getCustomerEmailResolutionService()
	{
		return customerEmailResolutionService;
	}

	/**
	 * @param customerEmailResolutionService
	 *           the customerEmailResolutionService to set
	 */
	public void setCustomerEmailResolutionService(final CustomerEmailResolutionService customerEmailResolutionService)
	{
		this.customerEmailResolutionService = customerEmailResolutionService;
	}

	/**
	 * @return the brainTreeSubscriptionInfoConverter
	 */
	public Converter<BrainTreeSubscriptionInfoData, BraintreeInfo> getBrainTreeSubscriptionInfoConverter()
	{
		return brainTreeSubscriptionInfoConverter;
	}

	/**
	 * @param brainTreeSubscriptionInfoConverter
	 *           the brainTreeSubscriptionInfoConverter to set
	 */
	public void setBrainTreeSubscriptionInfoConverter(
			final Converter<BrainTreeSubscriptionInfoData, BraintreeInfo> brainTreeSubscriptionInfoConverter)
	{
		this.brainTreeSubscriptionInfoConverter = brainTreeSubscriptionInfoConverter;
	}

	/**
	 * @return the addressReverseConverter
	 */
	public Converter<AddressData, AddressModel> getAddressReverseConverter()
	{
		return addressReverseConverter;
	}

	/**
	 * @param addressReverseConverter
	 *           the addressReverseConverter to set
	 */
	public void setAddressReverseConverter(final Converter<AddressData, AddressModel> addressReverseConverter)
	{
		this.addressReverseConverter = addressReverseConverter;
	}

	/**
	 * @return the brainTreePaymentInfoDataConverter
	 */
	public Converter<BrainTreePaymentInfoModel, BrainTreePaymentInfoData> getBrainTreePaymentInfoDataConverter()
	{
		return brainTreePaymentInfoDataConverter;
	}

	/**
	 * @param brainTreePaymentInfoDataConverter
	 *           the brainTreePaymentInfoDataConverter to set
	 */
	public void setBrainTreePaymentInfoDataConverter(
			final Converter<BrainTreePaymentInfoModel, BrainTreePaymentInfoData> brainTreePaymentInfoDataConverter)
	{
		this.brainTreePaymentInfoDataConverter = brainTreePaymentInfoDataConverter;
	}

	public BrainTreeConfigService getBrainTreeConfigService() {
		return brainTreeConfigService;
	}

	public void setBrainTreeConfigService(BrainTreeConfigService brainTreeConfigService) {
		this.brainTreeConfigService = brainTreeConfigService;
	}

	public BrainTreeUserFacade getBrainTreeUserFacade() {
		return brainTreeUserFacade;
	}

	public void setBrainTreeUserFacade(BrainTreeUserFacade brainTreeUserFacade) {
		this.brainTreeUserFacade = brainTreeUserFacade;
	}

	public BraintreeLocalPaymentMethodsService getBraintreeLocalPaymentMethodsService()
	{
		return braintreeLocalPaymentMethodsService;
	}

	public void setBraintreeLocalPaymentMethodsService(
			BraintreeLocalPaymentMethodsService braintreeLocalPaymentMethodsService)
	{
		this.braintreeLocalPaymentMethodsService = braintreeLocalPaymentMethodsService;
	}

	public Converter<OrderModel, OrderData> getOrderConverter()
	{
		return orderConverter;
	}

	public void setOrderConverter(Converter<OrderModel, OrderData> orderConverter)
	{
		this.orderConverter = orderConverter;
	}

}
