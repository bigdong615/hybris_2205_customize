package com.braintree.facade.impl;

import com.bl.logging.BlLogger;
import com.braintree.command.request.BrainTreeAddressRequest;
import com.braintree.command.result.BrainTreeAddressResult;
import com.braintree.command.result.BrainTreeVoidResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.converters.utils.BlBrainTreeConvertUtils;
import com.braintree.customfield.service.CustomFieldsService;
import com.braintree.enums.BrainTreePaymentMethod;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.hybris.data.PayPalAddressData;
import com.braintree.hybris.data.PayPalCheckoutData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.paypal.converters.impl.PayPalAddressDataConverter;
import com.braintree.paypal.converters.impl.PayPalCardDataConverter;
import com.braintree.transaction.service.BrainTreeTransactionService;
import de.hybris.platform.acceleratorfacades.order.impl.DefaultAcceleratorCheckoutFacade;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.core.model.user.UserModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.payment.commands.request.VoidRequest;
import de.hybris.platform.payment.dto.TransactionStatus;
import de.hybris.platform.payment.enums.PaymentTransactionType;
import de.hybris.platform.payment.model.PaymentTransactionEntryModel;
import de.hybris.platform.payment.model.PaymentTransactionModel;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.user.UserService;
import java.util.List;
import java.util.Optional;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_ORDER;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;


public class BrainTreeCheckoutFacade extends DefaultAcceleratorCheckoutFacade
{
	private static final Logger LOG = Logger.getLogger(BrainTreeCheckoutFacade.class);

	private Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> brainTreePaymentInfoConverter;
	private BrainTreePaymentService brainTreePaymentService;
	private BrainTreeTransactionService brainTreeTransactionService;
	private CartService cartService;
	private UserService userService;
	private PayPalAddressDataConverter payPalAddressDataConverter;
	private PayPalCardDataConverter payPalCardDataConverter;
	private BrainTreeConfigService brainTreeConfigService;
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;
	private CustomFieldsService customFieldsService;


	public void storeIntentToCart() {
		CartModel cart = cartService.getSessionCart();
		BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
		paymentInfo.setPayPalIntent(brainTreeConfigService.getIntent());

		getModelService().save(paymentInfo);
	}

	public void storeShipsFromPostalCodeToCart(final String shipsFromPostalCode) {
        CartModel cart = cartService.getSessionCart();
        BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
        paymentInfo.setShipsFromPostalCode(shipsFromPostalCode);

        getModelService().save(paymentInfo);
    }

	public void storeCustomFieldsToCart(final Map<String, String> customFields) {
		CartModel cart = cartService.getSessionCart();
		BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
		paymentInfo.setCustomFields(customFields);

		getModelService().save(paymentInfo);
	}

	@Override
	public boolean authorizePayment(final String securityCode)
	{
		return authorizePayment(securityCode, getCustomFields());
	}

	public boolean authorizePayment(final String securityCode, Map<String, String> customFields)
	{
		LOG.info("!isAvailablePaymentAuthorization: " + !isAvailablePaymentAuthorization());
		if (!isAvailablePaymentAuthorization())
		{
			LOG.error("calling authorizePaymentIfIntentOrder");
			return authorizePaymentIfIntentOrder(customFields);
		}
		if (!brainTreeConfigService.isOneOfPaymentMethodsEnabled())
		{
			LOG.info("Use default accelerator checkout flow.");
			return super.authorizePayment(securityCode);
		}

		return brainTreeTransactionService.createAuthorizationTransaction(customFields);
	}

	public boolean authorizePayment(CartModel cart){
		PaymentTransactionEntryModel paymentTransactionEntryModel = brainTreeTransactionService.createAuthorizationTransaction(cart);
		return paymentTransactionEntryModel != null
				&& (TransactionStatus.ACCEPTED.name().equals(paymentTransactionEntryModel.getTransactionStatus())
				|| TransactionStatus.REVIEW.name().equals(paymentTransactionEntryModel.getTransactionStatus()));
	}

	public boolean authorizePayment(OrderModel order, BigDecimal amount)
	{
		PaymentTransactionEntryModel paymentTransactionEntryModel =
				brainTreeTransactionService.createAuthorizationTransaction(order, amount);
		return paymentTransactionEntryModel != null
				&& (TransactionStatus.ACCEPTED.name().equals(paymentTransactionEntryModel.getTransactionStatus())
				|| TransactionStatus.REVIEW.name().equals(paymentTransactionEntryModel.getTransactionStatus()));
	}

	@Override
	public CCPaymentInfoData getPaymentDetails()
	{
		final CartModel cart = getCart();
		if (cart != null)
		{
			final PaymentInfoModel paymentInfo = cart.getPaymentInfo();
			if (paymentInfo instanceof BrainTreePaymentInfoModel)
			{
				return brainTreePaymentInfoConverter.convert((BrainTreePaymentInfoModel) paymentInfo);
			}
			else
			{
				return super.getPaymentDetails();
			}
		}

		return null;
	}
	
  /**
   * Sets the payment details with updating address on Braintree and PaymentInfoModel.
   *
   * @param paymentInfoId the payment info id
   * @param paymentMethodNonce the payment method nonce
   * @param addressId the address id
   * @param billingAddress the billing address
   * @return true, if successful
   */
  public boolean setPaymentDetails(final String paymentInfoId, final String paymentMethodNonce, final String addressId,
      final AddressData billingAddress)
  {
    validateParameterNotNullStandardMessage("paymentInfoId", paymentInfoId);

    if (checkIfCurrentUserIsTheCartUser())
    {
      final CustomerModel currentUserForCheckout = getCurrentUserForCheckout();
      if (StringUtils.isNotBlank(paymentInfoId) && Objects.nonNull(currentUserForCheckout))
      {
        final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentService.completeCreateSubscription(currentUserForCheckout, paymentInfoId);
        if (paymentInfo != null)
        {
          final AddressModel newAddressModel = updateAddressInBtIfAvailable(currentUserForCheckout, addressId, paymentInfo, billingAddress);
          if (Objects.nonNull(newAddressModel))
          {
            getModelService().remove(paymentInfo.getBillingAddress());
            newAddressModel.setOwner(paymentInfo);
            getModelService().save(newAddressModel);
            paymentInfo.setBillingAddress(newAddressModel);
          }
          paymentInfo.setNonce(paymentMethodNonce);
          getModelService().save(paymentInfo);
          return true;
        }
        else
        {
          super.setPaymentDetails(paymentInfoId);
        }
      }
    }
    return false;
  }
	
  /**
   * Update address in braintree and PaymentInfoModel if available.
   *
   * @param currentUserForCheckout the current user for checkout
   * @param addressId the address id
   * @param paymentInfo the payment info
   * @param billingAddress the billing address
   * @return the address model
   */
  private AddressModel updateAddressInBtIfAvailable(final CustomerModel currentUserForCheckout, final String addressId,
      final BrainTreePaymentInfoModel paymentInfo, final AddressData billingAddress)
  {
    try
    {
      if (!getUserService().isAnonymousUser(currentUserForCheckout))
      {
        if (StringUtils.isNotBlank(addressId) && !StringUtils.equalsIgnoreCase(addressId, paymentInfo.getBillingAddress().getPk().toString()))
        {
          final AddressData addressData =
              getAddressConverter().convert(getCustomerAccountService().getAddressForCode(currentUserForCheckout, addressId));
          return updateAddressOnBT(currentUserForCheckout, paymentInfo, addressData);
        }
        else if (Objects.nonNull(billingAddress))
        {
          return updateAddressOnBT(currentUserForCheckout, paymentInfo, billingAddress);
        }
      }
    }
    catch (final Exception exception)
    {
      LOG.error("Error occured while updating address", exception);
      return null;
    }
    return null;
  }

  /**
   * Update address on BrainTree.
   *
   * @param currentUserForCheckout the current user for checkout
   * @param paymentInfo the payment info
   * @param addressData the address data
   * @return the address model
   */
  private AddressModel updateAddressOnBT(final CustomerModel currentUserForCheckout, final BrainTreePaymentInfoModel paymentInfo,
      final AddressData addressData)
  {
    try
    {
      setRegionShortCodeIfNotAvailable(addressData);
      final BrainTreeAddressRequest brainTreeAddressRequest =
          BlBrainTreeConvertUtils.convertBrainTreeAddress(currentUserForCheckout.getBraintreeCustomerId(), addressData);
      final String brainTreeAddressId = paymentInfo.getBillingAddress().getBrainTreeAddressId();
      if (StringUtils.isNotBlank(brainTreeAddressId))
      {
        brainTreeAddressRequest.setAddressId(brainTreeAddressId);
        final BrainTreeAddressResult updateAddressResult = getBrainTreePaymentService().updateAddress(brainTreeAddressRequest);
        if (updateAddressResult.isSuccess())
        {
          final AddressModel addressModel = getModelService().create(AddressModel.class);
          getAddressReversePopulator().populate(addressData, addressModel);
          addressModel.setBrainTreeAddressId(brainTreeAddressId);
          return addressModel;
        }
      }
    }
    catch (final Exception exception)
    {
      LOG.error("Error occured while updating address", exception);
    }
    return null;
  }
  
  /**
   * Sets the region short code if not available.
   *
   * @param addressData the new region short code if not available
   */
  private void setRegionShortCodeIfNotAvailable(final AddressData addressData)
  {
    if (checkObjectEmptyAndNull(addressData))
    {
      RegionModel region = getCommonI18NService().getRegion(getCommonI18NService().getCountry(addressData.getCountry().getIsocode()),
          addressData.getRegion().getIsocode());
      addressData.getRegion().setIsocodeShort(region.getIsocodeShort());
    }
  }

  /**
   * Check object is no empty and not null.
   *
   * @param addressData the address data
   * @return true, if successful
   */
  private boolean checkObjectEmptyAndNull(final AddressData addressData)
  {
    return Objects.nonNull(addressData) && Objects.nonNull(addressData.getCountry()) && Objects.nonNull(addressData.getRegion())
        && StringUtils.isBlank(addressData.getRegion().getIsocodeShort()) && StringUtils.isNotBlank(addressData.getCountry().getIsocode());
  }

	public boolean setPaymentDetails(final String paymentInfoId, final String paymentMethodNonce) {
		validateParameterNotNullStandardMessage("paymentInfoId", paymentInfoId);

		if (checkIfCurrentUserIsTheCartUser()) {
			final CustomerModel currentUserForCheckout = getCurrentUserForCheckout();
			if (StringUtils.isNotBlank(paymentInfoId)) {
				final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentService
						.completeCreateSubscription(currentUserForCheckout, paymentInfoId);
				paymentInfo.setNonce(paymentMethodNonce);
				getModelService().save(paymentInfo);
				if (paymentInfo != null) {
					return true;
				} else {
					super.setPaymentDetails(paymentInfoId);
				}

			}
		}
		return false;
	}

	@Override
	public boolean setPaymentDetails(final String paymentInfoId)
	{
		validateParameterNotNullStandardMessage("paymentInfoId", paymentInfoId);

		if (checkIfCurrentUserIsTheCartUser())
		{
			final CustomerModel currentUserForCheckout = getCurrentUserForCheckout();
			if (StringUtils.isNotBlank(paymentInfoId))
			{
				final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentService
						.completeCreateSubscription(currentUserForCheckout, paymentInfoId);
				if (paymentInfo != null)
				{
					return true;
				}
				else
				{
					super.setPaymentDetails(paymentInfoId);
				}

			}
		}

		return false;
	}

	public String generateClientToken()
	{
		final String clientToken = brainTreePaymentService.generateClientToken();
		return clientToken;
	}

	public PayPalCheckoutData getPayPalCheckoutData()
	{
		final PayPalCheckoutData payPalCheckoutData = payPalCardDataConverter.convert(cartService.getSessionCart());

		if (cartService.getSessionCart().getDeliveryAddress() != null)
		{
			final PayPalAddressData payPalAddress = payPalAddressDataConverter
					.convert(cartService.getSessionCart().getDeliveryAddress());
			payPalCheckoutData.setShippingAddressOverride(payPalAddress);
			payPalCheckoutData.setEnvironment(brainTreeConfigService.getEnvironmentTypeName());
			payPalCheckoutData.setSecure3d(brainTreeConfigService.get3dSecureConfiguration());
			payPalCheckoutData.setSkip3dSecureLiabilityResult(brainTreeConfigService.getIsSkip3dSecureLiabilityResult());
		}
		return payPalCheckoutData;
	}

	public boolean isAvailablePaymentAuthorization() {
		if (isCreditCard() || isApplePay() || isVenmo() || isGooglePay()) {
			return true;
		}
		boolean paypalIntentIsOrder = PAYPAL_INTENT_ORDER.equals(getBrainTreeConfigService().getIntent());
		boolean config = Boolean.parseBoolean(getBrainTreeConfigService().getStoreInVaultForCurrentUser());
		return !(paypalIntentIsOrder && !config);
	}

	private boolean isCreditCard()
	{
		PaymentInfoModel paymentInfoModel = getCart().getPaymentInfo();
		if (paymentInfoModel != null && paymentInfoModel instanceof BrainTreePaymentInfoModel)
		{
			return BrainTreePaymentMethod.CREDITCARD.getCode().equalsIgnoreCase(((BrainTreePaymentInfoModel) paymentInfoModel).getPaymentProvider());
		}
		return false;
	}

	private boolean isApplePay()
	{
		PaymentInfoModel paymentInfoModel = getCart().getPaymentInfo();
		if (paymentInfoModel != null && paymentInfoModel instanceof BrainTreePaymentInfoModel)
		{
			return BrainTreePaymentMethod.APPLEPAYCARD.getCode().equalsIgnoreCase(((BrainTreePaymentInfoModel) paymentInfoModel).getPaymentProvider());
		}
		return false;
	}

	private boolean isGooglePay()
	{
		PaymentInfoModel paymentInfoModel = getCart().getPaymentInfo();
		if (paymentInfoModel != null && paymentInfoModel instanceof BrainTreePaymentInfoModel)
		{
			return BrainTreePaymentMethod.ANDROIDPAYCARD.getCode().equalsIgnoreCase(((BrainTreePaymentInfoModel) paymentInfoModel).getPaymentProvider());
		}
		return false;
	}

	private boolean isVenmo()
	{
		PaymentInfoModel paymentInfoModel = getCart().getPaymentInfo();
		if (paymentInfoModel != null && paymentInfoModel instanceof BrainTreePaymentInfoModel)
		{
			return BrainTreePaymentMethod.VENMOACCOUNT.getCode().equalsIgnoreCase(((BrainTreePaymentInfoModel) paymentInfoModel).getPaymentProvider());
		}
		return false;
	}

	public void setBrainTreePaymentInfoConverter(
			final Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> brainTreePaymentInfoConverter)
	{
		this.brainTreePaymentInfoConverter = brainTreePaymentInfoConverter;
	}

	public Map<String, String> getAcceptedPaymentMethodImages()
	{
		final Map<String, String> acceptedPaymentMethodImages = brainTreeConfigService.getAcceptedPaymentMethodImages();
		return acceptedPaymentMethodImages;
	}

	private boolean authorizePaymentIfIntentOrder(Map<String, String> customFields)
	{
		createPaymentMethodIfIntentOrder(customFields);
		return Boolean.TRUE.booleanValue();
	}

	private void createPaymentMethodIfIntentOrder (Map<String, String> customFields)
	{
		CartModel cart = getCartService().getSessionCart();
		final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo((BrainTreePaymentInfoModel)cart.getPaymentInfo(), true);
		subscriptionInfo.setIntent(BraintreeConstants.PAYPAL_INTENT_ORDER);
		subscriptionInfo.setAmount(String.valueOf(cart.getTotalPrice()));
		brainTreePaymentFacade.completeCreateSubscription(subscriptionInfo);

		BrainTreePaymentInfoModel brainTreePaymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
		brainTreePaymentInfo.setPayPalIntent(BraintreeConstants.PAYPAL_INTENT_ORDER);
		brainTreePaymentInfo.setSaved(false);
		brainTreePaymentInfo.setCustomFields(customFields);
		getModelService().save(brainTreePaymentInfo);
	}

	private BrainTreeSubscriptionInfoData buildSubscriptionInfo(final BrainTreePaymentInfoModel paymentInfoModel, final boolean isPaymentInfoSaved)
	{
		final BrainTreeSubscriptionInfoData subscriptionInfo = new BrainTreeSubscriptionInfoData();
		subscriptionInfo.setPaymentProvider(paymentInfoModel.getPaymentProvider());
		subscriptionInfo.setCardNumber(paymentInfoModel.getCardNumber());
		subscriptionInfo.setDeviceData(paymentInfoModel.getDeviceData());
		subscriptionInfo.setNonce(paymentInfoModel.getNonce());
		subscriptionInfo.setSavePaymentInfo(isPaymentInfoSaved);
		subscriptionInfo.setCardholder(paymentInfoModel.getCardholderName());
		if (paymentInfoModel.getLiabilityShifted() != null)
		{
			subscriptionInfo.setLiabilityShifted(paymentInfoModel.getLiabilityShifted());
		}
		if (paymentInfoModel.getCardType() != null)
		{
			subscriptionInfo.setCardType(paymentInfoModel.getCardType().getCode());
		}
		if (paymentInfoModel.getBillingAddress() != null)
		{
			subscriptionInfo.setEmail(paymentInfoModel.getBillingAddress().getEmail());
		}
		return subscriptionInfo;
	}

    public void handleOrderIntentViaSubscription(final OrderModel order) {
       	getModelService().refresh(order);
        UserModel user = order.getUser();

        CustomerModel customer = (CustomerModel) order.getPlacedBy();

        final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo((BrainTreePaymentInfoModel) order.getPaymentInfo(), true);
        subscriptionInfo.setIntent(BraintreeConstants.PAYPAL_INTENT_ORDER);
        subscriptionInfo.setAmount(String.valueOf(order.getTotalPrice()));
		 final boolean isCreditEnabled = brainTreeConfigService.getCreditEnabled();
        brainTreePaymentFacade.completeCreateSubscription(subscriptionInfo, (CustomerModel) user, order, isCreditEnabled);

        BrainTreePaymentInfoModel brainTreePaymentInfo = (BrainTreePaymentInfoModel) order.getPaymentInfo();
        brainTreePaymentInfo.setPayPalIntent(BraintreeConstants.PAYPAL_INTENT_ORDER);
        brainTreePaymentInfo.setSaved(false);
        brainTreePaymentInfo.setCustomFields(getCustomFields());

        String orderSite = order.getSite().getUid();
        String orderCurrency = order.getCurrency().getIsocode().toLowerCase();
        brainTreePaymentInfo.setMerchantAccountIdForCurrentSite(getBrainTreeConfigService().getMerchantAccountIdByCurrentSiteNameAndCurrency(orderSite, orderCurrency));

        getModelService().save(brainTreePaymentInfo);
    }

    public OrderData placeOrderByCart(CartModel cartModel) throws InvalidCartException
	 {
		 if (cartModel != null)
		 {
				 beforePlaceOrder(cartModel);
				 final OrderModel orderModel = placeOrder(cartModel);
				 afterPlaceOrder(cartModel, orderModel);
				 if (orderModel != null)
				 {
					 return getOrderConverter().convert(orderModel);
				 }
		 }
		 return null;
	 }

	@Override
	protected void afterPlaceOrder(CartModel cartModel, OrderModel orderModel)
	{
		if (orderModel != null)
		{
			getModelService().remove(cartModel);
			getModelService().refresh(orderModel);
		}
	}

	/**
	 * It voids the auth transaction of the order
	 */
	public void voidAuthTransaction() {
		final CartModel cart = cartService.getSessionCart();
  	try {
			final String merchantTransactionCode = cart.getUser().getUid();
			List<PaymentTransactionModel> transactions = cart.getPaymentTransactions();
			if (CollectionUtils.isNotEmpty(transactions) && null != merchantTransactionCode) {
				List<PaymentTransactionEntryModel> transactionEntries = transactions.get(0).getEntries();
				final Optional<PaymentTransactionEntryModel> authEntry = transactionEntries.stream()
						.filter(transactionEntry ->
								transactionEntry.getType().equals(PaymentTransactionType.AUTHORIZATION))
						.findFirst();
				if (authEntry.isPresent()) {
					final VoidRequest voidRequest = new VoidRequest(merchantTransactionCode,
							authEntry.get().getRequestId(), StringUtils.EMPTY,
							StringUtils.EMPTY);
					final BrainTreeVoidResult voidResult = brainTreePaymentService
							.voidTransaction(voidRequest);
					if (TransactionStatus.ACCEPTED.equals(voidResult.getTransactionStatus())) {
							cart.setIsAuthorizationVoided(Boolean.TRUE);
							getModelService().save(cart);
					}
				}
			}
		} catch (final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error occurred while voiding the auth transaction "
					+ "for order {} ", cart.getCode(), ex);
		}
	}

	private Map<String, String> getCustomFields()
	{
		return customFieldsService.getDefaultCustomFieldsMap();
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
	 * @return the cartService
	 */
	@Override
	public CartService getCartService()
	{
		return cartService;
	}

	/**
	 * @param cartService
	 *           the cartService to set
	 */
	@Override
	public void setCartService(final CartService cartService)
	{
		this.cartService = cartService;
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
	 * @return the payPalAddressDataConverter
	 */
	public PayPalAddressDataConverter getPayPalAddressDataConverter()
	{
		return payPalAddressDataConverter;
	}

	/**
	 * @param payPalAddressDataConverter
	 *           the payPalAddressDataConverter to set
	 */
	public void setPayPalAddressDataConverter(final PayPalAddressDataConverter payPalAddressDataConverter)
	{
		this.payPalAddressDataConverter = payPalAddressDataConverter;
	}

	/**
	 * @return the payPalCardDataConverter
	 */
	public PayPalCardDataConverter getPayPalCardDataConverter()
	{
		return payPalCardDataConverter;
	}

	/**
	 * @param payPalCardDataConverter
	 *           the payPalCardDataConverter to set
	 */
	public void setPayPalCardDataConverter(final PayPalCardDataConverter payPalCardDataConverter)
	{
		this.payPalCardDataConverter = payPalCardDataConverter;
	}

	/**
	 * @return the brainTreeConfigService
	 */
	public BrainTreeConfigService getBrainTreeConfigService()
	{
		return brainTreeConfigService;
	}

	/**
	 * @param brainTreeConfigService
	 *           the brainTreeConfigService to set
	 */
	public void setBrainTreeConfigService(final BrainTreeConfigService brainTreeConfigService)
	{
		this.brainTreeConfigService = brainTreeConfigService;
	}

	/**
	 * @return the brainTreePaymentInfoConverter
	 */
	public Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> getBrainTreePaymentInfoConverter()
	{
		return brainTreePaymentInfoConverter;
	}

	public BrainTreePaymentFacadeImpl getBrainTreePaymentFacade() {
		return brainTreePaymentFacade;
	}

	public void setBrainTreePaymentFacade(BrainTreePaymentFacadeImpl brainTreePaymentFacade) {
		this.brainTreePaymentFacade = brainTreePaymentFacade;
	}

	public CustomFieldsService getCustomFieldsService()
	{
		return customFieldsService;
	}

	public void setCustomFieldsService(CustomFieldsService customFieldsService)
	{
		this.customFieldsService = customFieldsService;
	}
}
