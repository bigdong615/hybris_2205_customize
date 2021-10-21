package com.braintree.facade.impl;

import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_ORDER;
import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import com.bl.core.enums.SerialStatusEnum;
import com.bl.core.model.BlItemsBillingChargeModel;
import com.bl.core.model.BlSerialProductModel;
import com.bl.core.order.dao.BlOrderDao;
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
import com.google.common.util.concurrent.AtomicDouble;
import de.hybris.platform.acceleratorfacades.order.impl.DefaultAcceleratorCheckoutFacade;
import de.hybris.platform.basecommerce.enums.ConsignmentStatus;
import de.hybris.platform.catalog.model.CompanyModel;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.c2l.RegionModel;
import de.hybris.platform.core.model.order.AbstractOrderModel;
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
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;


/**
 * Checkout facade for Braintree
 * @author Moumita
 */
public class BrainTreeCheckoutFacade extends DefaultAcceleratorCheckoutFacade
{
	private static final Logger LOG = Logger.getLogger(BrainTreeCheckoutFacade.class);
	private static final String PAYMENT_INFO_ID ="paymentInfoId";

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
	private BlOrderDao orderDao;

	/**
	 * It sets intent to cart
	 */
	public void storeIntentToCart() {
		CartModel cart = cartService.getSessionCart();
		BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
		paymentInfo.setPayPalIntent(brainTreeConfigService.getIntent());

		getModelService().save(paymentInfo);
	}

	/**
	 * It stores ship from postal code to cart
	 * @param shipsFromPostalCode the ship from postal code
	 */
	public void storeShipsFromPostalCodeToCart(final String shipsFromPostalCode) {
        CartModel cart = cartService.getSessionCart();
        BrainTreePaymentInfoModel paymentInfo = (BrainTreePaymentInfoModel) cart.getPaymentInfo();
        paymentInfo.setShipsFromPostalCode(shipsFromPostalCode);

        getModelService().save(paymentInfo);
    }

	/**
	 * It sets the custom fields into cart
 	 * @param customFields the custom fields
	 */
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

	/**
	 * It does the payment authorization
	 * @param securityCode the security code
	 * @param customFields the custom fields
	 * @return boolean
	 */
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

	/**
	 * It does the payment authorization
	 * @param cart the cart model
	 * @return boolean
	 */
	public boolean authorizePayment(CartModel cart){
		PaymentTransactionEntryModel paymentTransactionEntryModel = brainTreeTransactionService.createAuthorizationTransaction(cart);
		return paymentTransactionEntryModel != null
				&& (TransactionStatus.ACCEPTED.name().equals(paymentTransactionEntryModel.getTransactionStatus())
				|| TransactionStatus.REVIEW.name().equals(paymentTransactionEntryModel.getTransactionStatus()));
	}

	/**
	 * It creates authorize transaction
	 * @param order the order
	 * @param amount the amount
	 * @return boolean
	 */
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
    validateParameterNotNullStandardMessage(PAYMENT_INFO_ID, paymentInfoId);

    if (checkIfCurrentUserIsTheCartUser())
    {
      final CustomerModel currentUserForCheckout = getCurrentUserForCheckout();
      if (StringUtils.isNotBlank(paymentInfoId) && Objects.nonNull(currentUserForCheckout))
      {
        final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentService.completeCreateSubscription(currentUserForCheckout, paymentInfoId);
        if (paymentInfo != null)
        {
          final AddressModel newAddressModel = updateAddressInBraintreeIfAvailable(currentUserForCheckout, addressId, paymentInfo, billingAddress);
          if (Objects.nonNull(newAddressModel))
          {
            getModelService().remove(paymentInfo.getBillingAddress());
            newAddressModel.setOwner(paymentInfo);
            getModelService().save(newAddressModel);
            setAddressOnCustomer(newAddressModel, currentUserForCheckout, billingAddress);            
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
  private AddressModel updateAddressInBraintreeIfAvailable(final CustomerModel currentUserForCheckout, final String addressId,
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
          return updateAddressInBraintree(currentUserForCheckout, paymentInfo, addressData);
        }
        else if (Objects.nonNull(billingAddress))
        {
          return updateAddressInBraintree(currentUserForCheckout, paymentInfo, billingAddress);
        }
      }
    }
    catch (final Exception exception)
    {
      BlLogger.logMessage(LOG, Level.ERROR, "Error while updating address in braintree", exception);
      throw exception;
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
  private AddressModel updateAddressInBraintree(final CustomerModel currentUserForCheckout, final BrainTreePaymentInfoModel paymentInfo,
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
      BlLogger.logMessage(LOG, Level.ERROR, "Error while updating address in braintree", exception);
      throw exception;
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
  
  /**
   * Sets the given address on customer.
   *
   * @param paymentBillingAddressModel the payment billing address model
   * @param customerModel the customer model
   * @param billingAddressData the billing address data
   */
  private void setAddressOnCustomer(final AddressModel paymentBillingAddressModel, final CustomerModel customerModel, 
      final AddressData billingAddressData)
  {
    if(Objects.nonNull(billingAddressData))
    {
      try
      {
        final AddressModel addressOnUser = getModelService().clone(paymentBillingAddressModel, AddressModel.class);
        addressOnUser.setBrainTreeAddressId(StringUtils.EMPTY);
        addressOnUser.setVisibleInAddressBook(billingAddressData.isVisibleInAddressBook());
        addressOnUser.setOwner(customerModel);
        getCustomerAccountService().saveAddressEntry(customerModel, addressOnUser);
      }
      catch(final Exception exception)
      {
        BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception, 
            "Error while setting address on customer with uid - {}", customerModel.getUid());
        throw exception;
      }
    }
  }

	/**
	 * It sets the payment details
	 * @param paymentInfoId the payment info id
	 * @param paymentMethodNonce the payment method nonce
	 * @return boolean
	 */
	public boolean setPaymentDetails(final String paymentInfoId, final String paymentMethodNonce) {
		validateParameterNotNullStandardMessage(PAYMENT_INFO_ID, paymentInfoId);

		if (checkIfCurrentUserIsTheCartUser()) {
			final CustomerModel currentUserForCheckout = getCurrentUserForCheckout();
			if (StringUtils.isNotBlank(paymentInfoId)) {
				final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentService
						.completeCreateSubscription(currentUserForCheckout, paymentInfoId);
				if (paymentInfo != null) {
					paymentInfo.setNonce(paymentMethodNonce);
					getModelService().save(paymentInfo);
					return true;
				} else {
					super.setPaymentDetails(paymentInfoId);
				}

			}
		}
		return false;
	}

	/**
	 * It sets the payment details
	 * @param paymentInfoId the payment info id
	 * @param paymentMethodNonce the payment method nonce
	 * @param order the order
	 * @return boolean
	 */
	public boolean setPaymentDetailsForModifyPayment(final String paymentInfoId, final String paymentMethodNonce, final AbstractOrderModel order) {
		validateParameterNotNullStandardMessage(PAYMENT_INFO_ID, paymentInfoId);

		if (checkIfCurrentUserIsTheCartUser()) {
			final CustomerModel currentUserForCheckout = getCurrentUserForCheckout();
			if (StringUtils.isNotBlank(paymentInfoId)) {
				final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentService
						.completeCreateSubscriptionForModifyPayment(currentUserForCheckout, paymentInfoId, order);
				if (paymentInfo != null) {
					paymentInfo.setNonce(paymentMethodNonce);
					getModelService().save(paymentInfo);
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
		validateParameterNotNullStandardMessage(PAYMENT_INFO_ID, paymentInfoId);

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

	/**
	 * It generates client token
	 * @return client token
	 */
	public String generateClientToken()
	{
		return brainTreePaymentService.generateClientToken();
	}

	public PayPalCheckoutData getPayPalCheckoutData()
	{
		final PayPalCheckoutData payPalCheckoutData = payPalCardDataConverter.convert(cartService.getSessionCart());
		//Set default address for paypal 
       if(cartService.getSessionCart().getGiftCardCost() != null)
		{
    	  CompanyModel customergroup = getUserService().getUserGroupForUID(BraintreeConstants.PAYPAL_DEFAULT_ADDRESS,
					CompanyModel.class);
    	  if(customergroup != null)
    	  {
			final AddressModel contactAddress = customergroup.getContactAddress();

			final PayPalAddressData payPalAddress = payPalAddressDataConverter
					.convert(contactAddress);
			payPalCheckoutData.setShippingAddressOverride(payPalAddress);
			payPalCheckoutData.setEnvironment(brainTreeConfigService.getEnvironmentTypeName());
			payPalCheckoutData.setSecure3d(brainTreeConfigService.get3dSecureConfiguration());
			payPalCheckoutData.setSkip3dSecureLiabilityResult(brainTreeConfigService.getIsSkip3dSecureLiabilityResult());
    	  }
		}
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

	/**
	 * It gets the order by order code
	 * @param orderCode the order number
	 * @return AbstractOrderModel
	 */
	public AbstractOrderModel getOrderByCode(final String orderCode) {
		return getOrderDao().getOrderByCode(orderCode);
	}

	public boolean isAvailablePaymentAuthorization() {
		if (isCreditCard() || isApplePay() || isVenmo() || isGooglePay()) {
			return true;
		}
		boolean paypalIntentIsOrder = PAYPAL_INTENT_ORDER.equals(getBrainTreeConfigService().getIntent());
		boolean config = Boolean.parseBoolean(getBrainTreeConfigService().getStoreInVaultForCurrentUser());
		return !(paypalIntentIsOrder && !config);
	}

	/**
	 * It gets payment info model by payment info id
	 * @param customer the customer
	 * @param paymentInfoId the payment info id
	 * @param nonce the payment method nonce
	 * @return BrainTreePaymentInfoModel
	 */
	public BrainTreePaymentInfoModel getBrainTreePaymentInfoForCode(final CustomerModel customer, final String
			paymentInfoId, final String nonce) {
		return brainTreePaymentService.getBrainTreePaymentInfoForCode(customer, paymentInfoId, nonce);
	}
	
	/**
	 * It gets payment info model by payment info id for Deposit.
	 *
	 * @param customer the customer
	 * @param paymentInfoId the payment info id
	 * @param nonce the payment method nonce
	 * @param depositAmount the deposit amount
	 * @return BrainTreePaymentInfoModel
	 */
  public BrainTreePaymentInfoModel getBrainTreePaymentInfoForCodeToDeposit(final CustomerModel customer, final String
      paymentInfoId, final String nonce, final double depositAmount) {
    return brainTreePaymentService.getBrainTreePaymentInfoForCodeToDeposit(customer, paymentInfoId, nonce, depositAmount);
  }

	/**
	 * It sets the order and consignment status and payBill flag as true on successful payment
	 * @param order the order
	 */
	public void setPayBillFlagTrue(final AbstractOrderModel order) {
		AtomicBoolean isOrderComplete = new AtomicBoolean(true);
		order.getConsignments()
				.forEach(consignment -> consignment.getConsignmentEntries().forEach(consignmentEntry -> consignmentEntry
						.getBillingCharges().forEach((serialCode, listOfCharges) -> listOfCharges.forEach(billing -> {
							if(BooleanUtils.isFalse(billing.isBillPaid())) {
								billing.setBillPaid(true);
								getModelService().save(billing);
								setTotalAmountPastDue(consignment.getOrder().getUser(), billing);
							}
						}))));
		order.getConsignments().forEach(consignment -> consignment.getConsignmentEntries()
				.forEach(consignmentEntry -> consignmentEntry.getSerialProducts().forEach(serial -> {
						if(serial instanceof BlSerialProductModel && (((BlSerialProductModel) serial).getSerialStatus().equals(SerialStatusEnum.BOXED) ||
								((BlSerialProductModel) serial).getSerialStatus().equals(SerialStatusEnum.UNBOXED) ||
								((BlSerialProductModel) serial).getSerialStatus().equals(SerialStatusEnum.SHIPPED))) {
							isOrderComplete.set(false);
					}
					})));
		if(isOrderComplete.get()) {
			order.setStatus(OrderStatus.COMPLETED);
			order.setOrderCompletedDate(new Date());
			getModelService().save(order);
			order.getConsignments().forEach(consignmentModel -> {
				consignmentModel.setStatus(ConsignmentStatus.COMPLETED);
				getModelService().save(consignmentModel);
			});
		}
	}

	/**
	 * It updates the total amount past due on successful bill payment
	 * @param user the user
	 * @param billing the billing charge instance
	 */
	private void setTotalAmountPastDue(final UserModel user, final BlItemsBillingChargeModel billing) {
		final CustomerModel customerModel = (CustomerModel) user;
		if(Objects.nonNull(customerModel.getTotalAmountPastDue())) {
			customerModel.setTotalAmountPastDue(customerModel.getTotalAmountPastDue().subtract(billing.getChargedAmount()));
			getModelService().save(customerModel);
			BlLogger.logFormatMessageInfo(LOG, Level.DEBUG, "Total amount past due : {} updated for the customer {} ",
					customerModel.getTotalAmountPastDue(), customerModel.getUid());
		}
	}

	/**
	 * This method return true if any unpaid bill present on customer
	 * 
	 */
	public boolean isCustomerHasUnPaidBillOrders()
	{   
		final AtomicDouble totalAmt = new AtomicDouble(0.0);
		List<AbstractOrderModel> unPaidBillOrders = getOrderDao().getUnPaidBillOrderByCustomer();
	      unPaidBillOrders.forEach(orders -> orders.getConsignments()
	      .forEach(consignment -> consignment.getConsignmentEntries().forEach(consignmentEntry -> consignmentEntry
	      .getBillingCharges().forEach((serialCode, listOfCharges) -> listOfCharges.forEach(billing -> {
	        if(BooleanUtils.isFalse(billing.isBillPaid())) {
	          totalAmt.addAndGet(billing.getChargedAmount().doubleValue());
	        }
	      })))));
	    
		 return Double.compare(totalAmt.get(), 0.0) > 0 ;
	}
	private boolean isCreditCard()
	{
		PaymentInfoModel paymentInfoModel = getCart().getPaymentInfo();
		if (paymentInfoModel instanceof BrainTreePaymentInfoModel)
		{
			return BrainTreePaymentMethod.CREDITCARD.getCode().equalsIgnoreCase(((BrainTreePaymentInfoModel) paymentInfoModel).getPaymentProvider());
		}
		return false;
	}

	private boolean isApplePay()
	{
		PaymentInfoModel paymentInfoModel = getCart().getPaymentInfo();
		if (paymentInfoModel instanceof BrainTreePaymentInfoModel)
		{
			return BrainTreePaymentMethod.APPLEPAYCARD.getCode().equalsIgnoreCase(((BrainTreePaymentInfoModel) paymentInfoModel).getPaymentProvider());
		}
		return false;
	}

	private boolean isGooglePay()
	{
		PaymentInfoModel paymentInfoModel = getCart().getPaymentInfo();
		if (paymentInfoModel instanceof BrainTreePaymentInfoModel)
		{
			return BrainTreePaymentMethod.ANDROIDPAYCARD.getCode().equalsIgnoreCase(((BrainTreePaymentInfoModel) paymentInfoModel).getPaymentProvider());
		}
		return false;
	}

	private boolean isVenmo()
	{
		PaymentInfoModel paymentInfoModel = getCart().getPaymentInfo();
		if (paymentInfoModel instanceof BrainTreePaymentInfoModel)
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
		return brainTreeConfigService.getAcceptedPaymentMethodImages();
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

	/**
	 * It handles order intent via subscription
	 * @param order the order
	 */
    public void handleOrderIntentViaSubscription(final OrderModel order) {
       	getModelService().refresh(order);
        UserModel user = order.getUser();

        final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo((BrainTreePaymentInfoModel) order.getPaymentInfo(), true);
        subscriptionInfo.setIntent(BraintreeConstants.PAYPAL_INTENT_ORDER);
        subscriptionInfo.setAmount(String.valueOf(order.getTotalPrice()));
		 final boolean isCreditEnabled = brainTreeConfigService.getCreditEnabled();
        brainTreePaymentFacade.completeCreateSubscription(subscriptionInfo, (CustomerModel) user, order, isCreditEnabled, true);

        BrainTreePaymentInfoModel brainTreePaymentInfo = (BrainTreePaymentInfoModel) order.getPaymentInfo();
        brainTreePaymentInfo.setPayPalIntent(BraintreeConstants.PAYPAL_INTENT_ORDER);
        brainTreePaymentInfo.setSaved(false);
        brainTreePaymentInfo.setCustomFields(getCustomFields());

        String orderSite = order.getSite().getUid();
        String orderCurrency = order.getCurrency().getIsocode().toLowerCase();
        brainTreePaymentInfo.setMerchantAccountIdForCurrentSite(getBrainTreeConfigService().getMerchantAccountIdByCurrentSiteNameAndCurrency(orderSite, orderCurrency));

        getModelService().save(brainTreePaymentInfo);
    }

	/**
	 * It places the order
	 * @param cartModel the cart model
	 * @return Order data
	 * @throws InvalidCartException Invalid cart exception
	 */
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
					setAuthorizedFlagInOrder(voidResult.getTransactionStatus(), cart);
				}
			}
		} catch (final Exception ex) {
			BlLogger.logFormattedMessage(LOG, Level.ERROR, "Error occurred while voiding the auth transaction "
					+ "for order {} ", cart.getCode(), ex);
		}
	}

	/**
	 * @param transactionStatus
	 * @param cart
	 * This is used to set the isAuthorized flag of order
	 */
	private void setAuthorizedFlagInOrder(TransactionStatus transactionStatus,
			CartModel cart) {
		if (TransactionStatus.ACCEPTED.equals(transactionStatus)) {
			cart.setIsAuthorizationVoided(Boolean.TRUE);
			getModelService().save(cart);
		}
	}
	
	/**
	 * Gets the cloned payment info for code.
	 *
	 * @param customer the customer
	 * @param paymentInfoId the payment info id
	 * @param nonce the nonce
	 * @param newAmount the new amount
	 * @return the cloned payment info for code
	 */
	public BrainTreePaymentInfoModel getModifyOrderPaymentInfoForCode(final CustomerModel customer, final String paymentInfoId,
      final String nonce, final Double newAmount)
	{
	  return getBrainTreePaymentService().getModifyOrderPaymentInfoForCode(customer, paymentInfoId, nonce, newAmount);
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

	/**
	 * @return the blOrderDao
	 */
	public BlOrderDao getOrderDao()
	{
		return orderDao;
	}

	/**
	 * @param orderDao
	 *           the blOrderDao to set
	 */
	public void setOrderDao(final BlOrderDao orderDao)
	{
		this.orderDao = orderDao;
	}
	
}
