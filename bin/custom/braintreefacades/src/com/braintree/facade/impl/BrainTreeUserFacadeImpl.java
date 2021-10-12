/**
 *
 */
package com.braintree.facade.impl;

import static de.hybris.platform.servicelayer.util.ServicesUtil.validateParameterNotNullStandardMessage;

import com.bl.core.services.customer.BlCustomerAccountService;
import com.bl.core.utils.BlReplaceMentOrderUtils;
import com.braintree.command.request.BrainTreeAddressRequest;
import com.braintree.command.request.BrainTreeCreateCreditCardPaymentMethodRequest;
import com.braintree.command.request.BrainTreeCustomerRequest;
import com.braintree.command.request.BrainTreeDeletePaymentMethodRequest;
import com.braintree.command.request.BrainTreeUpdatePaymentMethodRequest;
import com.braintree.command.result.BrainTreeAddressResult;
import com.braintree.command.result.BrainTreeCreatePaymentMethodResult;
import com.braintree.command.result.BrainTreeFindCustomerResult;
import com.braintree.command.result.BrainTreePaymentMethodResult;
import com.braintree.command.result.BrainTreeUpdatePaymentMethodResult;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.BraintreeConstants;
import com.braintree.converters.BraintreePaymentMethodConverter;
import com.braintree.customer.service.BrainTreeCustomerAccountService;
import com.braintree.facade.BrainTreeUserFacade;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.method.BrainTreePaymentService;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.dto.BraintreeInfo;
import com.braintree.payment.info.service.PaymentInfoService;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.braintreegateway.PayPalAccount;
import com.braintreegateway.PaymentMethodRequest;
import com.google.common.collect.Lists;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.impl.DefaultUserFacade;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.commerceservices.strategies.CustomerNameStrategy;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.user.AddressModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.payment.commands.request.CreateSubscriptionRequest;
import de.hybris.platform.payment.commands.result.SubscriptionResult;
import de.hybris.platform.payment.dto.BillingInfo;
import de.hybris.platform.servicelayer.dto.converter.Converter;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;




public class BrainTreeUserFacadeImpl extends DefaultUserFacade implements BrainTreeUserFacade
{

	private Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> brainTreePaymentInfoConverter;
	private BrainTreeCustomerAccountService brainTreeCustomerAccountService;
	private BrainTreePaymentService brainTreePaymentService;
	private CheckoutCustomerStrategy checkoutCustomerStrategy;
	private BrainTreeTransactionService brainTreeTransactionService;
	private CustomerNameStrategy customerNameStrategy;
	private Converter<AddressData, AddressModel> addressReverseConverter;
	private Converter<BrainTreeSubscriptionInfoData, BraintreeInfo> brainTreeSubscriptionInfoConverter;
	private PaymentInfoService paymentInfoService;
	private BraintreePaymentMethodConverter paymentMethodConverter;
	private BrainTreeConfigService brainTreeConfigService;
	private UserService userService;
	private ModelService modelService;
	private BlCustomerAccountService customerAccountService;

	@Override
	public void addAddress(final AddressData addressData)
	{
	  final CartModel sessionCart = getCartService().getSessionCart();
	  if(Objects.nonNull(sessionCart) && Objects.nonNull(sessionCart.getDeliveryAddress()))
	  {
	    final AddressModel deliveryAddress = sessionCart.getDeliveryAddress();
	    addressData.setUpsStoreAddress(Objects.nonNull(addressData.getPickStoreAddress()) 
	        ? addressData.getPickStoreAddress() : BooleanUtils.toBoolean(deliveryAddress.getUpsStoreAddress()));
	    addressData.setPickStoreAddress(Objects.nonNull(addressData.getUpsStoreAddress()) 
	        ? addressData.getUpsStoreAddress() : BooleanUtils.toBoolean(deliveryAddress.getPickStoreAddress()));
	  }
		final BrainTreeAddressRequest addressRequest = convertBrainTreeAddress(addressData);

		validateParameterNotNullStandardMessage("addressData", addressData);

		final CustomerModel currentCustomer = getCurrentUserForCheckout();

		final boolean makeThisAddressTheDefault = addressData.isDefaultAddress()
				|| (currentCustomer.getDefaultShipmentAddress() == null && addressData.isVisibleInAddressBook());

		// Create the new address model
		final AddressModel newAddress = getModelService().create(AddressModel.class);
		getAddressReversePopulator().populate(addressData, newAddress);

		if(CollectionUtils.isEmpty(currentCustomer.getAddresses())){
			currentCustomer.setName(newAddress.getFirstname());
		}
		// Store the address against the user
		getCustomerAccountService().saveAddressEntry(currentCustomer, newAddress);

		// Update the address ID in the newly created address
		addressData.setId(newAddress.getPk().toString());

		if (makeThisAddressTheDefault)
		{
			getCustomerAccountService().setDefaultAddressEntry(currentCustomer, newAddress);
		}
	}

	@Override
	public void editAddress(final AddressData addressData)
	{
		final String brainTreeAddressId = getBrainTreeAddressId(addressData);

		if (StringUtils.isNotEmpty(brainTreeAddressId))
		{
			final BrainTreeAddressRequest addressRequest = convertBrainTreeAddress(addressData);
			addressRequest.setAddressId(brainTreeAddressId);
			brainTreePaymentService.updateAddress(addressRequest);
		}

		super.editAddress(addressData);
	}

	@Override
	public void removeAddress(final AddressData addressData)
	{
		final String brainTreeAddressId = getBrainTreeAddressId(addressData);

		if (StringUtils.isNotEmpty(brainTreeAddressId))
		{
			final BrainTreeAddressRequest addressRequest = convertBrainTreeAddress(addressData);
			addressRequest.setAddressId(brainTreeAddressId);
			brainTreePaymentService.removeAddress(addressRequest);
		}

		super.removeAddress(addressData);
	}

	private String getBrainTreeAddressId(final AddressData addressData)
	{
		validateParameterNotNullStandardMessage("addressData", addressData);
		final CustomerModel currentCustomer = getCurrentUserForCheckout();
		final AddressModel addressModel = getCustomerAccountService().getAddressForCode(currentCustomer, addressData.getId());
		return addressModel.getBrainTreeAddressId();
	}

	private BrainTreeAddressRequest convertBrainTreeAddress(final AddressData address)
	{
		final String customerID = checkoutCustomerStrategy.getCurrentUserForCheckout().getBraintreeCustomerId();
		final BrainTreeAddressRequest addressRequest = new BrainTreeAddressRequest(customerID);
		addressRequest.setCompany(address.getTitle());
		addressRequest.setStreetAddress(address.getLine1());
		addressRequest.setExtendedAddress(address.getLine2());
		addressRequest.setFirstName(address.getFirstName());
		addressRequest.setLastName(address.getLastName());
		addressRequest.setLocality(address.getTown());
		addressRequest.setPostalCode(address.getPostalCode());

		if (address.getCountry() != null)
		{
			addressRequest.setCountryCodeAlpha2(address.getCountry().getIsocode());
		}
		if (address.getRegion() != null)
		{
			//The state or province. For PayPal addresses, the region must be a 2-letter abbreviation; for all other payment methods, it must be less than or equal to 255 characters.
			//because of hybris specific use the isocodeShort - 2 character isocode - and its right for braintree
			//the isocode return  2 character isocode US-CA - wrong for braintree
			addressRequest.setRegion(address.getRegion().getIsocodeShort());
		}

		return addressRequest;
	}

	@Override
	public boolean editPaymentMethod(CCPaymentInfoData paymentInfo, final String expirationDate,
									 final String cvv, final AddressData addressData, final String defaultCard)
	{
		validateParameterNotNullStandardMessage("paymentInfo", paymentInfo);
		final BrainTreeUpdatePaymentMethodRequest request = new BrainTreeUpdatePaymentMethodRequest(
				paymentInfo.getPaymentMethodToken());

		request.setToken(paymentInfo.getPaymentMethodToken());
		//request.setCardholderName(cardholderName);  // NOSONAR
		request.setCardExpirationDate(expirationDate);
		request.setCvv(cvv);
		if(StringUtils.isNotBlank(defaultCard) && Boolean.TRUE.toString().equals(defaultCard))
		{
			request.setDefault(true);
		}
		
		if (addressData != null)
		{
			request.setBillingAddressId(addressData.getBrainTreeAddressId());
		}

		BrainTreeUpdatePaymentMethodResult result = getBrainTreePaymentService().updatePaymentMethod(request);
		final CustomerModel currentCustomer = getCurrentUserForCheckout();
		final BrainTreePaymentInfoModel ccPaymentInfoModel = brainTreeCustomerAccountService.getBrainTreePaymentInfoForCode(
				currentCustomer, paymentInfo.getId());

		if (ccPaymentInfoModel != null)
		{
			getCustomerAccountService().setDefaultPaymentInfo(currentCustomer, ccPaymentInfoModel);
		}
		if (result.isSuccess())
		{
			final BrainTreePaymentInfoModel braintreePaymentInfo = getPaymentMethodConverter().convert(result.getPaymentMethod());
			if (braintreePaymentInfo != null)
			{
				getPaymentInfoService().update(braintreePaymentInfo, addressData);
			}
			return true;
		}
		throw new AdapterException(result.getErrorMessage());
	}

	@Override
    public List<CCPaymentInfoData> getBrainTreeCCPaymentInfos(final boolean saved) {
        final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
        final Collection<BrainTreePaymentInfoModel> paymentInfos = brainTreeCustomerAccountService.getBrainTreePaymentInfos(
                currentCustomer, saved);
        	
        final List<BrainTreePaymentInfoModel> creditCards = Lists.newArrayList();

        boolean isPayPalEnabled = getBrainTreeConfigService().getPayPalStandardEnabled();
        boolean isHostedFieldsEnabled = getBrainTreeConfigService().getHostedFieldEnabled();
        boolean isVenmoEnabled = getBrainTreeConfigService().getVenmoEnabled();
		if (isHostedFieldsEnabled && isPayPalEnabled && isVenmoEnabled) {
			addAllPaymentInfos(paymentInfos, creditCards);
		} else {
            for (final PaymentInfoModel paymentInfoModel : paymentInfos) {
                if (paymentInfoModel instanceof BrainTreePaymentInfoModel) {
                    String paymentProvider = ((BrainTreePaymentInfoModel) paymentInfoModel).getPaymentProvider();
                    if (isHostedFieldsEnabled && BraintreeConstants.BRAINTREE_PAYMENT.equals(paymentProvider)) {
                        creditCards.add((BrainTreePaymentInfoModel) paymentInfoModel);
                        break;
                    }
                    if (isPayPalEnabled
                            && (BraintreeConstants.PAYPAL_PAYMENT.equals(paymentProvider) || BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT.equals(paymentProvider))) {
                        creditCards.add((BrainTreePaymentInfoModel) paymentInfoModel);
                        break;
                    }
                    if (isVenmoEnabled && BraintreeConstants.VENMO_CHECKOUT.equals(paymentProvider)) {
                    		creditCards.add((BrainTreePaymentInfoModel) paymentInfoModel);
						  }
                }
            }
        }


        final List<CCPaymentInfoData> ccPaymentInfos = new ArrayList<CCPaymentInfoData>();
        final PaymentInfoModel defaultPaymentInfoModel = currentCustomer.getDefaultPaymentInfo();

        for (final BrainTreePaymentInfoModel ccPaymentInfoModel : creditCards) {
            final CCPaymentInfoData defaultPaymentInfoData = getBrainTreePaymentInfoConverter().convert(ccPaymentInfoModel);
            if (ccPaymentInfoModel.equals(defaultPaymentInfoModel)) {
                defaultPaymentInfoData.setDefaultPaymentInfo(true);
            }
       
            ccPaymentInfos.add(defaultPaymentInfoData);
        }
        return ccPaymentInfos;
    }

    /**
     * Adds the all credit card payment infos list.
     *
     * @param paymentInfos the payment infos
     * @param creditCards the credit cards
     */
    private void addAllPaymentInfos(Collection<BrainTreePaymentInfoModel> paymentInfos, List<BrainTreePaymentInfoModel> creditCards)
    {
      creditCards.addAll(paymentInfos.stream().filter(paymentInfoModel -> isCCPaymentProvider(paymentInfoModel)).collect(Collectors.toList()));
    }

    /**
     * Checks if Payment Provider is Credit Card payment.
     *
     * @param paymentInfoModel the payment info model
     * @return true, if is CC payment provider
     */
    private boolean isCCPaymentProvider(final BrainTreePaymentInfoModel paymentInfoModel)
    {
      return StringUtils.isNotBlank(paymentInfoModel.getPaymentProvider())
          && BraintreeConstants.BRAINTREE_CREDITCARD_PAYMENT.equalsIgnoreCase(paymentInfoModel.getPaymentProvider());
    }

	@Override
	public BrainTreePaymentInfoModel addPaymentMethod(final BrainTreeSubscriptionInfoData subscriptionInfoData)
	{
		validateParameterNotNullStandardMessage("subscriptionInfoData", subscriptionInfoData);
		final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
		return savePaymentMethod(subscriptionInfoData, currentCustomer);
	}

	private BrainTreePaymentInfoModel savePaymentMethod(final BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData,
														final CustomerModel customer)
	{
		createBrainTreeCustomerIfNotExist(customer);
		hasExistingPayPalAccount(customer, brainTreeSubscriptionInfoData);
		createBrainTreeAddressIfNotExist(brainTreeSubscriptionInfoData, customer);
		final BrainTreeCreateCreditCardPaymentMethodRequest request = new BrainTreeCreateCreditCardPaymentMethodRequest(
				customer.getBraintreeCustomerId());
		final AddressData addressData = brainTreeSubscriptionInfoData.getAddressData();
		if (addressData != null)
		{
			AddressModel billingAddress = convertAddressData(addressData, brainTreeSubscriptionInfoData);

			final PaymentMethodRequest paymentMethodRequest = new PaymentMethodRequest();
			paymentMethodRequest.paymentMethodNonce(brainTreeSubscriptionInfoData.getNonce())
					.customerId(customer.getBraintreeCustomerId()).cardholderName(brainTreeSubscriptionInfoData.getCardholder());

			if(getBrainTreeConfigService().getVerifyCardOnVaulting())
			{
				paymentMethodRequest.paymentMethodNonce(brainTreeSubscriptionInfoData.getNonce())
						.customerId(customer.getBraintreeCustomerId()).cardholderName(brainTreeSubscriptionInfoData.getCardholder())
						.options().verifyCard(getBrainTreeConfigService().getVerifyCard())
						.verificationMerchantAccountId(getBrainTreeConfigService().getMerchantAccountIdForCurrentSiteAndCurrency());
				
			}
			else
			{
				paymentMethodRequest.paymentMethodNonce(brainTreeSubscriptionInfoData.getNonce())
						.customerId(customer.getBraintreeCustomerId()).cardholderName(brainTreeSubscriptionInfoData.getCardholder());
			}

			paymentMethodRequest.billingAddressId(billingAddress.getBrainTreeAddressId());
			request.setIsDefault(brainTreeSubscriptionInfoData.getIsDefault());
			request.setRequest(paymentMethodRequest);
			final BrainTreePaymentMethodResult creditCardPaymentMethod = getBrainTreePaymentService().createCreditCardPaymentMethod(
					request);

			
			if (creditCardPaymentMethod.isSuccess())
			{
				addAdditionalPaymentMethodFields(brainTreeSubscriptionInfoData, creditCardPaymentMethod);
				final BraintreeInfo braintreeInfo = getBrainTreeSubscriptionInfoConverter().convert(brainTreeSubscriptionInfoData);
				return getBrainTreeTransactionService().createSubscription(billingAddress, customer, braintreeInfo);
			}
			else
			{
				throw new AdapterException(creditCardPaymentMethod.getErrorMessage());
			}
		}
		return null;
	}

	private void hasExistingPayPalAccount(CustomerModel customerModel, BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData){
		BrainTreeFindCustomerResult result = findBrainTreeCustomer(customerModel.getBraintreeCustomerId());
		if (result.isCustomerExist() && StringUtils.isNotBlank(brainTreeSubscriptionInfoData.getEmail())) {
			boolean isMethodExistInAccount = checkForExistingPaymentMethodForCustomer(customerModel, brainTreeSubscriptionInfoData.getEmail());
			for (PayPalAccount account : result.getCustomer().getPayPalAccounts()) {
				if (brainTreeSubscriptionInfoData.getEmail().equalsIgnoreCase(account.getEmail()) && isMethodExistInAccount){
					throw new AdapterException("This payment method is already exist.");
				}
			}
		}
	}

	private boolean checkForExistingPaymentMethodForCustomer(CustomerModel customer, String acctountEmail){
		final List<PaymentInfoModel> paymentInfoModels = new ArrayList<PaymentInfoModel>(customer.getPaymentInfos());
		for (PaymentInfoModel paymentInfo : paymentInfoModels) {
			if (paymentInfo.isSaved()
					&& paymentInfo instanceof BrainTreePaymentInfoModel
					&& BraintreeConstants.PAYPAL_PAYMENT.equals(((BrainTreePaymentInfoModel) paymentInfo).getPaymentProvider())
					&& acctountEmail.equals((paymentInfo).getBillingAddress().getEmail()))
			{
				return true;
			}
		}
		return false;
	}

	private BrainTreeFindCustomerResult findBrainTreeCustomer(String braintreeCustomerId){
		if (StringUtils.isEmpty(braintreeCustomerId))
		{
			return new BrainTreeFindCustomerResult(false);
		}
		final BrainTreeCustomerRequest findCustomerRequest = new BrainTreeCustomerRequest(braintreeCustomerId);
		findCustomerRequest.setCustomerId(braintreeCustomerId);
		final BrainTreeFindCustomerResult response = getBrainTreePaymentService().findCustomer(findCustomerRequest);
		return response;
	}

	private AddressModel convertAddressData(AddressData addressData, BrainTreeSubscriptionInfoData brainTreeSubscriptionInfoData)
	{
		AddressModel billingAddress = getModelService().create(AddressModel.class);
		Converter<AddressData, AddressModel> converter = getAddressReverseConverter();
		converter.convert(addressData, billingAddress);
		billingAddress.setBrainTreeAddressId(addressData.getBrainTreeAddressId());
		if (StringUtils.isNotBlank(brainTreeSubscriptionInfoData.getEmail()))
		{
			billingAddress.setEmail(brainTreeSubscriptionInfoData.getEmail());
		}
		else
		{
			billingAddress.setEmail(addressData.getEmail());
		}
		return billingAddress;

	}

	private void createBrainTreeCustomerIfNotExist(final CustomerModel currentCustomer)
	{
		if (StringUtils.isEmpty(currentCustomer.getBraintreeCustomerId()))
		{
			final BillingInfo billingInfo = new BillingInfo();
			billingInfo.setEmail(currentCustomer.getContactEmail());

			final String[] names = getCustomerNameStrategy().splitName(currentCustomer.getName());
			if (names != null)
			{
				billingInfo.setFirstName(names[0]);
				billingInfo.setLastName(names[1]);
			}

			final SubscriptionResult customerSubscription = getBrainTreePaymentService().createCustomerSubscription(
					new CreateSubscriptionRequest(null, billingInfo, null, null, null, null, null));

			currentCustomer.setBraintreeCustomerId(customerSubscription.getMerchantTransactionCode());
			getModelService().save(currentCustomer);
		}
	}

	private void createBrainTreeAddressIfNotExist(final BrainTreeSubscriptionInfoData subscriptionInfoData,
												  final CustomerModel currentCustomer)
	{
		final AddressData addressData = subscriptionInfoData.getAddressData();
		if (addressData != null && StringUtils.isEmpty(addressData.getBrainTreeAddressId()))
		{
			final BrainTreeAddressRequest addressRequest = convertBrainTreeAddress(addressData);
			final BrainTreeAddressResult brainTreeAddress = getBrainTreePaymentService().createAddress(addressRequest,
					currentCustomer);
			if (brainTreeAddress != null && brainTreeAddress.getAddress() != null)
			{
				addressData.setBrainTreeAddressId(brainTreeAddress.getAddress().getId());
				updateCurrentAddress(currentCustomer, addressData, brainTreeAddress);
			}
		}
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
			brainTreeSubscriptionInfoData.setIsDefault(createPaymentMethodResult.getIsDefault());

			if (StringUtils.isNotBlank(createPaymentMethodResult.getEmail()))
			{
				brainTreeSubscriptionInfoData.setEmail(createPaymentMethodResult.getEmail());
			}
		}
	}

	private void updateCurrentAddress(final CustomerModel currentCustomer, final AddressData addressData,
									  final BrainTreeAddressResult brainTreeAddress)
	{
		for (final AddressModel addressModel : currentCustomer.getAddresses())
		{
			if (addressModel.getPk().toString().equals(addressData.getId()))
			{
				addressModel.setBrainTreeAddressId(brainTreeAddress.getAddress().getId());
				getModelService().save(addressModel);
			}
		}
		getModelService().save(currentCustomer);
	}

	@Override
	public void unlinkCCPaymentInfo(final String id)
	{
		validateParameterNotNullStandardMessage("id", id);
		final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();

		final BrainTreePaymentInfoModel brainTreePaymentInfo = brainTreeCustomerAccountService.getBrainTreePaymentInfoForCode(
				currentCustomer, id);

		brainTreeCustomerAccountService.unlinkCCPaymentInfo(currentCustomer, brainTreePaymentInfo);
	}

	@Override
	public void removeBTCCPaymentInfo(final String paymentMethodToken)
	{
		validateParameterNotNullStandardMessage("paymentMethodToken", paymentMethodToken);
		final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
		final BrainTreeDeletePaymentMethodRequest request = new BrainTreeDeletePaymentMethodRequest(
				currentCustomer.getBraintreeCustomerId(), paymentMethodToken);
		brainTreePaymentService.deletePaymentMethod(request);
	}

	@Override
	public CCPaymentInfoData getCCPaymentInfoForCode(final String code)
	{
		if (StringUtils.isNotBlank(code))
		{
			final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
			final BrainTreePaymentInfoModel ccPaymentInfoModel = brainTreeCustomerAccountService.getBrainTreePaymentInfoForCode(
					currentCustomer, code);
			if (ccPaymentInfoModel != null)
			{
				final PaymentInfoModel defaultPaymentInfoModel = currentCustomer.getDefaultPaymentInfo();
				final CCPaymentInfoData paymentInfoData = getBrainTreePaymentInfoConverter().convert(ccPaymentInfoModel);
				if (ccPaymentInfoModel.equals(defaultPaymentInfoModel))
				{
					paymentInfoData.setDefaultPaymentInfo(true);
				}
				return paymentInfoData;
			}
		}

		return null;
	}

	@Override
	public void setDefaultPaymentInfo(final CCPaymentInfoData paymentInfoData)
	{
		validateParameterNotNullStandardMessage("paymentInfoData", paymentInfoData);
		final BrainTreeUpdatePaymentMethodRequest request = new BrainTreeUpdatePaymentMethodRequest(
				paymentInfoData.getPaymentMethodToken());

		request.setToken(paymentInfoData.getPaymentMethodToken());
		request.setDefault(true);
		BrainTreeUpdatePaymentMethodResult result = getBrainTreePaymentService().updatePaymentMethod(request);

		final CustomerModel currentCustomer = getCurrentUserForCheckout();
		final BrainTreePaymentInfoModel ccPaymentInfoModel = brainTreeCustomerAccountService.getBrainTreePaymentInfoForCode(
				currentCustomer, paymentInfoData.getId());

		if (result.isSuccess())
		{
			final BrainTreePaymentInfoModel braintreePaymentInfo = getPaymentMethodConverter().convert(result.getPaymentMethod());
			if (braintreePaymentInfo != null)
			{
				getPaymentInfoService().update(braintreePaymentInfo.getPaymentMethodToken(), braintreePaymentInfo);
			}

		}
		if (ccPaymentInfoModel != null)
		{
			getCustomerAccountService().setDefaultPaymentInfo(currentCustomer, ccPaymentInfoModel);
		}
	}
	
	/**
	 * @return the checkoutCustomerStrategy
	 */
	@Override
	public CheckoutCustomerStrategy getCheckoutCustomerStrategy()
	{
		return checkoutCustomerStrategy;
	}

	/**
	 * @param checkoutCustomerStrategy
	 *           the checkoutCustomerStrategy to set
	 */
	@Override
	public void setCheckoutCustomerStrategy(final CheckoutCustomerStrategy checkoutCustomerStrategy)
	{
		this.checkoutCustomerStrategy = checkoutCustomerStrategy;
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

	public Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> getBrainTreePaymentInfoConverter()
	{
		return brainTreePaymentInfoConverter;
	}

	public void setBrainTreePaymentInfoConverter(
			final Converter<BrainTreePaymentInfoModel, CCPaymentInfoData> brainTreePaymentInfoConverter)
	{
		this.brainTreePaymentInfoConverter = brainTreePaymentInfoConverter;
	}

	/**
	 * @return the brainTreeCustomerAccountService
	 */
	public BrainTreeCustomerAccountService getBrainTreeCustomerAccountService()
	{
		return brainTreeCustomerAccountService;
	}

	/**
	 * @param brainTreeCustomerAccountService
	 *           the brainTreeCustomerAccountService to set
	 */
	public void setBrainTreeCustomerAccountService(final BrainTreeCustomerAccountService brainTreeCustomerAccountService)
	{
		this.brainTreeCustomerAccountService = brainTreeCustomerAccountService;
	}

	/**
	 * @return the customerNameStrategy
	 */
	public CustomerNameStrategy getCustomerNameStrategy()
	{
		return customerNameStrategy;
	}

	/**
	 * @param customerNameStrategy
	 *           the customerNameStrategy to set
	 */
	public void setCustomerNameStrategy(final CustomerNameStrategy customerNameStrategy)
	{
		this.customerNameStrategy = customerNameStrategy;
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
	 * This method is responsible for setting default billing address.
	 * @param addressData
	 */
	@Override
	public void setDefaultBillingAddress(final AddressData addressData)
	{
		validateParameterNotNullStandardMessage("addressData", addressData);
		final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
		final AddressModel addressModel = getCustomerAccountService().getAddressForCode(currentCustomer, addressData.getId());
		if (addressModel != null)
		{
			getCustomerAccountService().setDefaultBillingAddress(currentCustomer, addressModel);
		}
	}

	/**
	 * This method is responsible for getting default billing address.
	 */
	@Override
	public AddressData getDefaultBillingAddress()
	{
		final CustomerModel currentCustomer = (CustomerModel) getUserService().getCurrentUser();
		AddressData defaultBillingAddressData = null;

		final AddressModel defaultBillingAddress =currentCustomer.getDefaultBillingAddress();
		if (defaultBillingAddress != null)
		{
			defaultBillingAddressData = getAddressConverter().convert(defaultBillingAddress);
		}
		return defaultBillingAddressData;
	}

	/**
	 * This method is override to get default billing address whiling fetching address book.
	 */
	@Override
	public List<AddressData> getAddressBook() {
		// Get the current customer's addresses
		final CustomerModel currentUser = (CustomerModel) getUserService().getCurrentUser();
		final Collection<AddressModel> addresses = getCustomerAccountService()
				.getAddressBookDeliveryEntries(currentUser);

		if (CollectionUtils.isNotEmpty(addresses)) {
			final List<AddressData> addressBook = new ArrayList<>();
			final AddressData defaultAddress = getDefaultAddress();
			final AddressData defaultBillingAddress = getDefaultBillingAddress();

			for (final AddressModel address : addresses)
			{
				final AddressData addressData = getAddressConverter().convert(address);

				if (defaultBillingAddress!= null && StringUtils.isNotEmpty(defaultBillingAddress.getId()) && StringUtils.equals(defaultBillingAddress.getId(),addressData.getId())){
					addressData.setDefaultBillingAddress(Boolean.TRUE);
				}

				if (defaultAddress != null && StringUtils.isNotEmpty(defaultAddress.getId()) && StringUtils.equals(defaultAddress.getId(),addressData.getId()))
				{
					addressData.setDefaultAddress(true);
					addressBook.add(0, addressData);
				}
				else
				{
					addressBook.add(addressData);
				}
			}
			return addressBook;
		}
		return Collections.emptyList();
	}

	/**
	 *{@inheritDoc}
	 */
	@Override
	public List<AddressData> getShippingAddressBook() {

		// Get the current customer's addresses
		final CustomerModel currentUser = (CustomerModel) getUserService().getCurrentUser();
		final Collection<AddressModel> addresses = getCustomerAccountService()
				.getShippingAddressBookEntries(currentUser);

		if (CollectionUtils.isNotEmpty(addresses)) {
			final List<AddressData> addressBook = new ArrayList<>();
			final AddressData defaultAddress = getDefaultAddress();
			for (final AddressModel address : addresses)
			{
				final AddressData addressData = getAddressConverter().convert(address);
				if (defaultAddress != null && StringUtils.isNotEmpty(defaultAddress.getId()) && StringUtils.equals(defaultAddress.getId(),addressData.getId()))
				{
					addressData.setDefaultAddress(true);
					addressBook.add(0, addressData);
				}
				else
				{
					addressBook.add(addressData);
				}
			}
			return addressBook;
		}
		return Collections.emptyList();
	}

	public PaymentInfoService getPaymentInfoService()
	{
		return paymentInfoService;
	}

	public void setPaymentInfoService(PaymentInfoService paymentInfoService)
	{
		this.paymentInfoService = paymentInfoService;
	}

	public BraintreePaymentMethodConverter getPaymentMethodConverter()
	{
		return paymentMethodConverter;
	}

	public void setPaymentMethodConverter(BraintreePaymentMethodConverter paymentMethodConverter)
	{
		this.paymentMethodConverter = paymentMethodConverter;
	}

	public BrainTreeConfigService getBrainTreeConfigService() {
		return brainTreeConfigService;
	}

	public void setBrainTreeConfigService(BrainTreeConfigService brainTreeConfigService) {
		this.brainTreeConfigService = brainTreeConfigService;
	}

	@Override
	public UserService getUserService()	{return userService;}
	@Override
	public void setUserService(UserService userService)
	{
		this.userService = userService;
	}

	@Override
	public ModelService getModelService() {	return modelService;}

	@Override
	public void setModelService(ModelService modelService)
	{
		this.modelService = modelService;
	}

	@Override
	public BlCustomerAccountService getCustomerAccountService() {
		return customerAccountService;
	}

	public void setCustomerAccountService(
			BlCustomerAccountService customerAccountService) {
		this.customerAccountService = customerAccountService;
	}
}
