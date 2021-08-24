package com.braintree.controllers.pages;

import static com.braintree.controllers.BraintreeaddonControllerConstants.CLIENT_TOKEN;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;

import com.bl.facades.customer.BlCustomerFacade;
import com.bl.facades.order.BlOrderFacade;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.braintree.controllers.BraintreeaddonControllerConstants;
import com.braintree.exceptions.ResourceErrorMessage;
import com.braintree.facade.BrainTreeUserFacade;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.validators.PaymentMethodValidator;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.Breadcrumb;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.SopPaymentDetailsForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.payment.AdapterException;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import java.math.RoundingMode;




@Controller
@RequestMapping("/my-account")
public class BrainTreeAccountPageController extends AbstractPageController
{
	private static final String PAY_BILL = "/payBill";
	private static final String MY_ACCOUNT = "/my-account/";
	private static final String MY_ACCOUNT_PAYMENT_DETAILS = "/my-account/payment-details";
	private static final Logger LOGGER = Logger.getLogger(BrainTreeAccountPageController.class);
	private static final String REDIRECT_TO_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + MY_ACCOUNT_PAYMENT_DETAILS;
	private static final String REDIRECT_TO_ADD_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/add-payment-method";
	private static final String REDIRECT_TO_EDIT_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/edit-payment-method";

	// CMS Page
	private static final String ADD_EDIT_PAYMENT_METHOD_CMS_PAGE = "add-edit-payment-method";
	private static final String EDIT_PAYMENT_METHOD_CMS_PAGE = "edit-payment-method";
	
	private static final String PAY_BILL_CMS_PAGE = "pay-bill";

	@Resource(name = "userFacade")
	protected BrainTreeUserFacade userFacade;

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "accountBreadcrumbBuilder")
	private ResourceBreadcrumbBuilder accountBreadcrumbBuilder;

	@Resource(name = "paymentMethodValidator")
	private PaymentMethodValidator paymentMethodValidator;

	@Resource(name = "customerFacade")
	private BlCustomerFacade blCustomerFacade;
	
	@Resource()
	private BrainTreeTransactionService brainTreeTransactionService;
	
	@Resource(name = "orderFacade")
	private OrderFacade orderFacade;
	
	@Resource(name = "blOrderFacade")
	private BlOrderFacade blOrderFacade;

	@Resource(name = "priceDataFactory")
	private PriceDataFactory priceDataFactory;
	
	@RequestMapping(value = "/remove-payment-method-bt", method = RequestMethod.POST)
	@RequireHardLogIn
	public String removePaymentMethod(@RequestParam(value = "paymentInfoIdRemove") final String paymentInfoId,
                                      @RequestParam(value = "paymentMethodTokenRomove") final String paymentMethodNonce, final RedirectAttributes redirectAttributes)
			throws CMSItemNotFoundException
	{
		userFacade.unlinkCCPaymentInfo(paymentInfoId);
		userFacade.removeBTCCPaymentInfo(paymentMethodNonce);
		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				getLocalizedString("text.account.profile.paymentCart.remove"));

		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}


	@RequestMapping(value = "/edit-payment-method", method = RequestMethod.GET)
	@RequireHardLogIn
	public String editPaymentMethod(final Model model, @RequestParam(value = "paymentInfoId") final String paymentMethodId,
                                    @RequestParam(value = "cardholder", required = false) final String cardholder,
                                    @RequestParam(value = "errorMessage", required = false) final String errorMessage,
                                    @RequestParam(value = "expirationDate", required = false) final String expirationDate) throws CMSItemNotFoundException
	{
		final List<Breadcrumb> breadcrumbs = accountBreadcrumbBuilder.getBreadcrumbs(null);
		breadcrumbs.add(new Breadcrumb(MY_ACCOUNT_PAYMENT_DETAILS,
				getMessageSource().getMessage("text.account.paymentDetails", null, getI18nService().getCurrentLocale()), null));
		breadcrumbs.add(new Breadcrumb("#",getLocalizedString("text.account.paymentMethod.editPaymentMethod"), null));

		storeCmsPageInModel(model, getContentPageForLabelOrId(EDIT_PAYMENT_METHOD_CMS_PAGE));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(EDIT_PAYMENT_METHOD_CMS_PAGE));
		List<AddressData> addressBook = userFacade.getAddressBook();
		CCPaymentInfoData ccPaymentInfo = userFacade.getCCPaymentInfoForCode(paymentMethodId);

		if (errorMessage != null && !model.containsAttribute("accErrorMsgs"))
		{
			GlobalMessages.addErrorMessage(model, errorMessage);
		}

		model.addAttribute("errorMessage", cardholder);
		model.addAttribute("cardholder", cardholder);
		model.addAttribute("expirationDate", expirationDate);
		model.addAttribute("ccPaymentInfo", ccPaymentInfo);
		model.addAttribute("deliveryAddresses", addressBook);
		model.addAttribute("breadcrumbs", breadcrumbs);
		model.addAttribute("selectedPaymentMethodId", paymentMethodId);
		model.addAttribute("metaRobots", "noindex,nofollow");
		return getViewForPage(model);
	}

	@RequestMapping(value = "/edit-payment-method", method = RequestMethod.POST)
	@RequireHardLogIn
	public String editPaymentMethod(final Model model, @RequestParam(value = "paymentInfoId") final String paymentMethodId,
                                    @RequestParam(value = "billingAddressId") final String billingAddressId,
                                    @RequestParam(value = "expirationDate") final String expirationDate, @RequestParam(value = "cvv") final String cvv,
                                    @RequestParam(value = "default_Card") final String defaultCard,
									final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		try
		{
			String updatedexpirationDate = expirationDate.replace(",", "");
			ResourceErrorMessage validationMessage = paymentMethodValidator.validate(updatedexpirationDate, cvv);
			if (validationMessage != null && StringUtils.isNotBlank(validationMessage.getMessageKey()))
			{
				String localizedErrorMessage = getLocalizedString(validationMessage.getMessageKey());
				LOGGER.error("Failed to edit payment method. Error occurred while contacting external payment services.");
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);
				return redirectToEditPage(paymentMethodId, updatedexpirationDate, redirectAttributes, localizedErrorMessage);
			}
			else
			{
				CCPaymentInfoData ccPaymentInfo = userFacade.getCCPaymentInfoForCode(paymentMethodId);

				List<AddressData> addressBook = userFacade.getAddressBook();
				AddressData addressData = getAddressForPaymentInfo(ccPaymentInfo, billingAddressId, addressBook);

				userFacade.editPaymentMethod(ccPaymentInfo, updatedexpirationDate, cvv, addressData, defaultCard);
			}
		}
		catch (AdapterException e)
		{
			String localizedErrorMessage = getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.general")
					+ e.getMessage();
			LOGGER.error("Failed to edit payment method. Error occurred while contacting external payment services.", e);
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);

			return redirectToEditPage(paymentMethodId, expirationDate, redirectAttributes, localizedErrorMessage);
		}
		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				getLocalizedString("text.account.profile.paymentCart.editPaymentMethod.success"));
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}

	private String redirectToEditPage(final String paymentMethodId,  final String expirationDate,
                                      RedirectAttributes redirectAttributes, final String localizedErrorMessage)
	{
        redirectAttributes.addAttribute("paymentInfoId", paymentMethodId);

//        redirectAttributes.addAttribute("cardholder", cardholder); // NOSONAR
        redirectAttributes.addAttribute("expirationDate", expirationDate);
        redirectAttributes.addAttribute("errorMessage", localizedErrorMessage);
		return REDIRECT_TO_EDIT_PAYMENT_INFO_PAGE;
	}

	@RequestMapping(value = "/receive-address", method = RequestMethod.GET)
	@RequireHardLogIn
	@ResponseBody
	public String receiveAddress(@RequestParam(value = "selectedAddressCode", required = false) String selectedAddressCode)
			throws CMSItemNotFoundException, IOException
	{
		if (StringUtils.isNotBlank(selectedAddressCode))
		{
			final ObjectMapper mapper = new ObjectMapper();
			List<AddressData> addressBook = userFacade.getAddressBook();
			AddressData addressForPaymentInfo = getAddressForPaymentInfo(selectedAddressCode, addressBook);
			return mapper.writeValueAsString(addressForPaymentInfo);
		}
		return StringUtils.EMPTY;
	}

	@RequestMapping(value = "/add-payment-method", method = RequestMethod.GET)
	@RequireHardLogIn
	public String addPaymentMethod(@RequestParam(value = "orderId", required = false) final String orderCode,
			final Model model, final String selectedAddressCode) throws CMSItemNotFoundException {

		final List<Breadcrumb> breadcrumbs = accountBreadcrumbBuilder.getBreadcrumbs(null);
		breadcrumbs.add(new Breadcrumb(MY_ACCOUNT_PAYMENT_DETAILS,
				getMessageSource().getMessage("text.account.paymentDetails", null, getI18nService().getCurrentLocale()), null));
		breadcrumbs.add(new Breadcrumb("#",
				getMessageSource().getMessage("text.account.profile.paymentMethod.add", null, getI18nService().getCurrentLocale()), null));

		storeCmsPageInModel(model, getContentPageForLabelOrId(ADD_EDIT_PAYMENT_METHOD_CMS_PAGE));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(ADD_EDIT_PAYMENT_METHOD_CMS_PAGE));
		setupAdditionalFields(model);
		final List<AddressData> addressBook = userFacade.getAddressBook();
		model.addAttribute("deliveryAddresses", addressBook);

	//	model.addAttribute("selectedAddressCode", selectedAddressCode); // NOSONAR
		model.addAttribute("billingAddresses", blCustomerFacade.getAllVisibleBillingAddressesOnUser());
		model.addAttribute("defaultBillingAddress", blCustomerFacade.getDefaultBillingAddress());
		model.addAttribute("addressForm", new AddressForm());
		model.addAttribute("breadcrumbs", breadcrumbs);
		model.addAttribute("metaRobots", "noindex,nofollow");
		model.addAttribute("orderCode", orderCode);
		return getViewForPage(model);
	}

	private void setupAdditionalFields(final Model model)
	{
		String clientToken = StringUtils.EMPTY;

		try
		{
			clientToken = brainTreeCheckoutFacade.generateClientToken();
		}
		catch (final AdapterException exception)
		{
			LOGGER.error("[Brain Tree Controller] Error during token generation!");
		}

		model.addAttribute(CLIENT_TOKEN, clientToken);
	}

	@RequestMapping(value = "/add-payment-method", method = RequestMethod.POST)
	@RequireHardLogIn

	public String addPaymentMethod(@RequestParam(value = "bt_payment_method_nonce") final String nonce,   // NOSONAR
			@RequestParam(value = "payment_type") final String paymentProvider,
			@RequestParam(value = "paypal_email") final String payPalEmail,
			@RequestParam(value = "card_type") final String cardType,
			@RequestParam(value = "card_details") final String cardDetails,
			@RequestParam(value = "device_data") final String deviceData,
			@RequestParam(value = "liability_shifted") final String liabilityShifted,
			@RequestParam(value = "save_billing_address") final String saveBillingAddress,
		    @RequestParam(value = "company_name") final String companyName,
			@RequestParam(value = "selected_Billing_Address_Id") final String selectedAddressCode,
			@RequestParam(value = "cardholder", required = false) final String cardholder,
			@RequestParam(value = "default_Card") final String defaultCard,@RequestParam(value = "orderCode", required = false) final String orderCode,
			final RedirectAttributes redirectAttributes, @Valid final SopPaymentDetailsForm sopPaymentDetailsForm) throws CMSItemNotFoundException
	{
		if (StringUtils.isEmpty(nonce))
		{
			LOGGER.error("Failed to create payment method. Error occurred while contacting external payment services.");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.noNonce"));
			return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
		}

		final String storeInVault = getSiteConfigService().getProperty("braintree.store.in.vault");
		if (storeInVault != null && !Boolean.parseBoolean(storeInVault))
		{
			LOGGER.error(
					"Failed to create payment method. Store in vault is forbidden. For adding new payment methods enable store in vault.");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.forbidden"));
			return REDIRECT_TO_PAYMENT_INFO_PAGE;
		}
		
		if (StringUtils.isBlank(selectedAddressCode))
	    {
	      try
	      {
	        final AddressData newAddress = interpretResponseAddressData(StringUtils.EMPTY, sopPaymentDetailsForm, companyName);
	        newAddress.setVisibleInAddressBook(StringUtils.isNotBlank(saveBillingAddress) && Boolean.TRUE.toString().equals(saveBillingAddress));
	        getUserFacade().addAddress(newAddress);
	      }
	      catch (final Exception exception)
	      {
	    	  LOGGER.error("Error occurred while adding new billing address", exception);
	      }
	    }
		
		final AddressData addressData = interpretResponseAddressData(selectedAddressCode, sopPaymentDetailsForm, companyName);
	
		if (addressData == null)
		{
			LOGGER.error("Failed to create payment method. Error occurred while address selection");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.noAddress"));
			return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
		}

		final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo(nonce, paymentProvider, cardDetails, cardType,
				payPalEmail, deviceData, liabilityShifted, addressData, cardholder, defaultCard);

		try
		{
			final BrainTreePaymentInfoModel paymentMethod = userFacade.addPaymentMethod(subscriptionInfo);
			if (paymentMethod == null)
			{
				LOGGER.error("Failed to create payment method. Error occurred while payment method creation");
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
						getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.error"));
				return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
			}
		}
		catch (final AdapterException e)
		{
			LOGGER.error("Failed to create payment method. Error occurred while payment method creation: " + e.getMessage());
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.general.error"));
			return REDIRECT_TO_PAYMENT_INFO_PAGE;
		}

		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.success"));

		if(StringUtils.isNotBlank(orderCode)) {
			String originalOrderCode = orderCode.replace(BlControllerConstants.RATIO + getRedirectionUrl(orderCode), BlControllerConstants.EMPTY);
			if(getRedirectionUrl(orderCode).equalsIgnoreCase(BlControllerConstants.EXTEND)) {
				return REDIRECT_PREFIX + BlControllerConstants.MY_ACCOUNT_EXTEND_RENTAL + originalOrderCode;
			}
			else {
				return REDIRECT_PREFIX + MY_ACCOUNT + originalOrderCode + PAY_BILL;
			}
		}
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}

	/**
	 * This method created for the PayBill page. 
	 */
	@GetMapping(value = "/{orderCode}/payBill")
	@RequireHardLogIn
	public String getPayBillDetailsForOrder(@PathVariable(value = "orderCode" ,required = false) final String orderCode, final Model model) throws CMSItemNotFoundException{
		final ContentPageModel payBillPage = getContentPageForLabelOrId(PAY_BILL_CMS_PAGE);
		storeCmsPageInModel(model, payBillPage);
		setUpMetaDataForContentPage(model, payBillPage);
		final OrderData orderDetails = blOrderFacade.getOrderDetailsForCode(orderCode);
		blOrderFacade.setPayBillAttributes(orderDetails);
		model.addAttribute("orderData", orderDetails);
		model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
		setupAdditionalFields(model);
		return getViewForPage(model);
	}
	
	/**
	 * This method created for the PayBill confirmation page. 
	 */
	@PostMapping(value = "/payBillSuccess")
	@RequireHardLogIn
	public String getPayBillDetailsForOrder(final Model model, final HttpServletRequest request,
			final HttpServletResponse response) throws CMSItemNotFoundException{
		String orderCode = request.getParameter("orderCode");
		String paymentInfoId = request.getParameter("paymentId");
		String paymentMethodNonce = request.getParameter("paymentNonce");
		String billPayTotal = request.getParameter("payBillTotal");
		String poNumber = request.getParameter("extendPoNumber");
		String poNotes = request.getParameter("extendPoNotes");

		boolean isSuccess = false;
		double payBillAmount = Double.parseDouble(billPayTotal);
		AbstractOrderModel order = null;
		if ((StringUtils.isNotBlank(orderCode) && StringUtils.isNotBlank(paymentInfoId) &&
				StringUtils.isNotBlank(paymentMethodNonce)) || StringUtils.isNotBlank(poNumber) ) {
			order = brainTreeCheckoutFacade.getOrderByCode(orderCode);

				isSuccess = payBillSuccess(model, paymentInfoId, paymentMethodNonce, payBillAmount, poNumber,
						poNotes, order);

		}

		if (isSuccess) {
			final OrderData orderDetails = orderFacade.getOrderDetailsForCode(orderCode);
			order = brainTreeCheckoutFacade.getOrderByCode(orderCode);
		    PriceData payBillTotal  = convertDoubleToPriceData(payBillAmount, order);
			orderDetails.setOrderTotalWithTaxForPayBill(payBillTotal);
			model.addAttribute("orderData", orderDetails);
			brainTreeCheckoutFacade.setPayBillFlagTrue(order);
			final ContentPageModel payBillSuccessPage = getContentPageForLabelOrId(
					BraintreeaddonControllerConstants.PAY_BILL_SUCCESS_CMS_PAGE);
			storeCmsPageInModel(model, payBillSuccessPage);
			setUpMetaDataForContentPage(model, payBillSuccessPage);
			model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS,
					ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
			return getViewForPage(model);
		} else {
			return REDIRECT_PREFIX + MY_ACCOUNT + orderCode + PAY_BILL;
		}
	}

    /**
     * This method will return true after Authorization and capture done successfully
     *
     * @param paymentInfoId
     * @param paymentMethodNonce
     * @param billPayTotal
     * @param poNumber
     * @param poNotes
     * @return boolean
     *
     */
	private boolean payBillSuccess(final Model model, String paymentInfoId, String paymentMethodNonce,
			double billPayTotal, String poNumber, String poNotes,  final AbstractOrderModel order) {
		boolean isSuccess = false;
		if (null != order && StringUtils.isNotBlank(poNumber)) {
			isSuccess = blOrderFacade.savePoPaymentForPayBillOrder(poNumber, poNotes, order.getCode());
			if (BooleanUtils.isTrue(isSuccess)) {
				model.addAttribute(BlControllerConstants.PAYMENT_METHOD, BlControllerConstants.PO);
			}
		} else if(null != order) {
			final BrainTreePaymentInfoModel paymentInfo = brainTreeCheckoutFacade
					.getBrainTreePaymentInfoForCode(
							(CustomerModel) order.getUser(), paymentInfoId, paymentMethodNonce);
			if (null != paymentInfo) {

				isSuccess = brainTreeTransactionService
						.createAuthorizationTransactionOfOrder(order,
								BigDecimal.valueOf(billPayTotal).setScale(2, RoundingMode.HALF_EVEN), true, paymentInfo);
			}
			if (BooleanUtils.isTrue(isSuccess)) {
				model.addAttribute(BlControllerConstants.PAYMENT_METHOD, BlControllerConstants.CREDIT_CARD);
			}
		}
		return isSuccess;
	}

	private BrainTreeSubscriptionInfoData buildSubscriptionInfo(final String nonce, final String paymentProvider,                         // NOSONAR
                                                                final String cardDetails, final String cardType, final String email, final String deviceData,
                                                                final String liabilityShifted, final AddressData addressData, final String cardholder, final String defaultCard  )
	{
		final BrainTreeSubscriptionInfoData subscriptionInfo = new BrainTreeSubscriptionInfoData();
		subscriptionInfo.setPaymentProvider(paymentProvider);
		subscriptionInfo.setCardNumber(cardDetails);
		subscriptionInfo.setDeviceData(deviceData);
		subscriptionInfo.setCardType(cardType);
		subscriptionInfo.setNonce(nonce);
		subscriptionInfo.setEmail(email);
		subscriptionInfo.setAddressData(addressData);
		subscriptionInfo.setSavePaymentInfo(Boolean.TRUE);
		subscriptionInfo.setShouldBeSaved(Boolean.TRUE);
		subscriptionInfo.setCardholder(cardholder);
		//Added condition for default Card
		if(StringUtils.isNotBlank(defaultCard) && Boolean.TRUE.toString().equals(defaultCard))
		{
			subscriptionInfo.setIsDefault(Boolean.TRUE);
		}
		
		if (StringUtils.isNotBlank(liabilityShifted))
		{
			subscriptionInfo.setLiabilityShifted(Boolean.valueOf(liabilityShifted));
		}

		return subscriptionInfo;
	}

	private AddressData getAddressForPaymentInfo(String selectedAddressCode, List<AddressData> addressBook)
	{
		if (StringUtils.isNotBlank(selectedAddressCode))
		{
			for (AddressData addressData : addressBook)
			{
				if (selectedAddressCode.equals(addressData.getId()))
				{
					return addressData;
				}
			}
		}
		return null;
	}

	private AddressData getAddressForPaymentInfo(CCPaymentInfoData ccPaymentInfo, String selectedAddressCode,
                                                 List<AddressData> addressBook)
	{
		AddressData addressForPaymentInfo = getAddressForPaymentInfo(selectedAddressCode, addressBook);
		if (addressForPaymentInfo == null)
		{
			return ccPaymentInfo.getBillingAddress();
		}
		return addressForPaymentInfo;
	}
	
	private AddressData interpretResponseAddressData(final String selectedAddressId, final SopPaymentDetailsForm sopPaymentDetailsForm, 
		      final String companyName)
		  {
		    if (StringUtils.isNotBlank(selectedAddressId))
		    {
		      return blCustomerFacade.getAddressForCode(selectedAddressId);
		    }
		    final AddressData address = new AddressData();
		    final CountryData country = new CountryData();
		    country.setIsocode("US");
		    address.setCountry(country);
		    final RegionData region = new RegionData();
		    region.setIsocode(sopPaymentDetailsForm.getBillTo_state());
		    address.setRegion(region);
		    address.setTitleCode(sopPaymentDetailsForm.getBillTo_titleCode());
		    address.setFirstName(sopPaymentDetailsForm.getBillTo_firstName());
		    address.setLastName(sopPaymentDetailsForm.getBillTo_lastName());
		    address.setCompanyName(companyName);
		    address.setTown(sopPaymentDetailsForm.getBillTo_city());
		    address.setLine1(sopPaymentDetailsForm.getBillTo_street1());
		    address.setLine2(sopPaymentDetailsForm.getBillTo_street2());
		    address.setPostalCode(sopPaymentDetailsForm.getBillTo_postalCode());
		    address.setEmail(sopPaymentDetailsForm.getBillTo_email());
		    address.setPhone(sopPaymentDetailsForm.getBillTo_phoneNumber());
		    address.setBillingAddress(Boolean.TRUE);
		    address.setShippingAddress(Boolean.FALSE);
		    address.setPickStoreAddress(Boolean.FALSE);
		    address.setUpsStoreAddress(Boolean.FALSE);
		    return address;
		  }
	
	//Added method to set default credit card from frontend
	@RequestMapping(value = "/set-default-cc-payment-details", method = RequestMethod.POST)
	@RequireHardLogIn
	public String setDefaultPaymentDetails(@RequestParam(value = "paymentInfoId") final String paymentInfoId) {
		CCPaymentInfoData paymentInfoData = null;

		if (StringUtils.isNotBlank(paymentInfoId)) {
			paymentInfoData = userFacade.getCCPaymentInfoForCode(paymentInfoId);
		}
		userFacade.setDefaultPaymentInfo(paymentInfoData);
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}
	


	/**
	 * This method create to get the URL based on order code
	 * @param orderCode orderCode
	 * @return string
	 */
	private String getRedirectionUrl(final String orderCode) {
		return orderCode.contains(BlControllerConstants.EXTEND) ? BlControllerConstants.EXTEND : BlControllerConstants.PAY_BILL;
	}

	/**
	 * This method converts double to price data
	 */
	private PriceData convertDoubleToPriceData(final Double price , final AbstractOrderModel orderModel) {
		return priceDataFactory.create(PriceDataType.BUY ,BigDecimal.valueOf(price),orderModel.getCurrency());
	}
}
