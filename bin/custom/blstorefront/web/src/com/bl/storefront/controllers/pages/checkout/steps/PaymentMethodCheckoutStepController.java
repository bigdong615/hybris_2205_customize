/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages.checkout.steps;


import static de.hybris.platform.util.localization.Localization.getLocalizedString;

import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.services.blackout.BlBlackoutDateService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.customer.BlCustomerFacade;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.ControllerConstants;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.bl.storefront.forms.GiftCardForm;
import com.bl.storefront.forms.GiftCardPurchaseForm;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.model.BrainTreePaymentInfoModel;

import de.hybris.platform.acceleratorservices.enums.CheckoutPciOptionEnum;
import de.hybris.platform.acceleratorservices.payment.constants.PaymentConstants;
import de.hybris.platform.acceleratorservices.payment.data.PaymentData;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.PreValidateCheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.PreValidateQuoteCheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.checkout.steps.CheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.checkout.steps.AbstractCheckoutStepController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.PaymentDetailsForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.SopPaymentDetailsForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.VoucherForm;
import de.hybris.platform.acceleratorstorefrontcommons.util.AddressDataUtil;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.CardTypeData;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commerceservices.enums.CountryType;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.session.SessionService;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


@Controller
@RequestMapping(value = "/checkout/multi/payment-method")
public class PaymentMethodCheckoutStepController extends AbstractCheckoutStepController
{
	protected static final Map<String, String> CYBERSOURCE_SOP_CARD_TYPES = new HashMap<>();
	private static final String PAYMENT_METHOD = "payment-method";
	private static final String CART_DATA_ATTR = "cartData";
	private static final String REDIRECT_CART_URL = REDIRECT_PREFIX + "/cart";

	private static final Logger LOGGER = Logger.getLogger(PaymentMethodCheckoutStepController.class);

	@Resource(name = "checkoutFacade")
	private BlCheckoutFacade checkoutFacade;

	@Resource(name = "addressDataUtil")
	private AddressDataUtil addressDataUtil;

	@Resource(name = "customerFacade")
	private BlCustomerFacade blCustomerFacade;

	@Resource(name = "sessionService")
	private SessionService sessionService;

	@Resource(name = "cartFacade")
	private BlCartFacade blCartFacade;
	
	@Resource(name = "blDatePickerService")
	private BlDatePickerService blDatePickerService;
	
	@Resource(name = "blBlackoutDateService")
   private BlBlackoutDateService blBlackoutDateService;
	
	@Resource(name = "cartService")
	private BlCartService blCartService;
		
	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@ModelAttribute("billingCountries")
	public Collection<CountryData> getBillingCountries()
	{
		return getCheckoutFacade().getCountries(CountryType.BILLING);
	}

	@ModelAttribute("cardTypes")
	public Collection<CardTypeData> getCardTypes()
	{
		return getCheckoutFacade().getSupportedCardTypes();
	}

	@ModelAttribute("months")
	public List<SelectOption> getMonths()
	{
		final List<SelectOption> months = new ArrayList<>();

		months.add(new SelectOption("1", "01"));
		months.add(new SelectOption("2", "02"));
		months.add(new SelectOption("3", "03"));
		months.add(new SelectOption("4", "04"));
		months.add(new SelectOption("5", "05"));
		months.add(new SelectOption("6", "06"));
		months.add(new SelectOption("7", "07"));
		months.add(new SelectOption("8", "08"));
		months.add(new SelectOption("9", "09"));
		months.add(new SelectOption("10", "10"));
		months.add(new SelectOption("11", "11"));
		months.add(new SelectOption("12", "12"));

		return months;
	}

	@ModelAttribute("startYears")
	public List<SelectOption> getStartYears()
	{
		final List<SelectOption> startYears = new ArrayList<>();
		final Calendar calender = new GregorianCalendar();

		for (int i = calender.get(Calendar.YEAR); i > calender.get(Calendar.YEAR) - 6; i--)
		{
			startYears.add(new SelectOption(String.valueOf(i), String.valueOf(i)));
		}

		return startYears;
	}

	@ModelAttribute("expiryYears")
	public List<SelectOption> getExpiryYears()
	{
		final List<SelectOption> expiryYears = new ArrayList<>();
		final Calendar calender = new GregorianCalendar();

		for (int i = calender.get(Calendar.YEAR); i < calender.get(Calendar.YEAR) + 11; i++)
		{
			expiryYears.add(new SelectOption(String.valueOf(i), String.valueOf(i)));
		}

		return expiryYears;
	}

	@ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
	private RentalDateDto getRentalsDuration()
	{
		return BlRentalDateUtils.getRentalsDuration();
	}

	@ModelAttribute(name = BlControllerConstants.HOLIDAY_DATES)
	private String getHolidayDates(){
		return BlRentalDateUtils.getHolidayDates();
	}

	@Override
	@RequestMapping(value = "/add", method = RequestMethod.GET)
	@RequireHardLogIn
	@PreValidateQuoteCheckoutStep
	@PreValidateCheckoutStep(checkoutStep = PAYMENT_METHOD)
	public String enterStep(final Model model, final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
		 if (blBlackoutDateService.checkForBlackoutDate(rentalDateDto))
		 {
			 return REDIRECT_CART_URL;
		 }
		 model.addAttribute(BlControllerConstants.ENABLE_DATE_PICKER, blCartService.isRentalCartOnly());
		sessionService.setAttribute(BlInventoryScanLoggingConstants.IS_PAYMENT_PAGE_VISITED, true);
		model.addAttribute("pageType",BlControllerConstants.BILLING_PAGE);
		model.addAttribute("paymentErrorMessage",getMessageSource().getMessage("text.payment.error.message", null,getI18nService().getCurrentLocale()));
		showMessageForRemovedGiftCard(model);
		getCheckoutFacade().setDeliveryModeIfAvailable();
		setupAddPaymentPage(model);

		model.addAttribute(BlControllerConstants.VOUCHER_FORM, new VoucherForm());
		model.addAttribute(BlControllerConstants.GIFT_CARD_FORM, new GiftCardForm());

		if(null != sessionService.getAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION) && BooleanUtils.isTrue(sessionService.getAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION))) {
			return REDIRECT_PREFIX  + BlControllerConstants.DELIVERY_METHOD_CHECKOUT_URL;
		}

		
		
		// Use the checkout PCI strategy for getting the URL for creating new subscriptions.
		final CheckoutPciOptionEnum subscriptionPciOption = getCheckoutFlowFacade().getSubscriptionPciOption();
		setCheckoutStepLinksForModel(model, getCheckoutStep());
		if (CheckoutPciOptionEnum.HOP.equals(subscriptionPciOption))
		{
			// Redirect the customer to the HOP page or show error message if it fails (e.g. no HOP configurations).
			try
			{
				final PaymentData hostedOrderPageData = getPaymentFacade().beginHopCreateSubscription("/checkout/multi/hop/response",
						"/integration/merchant_callback");
				model.addAttribute("hostedOrderPageData", hostedOrderPageData);

				final boolean hopDebugMode = getSiteConfigService().getBoolean(PaymentConstants.PaymentProperties.HOP_DEBUG_MODE,
						false);
				model.addAttribute("hopDebugMode", Boolean.valueOf(hopDebugMode));

				return ControllerConstants.Views.Pages.MultiStepCheckout.HostedOrderPostPage;
			}
			catch (final Exception e)
			{
				LOGGER.error("Failed to build beginCreateSubscription request", e);
				GlobalMessages.addErrorMessage(model, "checkout.multi.paymentMethod.addPaymentDetails.generalError");
			}
		}
		else if (CheckoutPciOptionEnum.SOP.equals(subscriptionPciOption))
		{
			//setting default card in checkout BLS-109
			if (null != getCheckoutCustomerStrategy().getCurrentUserForCheckout())
			{
				final BrainTreePaymentInfoModel defaultPaymentInfo = (BrainTreePaymentInfoModel) getCheckoutCustomerStrategy()
						.getCurrentUserForCheckout().getDefaultPaymentInfo();

				if (Objects.isNull(getCheckoutFlowFacade().getCheckoutCart().getPaymentInfo()) && null != defaultPaymentInfo)
				{
					brainTreeCheckoutFacade.setPaymentDetails(defaultPaymentInfo.getPk().toString(), defaultPaymentInfo.getNonce());
				}
			}
			
			// Build up the SOP form data and render page containing form
			final SopPaymentDetailsForm sopPaymentDetailsForm = new SopPaymentDetailsForm();
			try
			{
				setupSilentOrderPostPage(sopPaymentDetailsForm, model);
				final CartData cartData = getCheckoutFlowFacade().getCheckoutCart();
        if(Objects.nonNull(cartData) && StringUtils.isNotEmpty(cartData.getPoNumber())){
          model.addAttribute(BlControllerConstants.USER_SELECTED_PO_NUMBER, cartData.getPoNumber());
          model.addAttribute(BlControllerConstants.USER_SELECTED_PO_NOTES, cartData.getPoNotes());
        }
				if(Objects.nonNull(cartData) && Objects.nonNull(cartData.getPaymentInfo()))
				{
					final CCPaymentInfoData paymentInfo = cartData.getPaymentInfo();
					setPaymentDetailForPage(paymentInfo, model);					
				}
				// adding model attribute to disable other payment excluding Credit card if Gift card is applied
				disableOtherPayments(cartData, model);
				
				model.addAttribute(BlControllerConstants.DEFAULT_BILLING_ADDRESS, getBlCustomerFacade().getDefaultBillingAddress());
				return ControllerConstants.Views.Pages.MultiStepCheckout.SilentOrderPostPage;
			}
			catch (final Exception e)
			{
				LOGGER.error("Failed to build beginCreateSubscription request", e);
				GlobalMessages.addErrorMessage(model, "checkout.multi.paymentMethod.addPaymentDetails.generalError");
				model.addAttribute("sopPaymentDetailsForm", sopPaymentDetailsForm);
			}
		}

		// If not using HOP or SOP we need to build up the payment details form
		final PaymentDetailsForm paymentDetailsForm = new PaymentDetailsForm();
		final AddressForm addressForm = new AddressForm();
		paymentDetailsForm.setBillingAddress(addressForm);
		model.addAttribute(paymentDetailsForm);

		final CartData cartData = getCheckoutFacade().getCheckoutCart();
		model.addAttribute(CART_DATA_ATTR, cartData);

		if (Boolean.TRUE.equals(cartData.getIsRentalCart()))
		{
			model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_SUMMARY_DATE);
		}
		return ControllerConstants.Views.Pages.MultiStepCheckout.AddPaymentMethodPage;
	}

	@PostMapping(value = "/giftCardPaymentAdd")
	@RequireHardLogIn
	@PreValidateQuoteCheckoutStep
	@PreValidateCheckoutStep(checkoutStep = PAYMENT_METHOD)
	public String enterStep(final Model model, final RedirectAttributes redirectAttributes, final HttpServletRequest request, @Valid final GiftCardPurchaseForm giftCardPurchaseForm, final BindingResult bindingResult) throws CMSItemNotFoundException
	{
		if(getCheckoutFacade().updateGiftCardPurchaseForm(giftCardPurchaseForm)){
			return enterStep(model,redirectAttributes);
		}
		GlobalMessages
				.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
						"gift.card.error.message", new Object[]{StringUtils.EMPTY});
		return REDIRECT_PREFIX + "/cart";
	}
	
	/**
	 * Disable other payments excluding Credit Card.
	 *
	 * @param cart the cart
	 * @param model the model
	 */
	private void disableOtherPayments(final CartData cart, final Model model){
		model.addAttribute(BlControllerConstants.DISABLE_PAYMENT, Boolean.FALSE);
		if(Objects.nonNull(cart) && CollectionUtils.isNotEmpty(cart.getGiftCardData()))
		{
			model.addAttribute(BlControllerConstants.DISABLE_PAYMENT, Boolean.TRUE);
		}		
	}
	
	/**
	 * Sets the payment detail for payment page if already selected.
	 *
	 * @param paymentInfo the payment info
	 * @param model the model
	 */
	private void setPaymentDetailForPage(final CCPaymentInfoData paymentInfo, final Model model)
	{
		if(BlControllerConstants.CREDIT_CARD_CHECKOUT.equalsIgnoreCase(paymentInfo.getSubscriptionId()))
		{
			model.addAttribute(BlControllerConstants.USER_SELECTED_PAYMENT_INFO, paymentInfo);
			model.addAttribute(BlControllerConstants.SELECTED_PAYMENT_METHOD_NONCE, paymentInfo.getPaymentMethodNonce());
			model.addAttribute(BlControllerConstants.PAYMENT_INFO_BILLING_ADDRESS, paymentInfo.getBillingAddress());
			model.addAttribute(BlControllerConstants.IS_SAVED_CARD_ORDER, Boolean.TRUE);
		}
		else if(BlControllerConstants.PAYPAL_CHECKOUT.equalsIgnoreCase(paymentInfo.getSubscriptionId()))
		{
			model.addAttribute(BlControllerConstants.USER_SELECTED_PAYPAL_PAYMENT_INFO, paymentInfo);
		}
	}
	
	/**
	 * If gift card removed from cart then it add message to model attribute for removed gift card.
	 * @param model
	 */
	private void showMessageForRemovedGiftCard(final Model model) {
		final List<String> removedGiftCardCodeList = getCheckoutFacade().recalculateCartForGiftCard();
		if (CollectionUtils.isNotEmpty(removedGiftCardCodeList)) {
			final Locale locale = getI18nService().getCurrentLocale();
			List<String> removeGiftCardMessage = new ArrayList<>();
			for (String gcCode : removedGiftCardCodeList) {
				removeGiftCardMessage
						.add(getMessageSource().getMessage("text.gift.cart.insufficient.balance", new Object[]
								{gcCode}, locale));
			}
			model.addAttribute(BlControllerConstants.GIFT_CARD_REMOVE, removeGiftCardMessage);
		}
	}

	@RequestMapping(value =
	{ "/add" }, method = RequestMethod.POST)
	@RequireHardLogIn
	public String add(final Model model, @Valid final PaymentDetailsForm paymentDetailsForm, final BindingResult bindingResult)
			throws CMSItemNotFoundException
	{
		getPaymentDetailsValidator().validate(paymentDetailsForm, bindingResult);
		setupAddPaymentPage(model);

		final CartData cartData = getCheckoutFacade().getCheckoutCart();
		model.addAttribute(CART_DATA_ATTR, cartData);

		if (bindingResult.hasErrors())
		{
			GlobalMessages.addErrorMessage(model, "checkout.error.paymentethod.formentry.invalid");
			return ControllerConstants.Views.Pages.MultiStepCheckout.AddPaymentMethodPage;
		}

		final CCPaymentInfoData paymentInfoData = new CCPaymentInfoData();
		fillInPaymentData(paymentDetailsForm, paymentInfoData);

		final AddressData addressData;
		if (Boolean.FALSE.equals(paymentDetailsForm.getNewBillingAddress()))
		{
			addressData = getCheckoutFacade().getCheckoutCart().getDeliveryAddress();
			if (addressData == null)
			{
				GlobalMessages.addErrorMessage(model,
						"checkout.multi.paymentMethod.createSubscription.billingAddress.noneSelectedMsg");
				return ControllerConstants.Views.Pages.MultiStepCheckout.AddPaymentMethodPage;
			}

			addressData.setBillingAddress(true); // mark this as billing address
		}
		else
		{
			final AddressForm addressForm = paymentDetailsForm.getBillingAddress();
			addressData = addressDataUtil.convertToAddressData(addressForm);
			addressData.setShippingAddress(Boolean.TRUE.equals(addressForm.getShippingAddress()));
			addressData.setBillingAddress(Boolean.TRUE.equals(addressForm.getBillingAddress()));
		}

		getAddressVerificationFacade().verifyAddressData(addressData);
		paymentInfoData.setBillingAddress(addressData);

		final CCPaymentInfoData newPaymentSubscription = getCheckoutFacade().createPaymentSubscription(paymentInfoData);
		if (!checkPaymentSubscription(model, paymentDetailsForm, newPaymentSubscription))
		{
			return ControllerConstants.Views.Pages.MultiStepCheckout.AddPaymentMethodPage;
		}

		model.addAttribute("paymentId", newPaymentSubscription.getId());
		setCheckoutStepLinksForModel(model, getCheckoutStep());

		return getCheckoutStep().nextStep();
	}

	protected boolean checkPaymentSubscription(final Model model, @Valid final PaymentDetailsForm paymentDetailsForm,
			final CCPaymentInfoData newPaymentSubscription)
	{
		if (newPaymentSubscription != null && StringUtils.isNotBlank(newPaymentSubscription.getSubscriptionId()))
		{
			if (Boolean.TRUE.equals(paymentDetailsForm.getSaveInAccount()) && getUserFacade().getCCPaymentInfos(true).size() <= 1)
			{
				getUserFacade().setDefaultPaymentInfo(newPaymentSubscription);
			}
			getCheckoutFacade().setPaymentDetails(newPaymentSubscription.getId());
		}
		else
		{
			GlobalMessages.addErrorMessage(model, "checkout.multi.paymentMethod.createSubscription.failedMsg");
			return false;
		}
		return true;
	}

	protected void fillInPaymentData(@Valid final PaymentDetailsForm paymentDetailsForm, final CCPaymentInfoData paymentInfoData)
	{
		paymentInfoData.setId(paymentDetailsForm.getPaymentId());
		paymentInfoData.setCardType(paymentDetailsForm.getCardTypeCode());
		paymentInfoData.setAccountHolderName(paymentDetailsForm.getNameOnCard());
		paymentInfoData.setCardNumber(paymentDetailsForm.getCardNumber());
		paymentInfoData.setStartMonth(paymentDetailsForm.getStartMonth());
		paymentInfoData.setStartYear(paymentDetailsForm.getStartYear());
		paymentInfoData.setExpiryMonth(paymentDetailsForm.getExpiryMonth());
		paymentInfoData.setExpiryYear(paymentDetailsForm.getExpiryYear());
		if (Boolean.TRUE.equals(paymentDetailsForm.getSaveInAccount()) || getCheckoutCustomerStrategy().isAnonymousCheckout())
		{
			paymentInfoData.setSaved(true);
		}
		paymentInfoData.setIssueNumber(paymentDetailsForm.getIssueNumber());
	}

	@RequestMapping(value = "/remove", method = RequestMethod.POST)
	@RequireHardLogIn
	public String remove(@RequestParam(value = "paymentInfoId") final String paymentMethodId,
			final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		getUserFacade().unlinkCCPaymentInfo(paymentMethodId);
		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				"text.account.profile.paymentCart.removed");
		return getCheckoutStep().currentStep();
	}

	/**
	 * This method gets called when the "Use These Payment Details" button is clicked. It sets the selected payment
	 * method on the checkout facade and reloads the page highlighting the selected payment method.
	 *
	 * @param selectedPaymentMethodId
	 *           - the id of the payment method to use.
	 * @return - a URL to the page to load.
	 */
	@RequestMapping(value = "/choose", method = RequestMethod.GET)
	@RequireHardLogIn
	public String doSelectPaymentMethod(@RequestParam("selectedPaymentMethodId") final String selectedPaymentMethodId)
	{
		if (StringUtils.isNotBlank(selectedPaymentMethodId))
		{
			getCheckoutFacade().setPaymentDetails(selectedPaymentMethodId);
		}
		return getCheckoutStep().nextStep();
	}

	@RequestMapping(value = "/back", method = RequestMethod.GET)
	@RequireHardLogIn
	@Override
	public String back(final RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().previousStep();
	}

	@RequestMapping(value = "/next", method = RequestMethod.GET)
	@RequireHardLogIn
	@Override
	public String next(final RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().nextStep();
	}

	protected CardTypeData createCardTypeData(final String code, final String name)
	{
		final CardTypeData cardTypeData = new CardTypeData();
		cardTypeData.setCode(code);
		cardTypeData.setName(name);
		return cardTypeData;
	}

	protected void setupAddPaymentPage(final Model model) throws CMSItemNotFoundException
	{
		model.addAttribute("metaRobots", "noindex,nofollow");
		model.addAttribute("hasNoPaymentInfo", Boolean.valueOf(getCheckoutFlowFacade().hasNoPaymentInfo()));
		prepareDataForPage(model);
		model.addAttribute(WebConstants.BREADCRUMBS_KEY,
				getResourceBreadcrumbBuilder().getBreadcrumbs("checkout.multi.paymentMethod.breadcrumb"));
		final ContentPageModel contentPage = getContentPageForLabelOrId(MULTI_CHECKOUT_SUMMARY_CMS_PAGE_LABEL);
		storeCmsPageInModel(model, contentPage);
		setUpMetaDataForContentPage(model, contentPage);
		setCheckoutStepLinksForModel(model, getCheckoutStep());
	}

	protected void setupSilentOrderPostPage(final SopPaymentDetailsForm sopPaymentDetailsForm, final Model model)
	{
		try
		{
			final PaymentData silentOrderPageData = getPaymentFacade().beginSopCreateSubscription("/checkout/multi/sop/response",
					"/integration/merchant_callback");
			model.addAttribute("silentOrderPageData", silentOrderPageData);
			sopPaymentDetailsForm.setParameters(silentOrderPageData.getParameters());
			model.addAttribute("paymentFormUrl", silentOrderPageData.getPostUrl());
		}
		catch (final IllegalArgumentException e)
		{
			model.addAttribute("paymentFormUrl", "");
			model.addAttribute("silentOrderPageData", null);
			LOGGER.warn("Failed to set up silent order post page", e);
			GlobalMessages.addErrorMessage(model, "checkout.multi.sop.globalError");
		}

		final CartData cartData = getCheckoutFacade().getCheckoutCart();
		model.addAttribute("silentOrderPostForm", new PaymentDetailsForm());
		model.addAttribute(CART_DATA_ATTR, cartData);
		model.addAttribute("deliveryAddress", cartData.getDeliveryAddress());
		model.addAttribute("sopPaymentDetailsForm", sopPaymentDetailsForm);
		model.addAttribute("paymentInfos", getUserFacade().getCCPaymentInfos(true));
		model.addAttribute("sopCardTypes", getSopCardTypes());
		model.addAttribute("billingAddresses", getBlCustomerFacade().getAllVisibleBillingAddressesOnUser());
		if (MapUtils.isNotEmpty(sopPaymentDetailsForm.getParameters())
				&& sopPaymentDetailsForm.getParameters().containsKey(BlControllerConstants.BILL_TO_COUNTRY)
				&& StringUtils.isNotBlank(sopPaymentDetailsForm.getParameters().get(BlControllerConstants.BILL_TO_COUNTRY)))
		{
			model.addAttribute("regions",
					getI18NFacade().getRegionsForCountryIso(sopPaymentDetailsForm.getParameters().get(BlControllerConstants.BILL_TO_COUNTRY)));
			model.addAttribute("country", sopPaymentDetailsForm.getBillTo_country());
		}
		if (sessionService.getAttribute(BlCoreConstants.COUPON_APPLIED_MSG) != null)
		{
			model.addAttribute(BlCoreConstants.COUPON_APPLIED_MSG, sessionService.getAttribute(BlCoreConstants.COUPON_APPLIED_MSG));
			sessionService.removeAttribute(BlCoreConstants.COUPON_APPLIED_MSG);
		}
	}

	protected Collection<CardTypeData> getSopCardTypes()
	{
		final Collection<CardTypeData> sopCardTypes = new ArrayList<>();

		final List<CardTypeData> supportedCardTypes = getCheckoutFacade().getSupportedCardTypes();
		for (final CardTypeData supportedCardType : supportedCardTypes)
		{
			// Add credit cards for all supported cards that have mappings for cybersource SOP
			if (CYBERSOURCE_SOP_CARD_TYPES.containsKey(supportedCardType.getCode()))
			{
				sopCardTypes.add(
						createCardTypeData(CYBERSOURCE_SOP_CARD_TYPES.get(supportedCardType.getCode()), supportedCardType.getName()));
			}
		}
		return sopCardTypes;
	}

  /**
   * It saves PO payment details.
   * @param poNumber
   * @param poNotes
   * @param model
   * @param redirectAttributes
   * @return
   */
	@PostMapping(value = "/reviewSavePoPayment")
	@RequireHardLogIn
	public String savePoPaymentMethod(@RequestParam("poNumber") final String poNumber, @RequestParam("poNotes") final String poNotes,
      final Model model, final RedirectAttributes redirectAttributes){
		try {
			if(StringUtils.isNotBlank(poNumber)) {
				blCartFacade.savePoPaymentDetails(poNumber, poNotes);
			}
		} catch (final Exception exception) {
			BlLogger.logMessage(LOGGER, Level.ERROR,
					"Error occurred while setting selected PO payment details", exception);
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("braintree.billing.general.error"), null);
			return getCheckoutStep().currentStep();
		}
		return getCheckoutStep().nextStep();
	}

	protected CheckoutStep getCheckoutStep()
	{
		return getCheckoutStep(PAYMENT_METHOD);
	}

	static
	{
		// Map hybris card type to Cybersource SOP credit card
		CYBERSOURCE_SOP_CARD_TYPES.put("visa", "001");
		CYBERSOURCE_SOP_CARD_TYPES.put("master", "002");
		CYBERSOURCE_SOP_CARD_TYPES.put("amex", "003");
		CYBERSOURCE_SOP_CARD_TYPES.put("diners", "005");
		CYBERSOURCE_SOP_CARD_TYPES.put("maestro", "024");
	}

	@Override
	public BlCheckoutFacade getCheckoutFacade()
	{
		return checkoutFacade;
	}

	public void setCheckoutFacade(final BlCheckoutFacade checkoutFacade)
	{
		this.checkoutFacade = checkoutFacade;
	}

	/**
	 * @return the blCustomerFacade
	 */
	public BlCustomerFacade getBlCustomerFacade()
	{
		return blCustomerFacade;
	}

	/**
	 * @param blCustomerFacade
	 *           the blCustomerFacade to set
	 */
	public void setBlCustomerFacade(final BlCustomerFacade blCustomerFacade)
	{
		this.blCustomerFacade = blCustomerFacade;
	}
}
