package com.braintree.controllers.pages;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.core.utils.BlDateTimeUtils;
import com.bl.core.utils.BlReplaceMentOrderUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.facades.subscription.BlEmailSubscriptionFacade;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.bl.storefront.forms.GiftCardPurchaseForm;
import com.bl.storefront.security.cookie.BlRentalDateCookieGenerator;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.ControllerConstants;
import com.braintree.controllers.BraintreeaddonControllerConstants;
import com.braintree.controllers.form.BraintreePlaceOrderForm;
import com.braintree.customfield.service.CustomFieldsService;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.model.BrainTreePaymentInfoModel;
import de.hybris.platform.core.model.order.CartModel;
import com.braintree.facade.impl.BrainTreePaymentFacadeImpl;
import de.hybris.platform.acceleratorservices.enums.CheckoutPciOptionEnum;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.PreValidateCheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.checkout.steps.CheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.checkout.steps.AbstractCheckoutStepController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.PlaceOrderForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.payment.AdapterException;

import com.bl.core.services.blackout.BlBlackoutDateService;
import com.bl.core.services.cart.BlCartService;
import java.math.BigDecimal;
import java.util.*;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

@Controller
@RequestMapping(value = "checkout/multi/summary/braintree")
public class BrainTreeSummaryCheckoutStepController extends AbstractCheckoutStepController
{
	private static final Logger LOG = Logger.getLogger(BrainTreeSummaryCheckoutStepController.class);
	public static final String REDIRECT_PREFIX = "redirect:";
	public static final String CREDIT_CARD_CHECKOUT = "CreditCard";
	public static final String REVIEW_SUMMARY_PAGE = "reviewSummaryPage";
	private static final String REDIRECT_CART_URL = REDIRECT_PREFIX + "/cart";
	private static final String MULTI_CHECKOUT_REVIEW_CMS_PAGE_LABEL = "multiStepCheckoutReviewPage";

	@Resource(name = "brainTreePaymentFacadeImpl")
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;

	private static final String SUMMARY = "summary";

	@Resource(name = "customFieldsService")
	private CustomFieldsService customFieldsService;

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "brainTreeConfigService")
	private BrainTreeConfigService brainTreeConfigService;

	@Resource(name = "checkoutFacade")
	private BlCheckoutFacade blCheckoutFacade;
	
	@Resource(name = "blDatePickerService")
	private BlDatePickerService blDatePickerService;

	@Resource(name = "blRentalDateCookieGenerator")
	private BlRentalDateCookieGenerator blRentalDateCookieGenerator;

	@Resource(name = "blEmailSubscriptionFacade")
	private BlEmailSubscriptionFacade blEmailSubscriptionFacade;

	@Resource(name = "cartService")
	private BlCartService blCartService;
	
	@Resource(name = "blBlackoutDateService")
  private BlBlackoutDateService blBlackoutDateService;
	
	@ModelAttribute(name = BraintreeaddonControllerConstants.RENTAL_DATE)
	private RentalDateDto getRentalsDuration()
	{
		return BlRentalDateUtils.getRentalsDuration();
	}

	@GetMapping(value = "/view")
	@RequireHardLogIn
	@Override
	@PreValidateCheckoutStep(checkoutStep = SUMMARY)
	public String enterStep(final Model model, final RedirectAttributes redirectAttributes)
			throws CMSItemNotFoundException, CommerceCartModificationException
	{
	  final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
    if (blBlackoutDateService.checkForBlackoutDate(rentalDateDto))
    {
      return REDIRECT_CART_URL;
    }
		final boolean isCustomerHasUnPaidBillOrders =  brainTreeCheckoutFacade.isCustomerHasUnPaidBillOrders();
		model.addAttribute("isCustomerHasUnPaidBillOrders", isCustomerHasUnPaidBillOrders);
		model.addAttribute("pageType", REVIEW_SUMMARY_PAGE);
		final List<String> removedGiftCardCodeList = blCheckoutFacade.recalculateCartForGiftCard();
		if(CollectionUtils.isNotEmpty(removedGiftCardCodeList)) {
			return redirectToPaymentPageOnGiftCardRemove(redirectAttributes, removedGiftCardCodeList);
		}
		final CartData cartData = getCheckoutFlowFacade().getCheckoutCart();
    addCartDataInModel(cartData, model);
    setFormattedRentalDates(model);
    model.addAttribute(BraintreeaddonControllerConstants.CURRENT_PAGE, BraintreeaddonControllerConstants.REVIEW_PAGE);
    model.addAttribute("shipsFromPostalCode", "");

		// Only request the security code if the SubscriptionPciOption is set to Default.
		final boolean requestSecurityCode = (CheckoutPciOptionEnum.DEFAULT
				.equals(getCheckoutFlowFacade().getSubscriptionPciOption()));
		model.addAttribute("requestSecurityCode", Boolean.valueOf(requestSecurityCode));

		Map<String, String> defaultCustomFields = customFieldsService.getDefaultCustomFieldsMap();

		final BraintreePlaceOrderForm placeOrderForm = new BraintreePlaceOrderForm();
		placeOrderForm.setCustomFields(defaultCustomFields);
		model.addAttribute("placeOrderForm", placeOrderForm);

		storeCmsPageInModel(model, getContentPageForLabelOrId(MULTI_CHECKOUT_REVIEW_CMS_PAGE_LABEL));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(MULTI_CHECKOUT_REVIEW_CMS_PAGE_LABEL));
		model.addAttribute(WebConstants.BREADCRUMBS_KEY,
				getResourceBreadcrumbBuilder().getBreadcrumbs("checkout.multi.summary.breadcrumb"));
		model.addAttribute("metaRobots", "noindex,nofollow");
		setCheckoutStepLinksForModel(model, getCheckoutStep());
		return ControllerConstants.Views.Pages.MultiStepCheckout.CheckoutSummaryPage;
	}

	/**
	 * Sets the formatted rental dates on checkout summary page (Checkout Step 4).
	 *
	 * @param model the new formatted rental dates
	 */
	private void setFormattedRentalDates(final Model model)
	{
		final RentalDateDto rentalDateDto = getBlDatePickerService().getRentalDatesFromSession();
		if(Objects.nonNull(rentalDateDto))
		{
			final String formattedRentalStartDate = getFormattedDate(BlDateTimeUtils.getDate(rentalDateDto.getSelectedFromDate(),
					BraintreeaddonControllerConstants.DATE_FORMAT_PATTERN));
			model.addAttribute(BraintreeaddonControllerConstants.FORMATTED_RENTAL_START_DATE,formattedRentalStartDate);
			final String formattedRentalEndDate = getFormattedDate(BlDateTimeUtils.getDate(rentalDateDto.getSelectedToDate(),
					BraintreeaddonControllerConstants.DATE_FORMAT_PATTERN));
			model.addAttribute(BraintreeaddonControllerConstants.FORMATTED_RENTAL_END_DATE,formattedRentalEndDate);
		}
	}

	/**
	 * Gets the formatted date in EEEE, MMM d format.
	 * Example - Wednesday, Jan 31
	 *
	 * @param date the date
	 * @return the formatted date
	 */
	private String getFormattedDate(final Date date)
	{
		return BlDateTimeUtils.convertDateToStringDate(date, BraintreeaddonControllerConstants.REVIEW_PAGE_DATE_FORMAT);
	}

	/**
	 * If gift card removed from cart then it add message to model attribute for removed gift card and
	 * redirects to payment page.
	 * @param redirectAttributes
	 * @param removedGiftCardCodeList
	 */
	private String redirectToPaymentPageOnGiftCardRemove(final RedirectAttributes redirectAttributes,
			final List<String> removedGiftCardCodeList) {
		try {
			final Locale locale = getI18nService().getCurrentLocale();
			List<String> removeGiftCardMessage = new ArrayList<>();
			for (String gcCode : removedGiftCardCodeList) {
				removeGiftCardMessage
						.add(getMessageSource().getMessage("text.gift.cart.insufficient.balance", new Object[]
								{gcCode}, locale));
			}
			redirectAttributes.addFlashAttribute(BraintreeaddonControllerConstants.GIFT_CARD_REMOVE, removeGiftCardMessage);
			} catch (final Exception exception) {
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Error occurred while adding message to redirect attribute for removed gift card",
					exception);
		}
		return REDIRECT_PREFIX + BraintreeaddonControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
	}

	@PostMapping(value = "/placeOrder")
	@RequireHardLogIn
	public String placeOrder(@ModelAttribute("placeOrderForm") final BraintreePlaceOrderForm placeOrderForm, @RequestParam(value ="orderNotes")
	final String orderNotes, final Model model,
			final HttpServletRequest request, final HttpServletResponse response, final RedirectAttributes redirectModel)
					throws CMSItemNotFoundException, InvalidCartException, CommerceCartModificationException
	{

	  final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
    if (blBlackoutDateService.checkForBlackoutDate(rentalDateDto))
    {
      return REDIRECT_CART_URL;
    }
		updateGiftCardPurchaseForm(request);
		final CartModel cartModel = blCartService.getSessionCart();
		double priceBeforeRecalculateGiftCard = cartModel.getTotalPrice();
		final List<String> removedGiftCardCodeList = blCheckoutFacade.recalculateCartForGiftCard();
		double priceAfterRecalculateGiftCard = cartModel.getTotalPrice();
		blCheckoutFacade.saveOrderNotes(orderNotes);
		final boolean submitForSettlement =true;
		if (CollectionUtils.isNotEmpty(removedGiftCardCodeList)) {
			return redirectToPaymentPageOnGiftCardRemove(redirectModel, removedGiftCardCodeList);
		}
		if(Double.compare(priceBeforeRecalculateGiftCard, priceAfterRecalculateGiftCard) < 0.0) {
			return REDIRECT_PREFIX + BraintreeaddonControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
		}

		blCheckoutFacade.updateOrderTypes();

		if (validateOrderForm(placeOrderForm, model))
		{
			return enterStep(model, redirectModel);
		}

		//Validate the cart
		if (validateCart(redirectModel))
		{

			// Invalid cart. Bounce back to the cart page.
			return REDIRECT_PREFIX + "/cart";
    }
    LOG.info("placeOrderForm.getShippingPostalCode: " + placeOrderForm.getShipsFromPostalCode());
    CCPaymentInfoData paymentInfo = getCheckoutFacade().getCheckoutCart().getPaymentInfo();
    boolean isPaymentAuthorized = false;

    if (paymentInfo != null && !BooleanUtils.toBoolean(cartModel.isGiftCardOrder()) && !BooleanUtils.toBoolean(cartModel.getIsRetailGearOrder()))
    {
      try
      {
        isPaymentAuthorized = getCheckoutFacade().authorizePayment(placeOrderForm.getSecurityCode());
      }
      catch (final AdapterException ae)
      {
        // handle a case where a wrong paymentProvider configurations on the store see getCommerceCheckoutService().getPaymentProvider()
        LOG.error(ae.getMessage(), ae);
      }
      if(!isPaymentAuthorized) {
        GlobalMessages.addErrorMessage(model, "checkout.error.authorization.failed");
        return enterStep(model, redirectModel);
      }
    }
    else if(cartModel.isGiftCardOrder()){
		final BrainTreePaymentInfoModel brainTreePaymentInfo =null;
    boolean isSuccess= blCheckoutFacade.createAuthorizationTransactionOfOrderForGiftCardPurchase(cartModel, BigDecimal.valueOf(cartModel.getTotalPrice()), submitForSettlement, brainTreePaymentInfo);
    BlLogger.logMessage(LOG, Level.DEBUG,String.valueOf(isSuccess));
    }
		final OrderData orderData;
		try
		{
			LOG.info("getCheckoutFacade: " + getCheckoutFacade());
			if(paymentInfo != null) {
				brainTreeCheckoutFacade.storeIntentToCart();
				brainTreeCheckoutFacade
						.storeCustomFieldsToCart(getMergedCustomFields(placeOrderForm.getCustomFields()));
				brainTreeCheckoutFacade
						.storeShipsFromPostalCodeToCart(placeOrderForm.getShipsFromPostalCode());
			}
			getSessionService().setAttribute("SHOPPERIP", getShopperIp(request));
			orderData = getCheckoutFacade().placeOrder();
			LOG.info("Order has been placed, number/code: " + orderData.getCode());

			subscribeEmailForNewsLetters(placeOrderForm, paymentInfo, orderData);

			blRentalDateCookieGenerator.removeCookie(response);
			blDatePickerService.removeRentalDatesFromSession();
		}
		catch (final Exception e)
		{
			LOG.error("Failed to place Order, message: " + e.getMessage(), e);
			GlobalMessages.addErrorMessage(model, "checkout.placeOrder.failed");
			return enterStep(model, redirectModel);
		}

		return redirectToOrderConfirmationPage(orderData);
	}
	
	private String getShopperIp(final HttpServletRequest request)
	{
		final String trueClient = "True-Client-IP";
		final String cfConnectingIP = "CF-Connecting-IP";
		final String forwardFor = "X-Forwarded-For";
		String shopperIP = request.getHeader(trueClient);
		String headerUsed = trueClient;

		if (shopperIP == null)
		{
			headerUsed = cfConnectingIP;
			shopperIP = request.getHeader(cfConnectingIP);
		}

		if (shopperIP == null)
		{
			headerUsed = forwardFor;
			final String xfowardedHeader = request.getHeader(forwardFor);
			// to be extra sure I don't want this to throw an exception for any reason, so I'll add a try catch
			try
			{
				if (StringUtils.isNotBlank(xfowardedHeader))
				{
					final String[] headerParts = xfowardedHeader.split(",");
					if (headerParts.length > 0)
					{
						shopperIP = request.getHeader(headerParts[0]);
					}
				}
			}
			catch (final Exception ex)
			{
				LOG.info("Could not assign shopper IP from X-Forward-For header ", ex);
			}
		}

		if (shopperIP == null)
		{
			headerUsed = "Remote Address";
			shopperIP = request.getRemoteAddr();
		}

		if (shopperIP == null)
		{
			shopperIP = "";
		}
		LOG.info("Client IP found using BrainTreeSummaryCheckoutStepController " + headerUsed);
		LOG.info("shopperIP BrainTreeSummaryCheckoutStepController " + shopperIP);

		return shopperIP;
	}

	/**
	 * It subscribes customer email id for news letters
	 * @param placeOrderForm the placeOrderForm
	 * @param paymentInfo the paymentInfo
	 * @param orderData the orderData
	 */
	private void subscribeEmailForNewsLetters(
			@ModelAttribute("placeOrderForm") final BraintreePlaceOrderForm placeOrderForm,
			final CCPaymentInfoData paymentInfo, final OrderData orderData) {

		try {
			if (paymentInfo != null && placeOrderForm.isNewsLetterSubscriptionOpted()) {
				if (StringUtils.isNotEmpty(paymentInfo.getBillingAddress().getEmail())) {
					blEmailSubscriptionFacade.subscribe(paymentInfo.getBillingAddress().getEmail());
				} else {
					blEmailSubscriptionFacade.subscribe(orderData.getUser().getUid());
				}
			}
		} catch (Exception ex) {
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Error occurred while subscribing the email in ESP", ex);
		}
	}

 	/**
	 * It saves Gift Card Purchase Form
	 * @param HttpServletRequest the request
	 */
	private void updateGiftCardPurchaseForm(final HttpServletRequest request) {
		final GiftCardPurchaseForm giftCardPurchaseForm = new GiftCardPurchaseForm();
		giftCardPurchaseForm.setName(StringUtils.stripToEmpty(request.getParameter(BraintreeaddonControllerConstants.GIFTCARDPURCHASENAME)));
		giftCardPurchaseForm.setEmail(StringUtils.stripToEmpty(request.getParameter(BraintreeaddonControllerConstants.GIFTCARDPURCHASEEMAIL)));
		giftCardPurchaseForm.setMessage(StringUtils.stripToEmpty(request.getParameter(BraintreeaddonControllerConstants.GIFTCARDPURCHASEMESSAGE)));
		blCheckoutFacade.updateGiftCardPurchaseForm(giftCardPurchaseForm);
	}


	private Map<String, String> getMergedCustomFields (Map<String, String> customFieldsFromUI)
	{
		Map<String, String> customFields = customFieldsService.getDefaultCustomFieldsMap();
		customFields.putAll(customFieldsFromUI);
		BlLogger.logFormatMessageInfo(LOG,Level.INFO,"Custom Fields - {} ",customFields.toString());
		return customFields;
	}

	protected boolean validateOrderForm(final PlaceOrderForm placeOrderForm, final Model model)
	{
		final String securityCode = placeOrderForm.getSecurityCode();
		boolean invalid = false;
		if (BooleanUtils.isTrue(getCheckoutFlowFacade().getCheckoutCart().getHasGiftCart()))
		{
			return invalid;
		}
		
		if (getCheckoutFlowFacade().hasNoDeliveryAddress())
		{
			GlobalMessages.addErrorMessage(model, "checkout.deliveryAddress.notSelected");
			invalid = true;
		}

		if (getCheckoutFlowFacade().hasNoDeliveryMode())
		{
			GlobalMessages.addErrorMessage(model, "checkout.deliveryMethod.notSelected");
			invalid = true;
		}

		if (StringUtils.isEmpty(getCheckoutFlowFacade().getCheckoutCart().getPoNumber()) && getCheckoutFlowFacade().hasNoPaymentInfo())
		{
			GlobalMessages.addErrorMessage(model, "checkout.paymentMethod.notSelected");
			invalid = true;
		}
		else
		{
			// Only require the Security Code to be entered on the summary page if the SubscriptionPciOption is set to Default.
			if (CheckoutPciOptionEnum.DEFAULT.equals(getCheckoutFlowFacade().getSubscriptionPciOption())
					&& StringUtils.isBlank(securityCode))
			{
				GlobalMessages.addErrorMessage(model, "checkout.paymentMethod.noSecurityCode");
				invalid = true;
			}
		}
		
		final CartData cartData = getCheckoutFacade().getCheckoutCart();

		if (!getCheckoutFacade().containsTaxValues())
		{
			LOG.error(String.format(
					"Cart %s does not have any tax values, which means the tax cacluation was not properly done, placement of order can't continue",
					cartData.getCode()));
			GlobalMessages.addErrorMessage(model, "checkout.error.tax.missing");
			invalid = true;
		}

		if (!cartData.isCalculated())
		{
			LOG.error(
					String.format("Cart %s has a calculated flag of FALSE, placement of order can't continue", cartData.getCode()));
			GlobalMessages.addErrorMessage(model, "checkout.error.cart.notcalculated");
			invalid = true;
		}

		return invalid;
	}

	protected CheckoutStep getCheckoutStep()
	{
		return getCheckoutStep(SUMMARY);
	}

	@GetMapping(value = "/back")
	@RequireHardLogIn
	@Override
	public String back(final RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().previousStep();
	}

	@GetMapping(value = "/next")
	@RequireHardLogIn
	@Override
	public String next(final RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().nextStep();
	}
	
	@GetMapping(value = "/reviewPrint")
  	public String print(final HttpServletRequest request, final Model model) {
		try {
			final CartData cartData = getCheckoutFlowFacade().getCheckoutCart();
			blCheckoutFacade.getModifiedTotalForPrintQuote(cartData);
			addCartDataInModel(cartData, model);
			setFormattedRentalDates(model);
			model.addAttribute(BraintreeaddonControllerConstants.FROM_PAGE_STATUS, BraintreeaddonControllerConstants.REVIEW_PAGE);
			return ControllerConstants.Views.Pages.MultiStepCheckout.ReviewPrint;
		}
		catch(final Exception exception) {
			BlLogger.logMessage(LOG, Level.ERROR, "Error while creating data for Print Page from Review page", exception);
		}
		return getCheckoutStep().currentStep();
  	}
	
	/**
	 * Adds the cart data in model.
	 *
	 * @param cartData the cart data
	 * @param model the model
	 */
	private void addCartDataInModel(final CartData cartData, final Model model) {
		if(Objects.nonNull(cartData)) {
			/*if (Objects.nonNull(cartData.getEntries()) && CollectionUtils.isNotEmpty(cartData.getEntries())) {
			  cartData.getEntries().forEach(cartEntry -> {
			  	final String productCode = cartEntry.getProduct().getCode();
			  	final ProductData product = getProductFacade().getProductForCodeAndOptions(productCode,
              		Arrays.asList(ProductOption.BASIC, ProductOption.PRICE));
			  	cartEntry.setProduct(product);
			  });
			}*/
			model.addAttribute(BraintreeaddonControllerConstants.CART_DATA, cartData);
			model.addAttribute(BraintreeaddonControllerConstants.ALL_ITEMS, cartData.getEntries());
			model.addAttribute(BraintreeaddonControllerConstants.DELIVERY_ADDRESS, cartData.getDeliveryAddress());
			model.addAttribute(BraintreeaddonControllerConstants.DELIVERY_MODE, cartData.getDeliveryMode());
			model.addAttribute(BraintreeaddonControllerConstants.PAYMENT_INFO, cartData.getPaymentInfo());
		}
	}


	@PostMapping(value = "/placeReplacementOrder")
	@RequireHardLogIn
	public String placeReplacementOrder(@ModelAttribute("placeOrderForm") final BraintreePlaceOrderForm placeOrderForm , final Model model,
			final HttpServletRequest request, final HttpServletResponse response, final RedirectAttributes redirectModel) {
		final CartModel cartModel = blCartService.getSessionCart();
		final OrderData orderData;
		try
		{
			BlReplaceMentOrderUtils.setIsCartUsedForReplacementOrder(cartModel);
			orderData = getCheckoutFacade().placeOrder();
			BlLogger.logMessage(LOG , Level.INFO , "Replacement Order has been placed, number/code: " +
					orderData.getCode() + "for -> original order number" + cartModel.getReturnRequestForOrder().getOrder().getCode());

			blRentalDateCookieGenerator.removeCookie(response);
			blDatePickerService.removeRentalDatesFromSession();
		}
		catch (final Exception e)
		{
			LOG.error("Failed to place Order, message: " + e.getMessage(), e);
			GlobalMessages.addErrorMessage(model, "checkout.placeOrder.failed");
			return REDIRECT_PREFIX + BlControllerConstants.DELIVERY_METHOD_CHECKOUT_URL;
		}

		if(null != getSessionService().getAttribute(BlCoreConstants.RETURN_REQUEST)) {
			getSessionService().removeAttribute(BlCoreConstants.RETURN_REQUEST);
		}

		return redirectToOrderConfirmationPage(orderData);
	}


	public CustomFieldsService getCustomFieldsService() {
		return customFieldsService;
	}

	public void setCustomFieldsService(CustomFieldsService customFieldsService) {
		this.customFieldsService = customFieldsService;
	}

	public BrainTreeCheckoutFacade getBrainTreeCheckoutFacade() {
		return brainTreeCheckoutFacade;
	}

	public void setBrainTreeCheckoutFacade(BrainTreeCheckoutFacade brainTreeCheckoutFacade) {
		this.brainTreeCheckoutFacade = brainTreeCheckoutFacade;
	}

	public BrainTreeConfigService getBrainTreeConfigService() {
		return brainTreeConfigService;
	}

	public void setBrainTreeConfigService(BrainTreeConfigService brainTreeConfigService) {
		this.brainTreeConfigService = brainTreeConfigService;
	}
	
	/**
   * @return the blDatePickerService
   */
  public BlDatePickerService getBlDatePickerService()
  {
    return blDatePickerService;
  }

  /**
   * @param blDatePickerService the blDatePickerService to set
   */
  public void setBlDatePickerService(BlDatePickerService blDatePickerService)
  {
    this.blDatePickerService = blDatePickerService;
  }
}
