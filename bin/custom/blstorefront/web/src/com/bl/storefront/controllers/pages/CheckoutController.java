/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages;

import static de.hybris.platform.commercefacades.constants.CommerceFacadesConstants.CONSENT_GIVEN;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.GiftCardModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.services.gitfcard.BlGiftCardService;
import com.bl.facades.giftcard.BlGiftCardFacade;
import com.bl.facades.giftcard.data.BLGiftCardData;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.ControllerConstants;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.order.calculation.exception.CalculationException;
import de.hybris.platform.acceleratorfacades.flow.impl.SessionOverrideCheckoutFlowFacade;
import de.hybris.platform.acceleratorservices.controllers.page.PageType;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractCheckoutController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.ConsentForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.GuestRegisterForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.validation.GuestRegisterValidator;
import de.hybris.platform.acceleratorstorefrontcommons.security.AutoLoginStrategy;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.consent.ConsentFacade;
import de.hybris.platform.commercefacades.consent.CustomerConsentDataStrategy;
import de.hybris.platform.commercefacades.consent.data.AnonymousConsentData;
import de.hybris.platform.commercefacades.coupon.data.CouponData;
import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.ProductFacade;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commercefacades.voucher.VoucherFacade;
import de.hybris.platform.commercefacades.voucher.exceptions.VoucherOperationException;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.commerceservices.order.CommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.commerceservices.util.ResponsiveUtils;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.couponservices.model.AbstractCouponModel;
import de.hybris.platform.couponservices.services.CouponService;
import de.hybris.platform.servicelayer.exceptions.ModelNotFoundException;
import de.hybris.platform.servicelayer.exceptions.UnknownIdentifierException;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.servicelayer.user.UserService;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import org.springframework.web.util.WebUtils;


/**
 * CheckoutController
 */
@Controller
@RequestMapping(value = "/checkout")
public class CheckoutController extends AbstractCheckoutController {

  private static final Logger LOG = Logger.getLogger(CheckoutController.class);
  /**
   * We use this suffix pattern because of an issue with Spring 3.1 where a Uri value is incorrectly
   * extracted if it contains on or more '.' characters. Please see https://jira.springsource.org/browse/SPR-6164
   * for a discussion on the issue and future resolution.
   */
  private static final String ORDER_CODE_PATH_VARIABLE_PATTERN = "{orderCode:.*}";

  private static final String CHECKOUT_ORDER_CONFIRMATION_CMS_PAGE_LABEL = "orderConfirmation";
  private static final String CONTINUE_URL_KEY = "continueUrl";
  private static final String CONSENT_FORM_GLOBAL_ERROR = "consent.form.global.error";

  @Resource(name = "productFacade")
  private ProductFacade productFacade;

  @Resource(name = "orderFacade")
  private OrderFacade orderFacade;

  @Resource(name = "checkoutFacade")
  private CheckoutFacade checkoutFacade;

  @Resource(name = "guestRegisterValidator")
  private GuestRegisterValidator guestRegisterValidator;

  @Resource(name = "autoLoginStrategy")
  private AutoLoginStrategy autoLoginStrategy;

  @Resource(name = "consentFacade")
  protected ConsentFacade consentFacade;

  @Resource(name = "customerConsentDataStrategy")
  protected CustomerConsentDataStrategy customerConsentDataStrategy;

  @Resource(name = "cartFacade")
  private CartFacade cartFacade;

  @Resource(name = "voucherFacade")
  private VoucherFacade voucherFacade;

  @Resource(name = "giftCardService")
  private BlGiftCardService giftCardService;

  @Resource(name = "sessionService")
  private SessionService sessionService;

  @Resource(name = "userService")
  private UserService userService;

  @Resource(name = "couponService")
  private CouponService couponService;

  @Resource(name = "cartService")
  private BlCartService blCartService;

  @Resource(name = "checkoutCartCalculationStrategy")
  private CommerceCartCalculationStrategy commerceCartCalculationStrategy;

  @Resource(name = "blGiftCardFacade")
  private BlGiftCardFacade blGiftCardFacade;

  @ExceptionHandler(ModelNotFoundException.class)
  public String handleModelNotFoundException(final ModelNotFoundException exception,
      final HttpServletRequest request) {
    request.setAttribute("message", exception.getMessage());
    return FORWARD_PREFIX + "/404";
  }

  @GetMapping
  public String checkout(final RedirectAttributes redirectModel) {
    if (getCheckoutFlowFacade().hasValidCart()) {
      if (validateCart(redirectModel)) {
        return REDIRECT_PREFIX + "/cart";
      } else {
        checkoutFacade.prepareCartForCheckout();
        return getCheckoutRedirectUrl();
      }
    }

    LOG.info("Missing, empty or unsupported cart");

    // No session cart or empty session cart. Bounce back to the cart page.
    return REDIRECT_PREFIX + "/cart";
  }

  @GetMapping(value = "/orderConfirmation/"
      + ORDER_CODE_PATH_VARIABLE_PATTERN)
  @RequireHardLogIn
  public String orderConfirmation(@PathVariable("orderCode") final String orderCode,
      final HttpServletRequest request,
      final Model model, final RedirectAttributes redirectModel) throws CMSItemNotFoundException {
    SessionOverrideCheckoutFlowFacade.resetSessionOverrides();
    return processOrderCode(orderCode, model, request, redirectModel);
  }


  @PostMapping(value = "/orderConfirmation/"
      + ORDER_CODE_PATH_VARIABLE_PATTERN)
  public String orderConfirmation(final GuestRegisterForm form, final BindingResult bindingResult,
      final Model model,
      final HttpServletRequest request, final HttpServletResponse response,
      final RedirectAttributes redirectModel)
      throws CMSItemNotFoundException {
    getGuestRegisterValidator().validate(form, bindingResult);
    return processRegisterGuestUserRequest(form, bindingResult, model, request, response,
        redirectModel);
  }

  protected String processRegisterGuestUserRequest(final GuestRegisterForm form,
      final BindingResult bindingResult,
      final Model model, final HttpServletRequest request, final HttpServletResponse response,
      final RedirectAttributes redirectModel) throws CMSItemNotFoundException {
    if (bindingResult.hasErrors()) {
      form.setTermsCheck(false);
      GlobalMessages.addErrorMessage(model, "form.global.error");
      return processOrderCode(form.getOrderCode(), model, request, redirectModel);
    }
    try {
      getCustomerFacade().changeGuestToCustomer(form.getPwd(), form.getOrderCode());
      getAutoLoginStrategy()
          .login(getCustomerFacade().getCurrentCustomer().getUid(), form.getPwd(), request,
              response);
      getSessionService().removeAttribute(WebConstants.ANONYMOUS_CHECKOUT);
    } catch (final DuplicateUidException e) {
      // User already exists
      LOG.debug("guest registration failed.");
      form.setTermsCheck(false);
      model.addAttribute(new GuestRegisterForm());
      GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER,
          "guest.checkout.existingaccount.register.error", new Object[]
              {form.getUid()});
      return REDIRECT_PREFIX + request.getHeader("Referer");
    }

    // Consent form data
    try {
      final ConsentForm consentForm = form.getConsentForm();
      if (consentForm != null && consentForm.getConsentGiven()) {
        getConsentFacade().giveConsent(consentForm.getConsentTemplateId(),
            consentForm.getConsentTemplateVersion());
      }
    } catch (final Exception e) {
      LOG.error("Error occurred while creating consents during registration", e);
      GlobalMessages.addFlashMessage(redirectModel, GlobalMessages.ERROR_MESSAGES_HOLDER,
          CONSENT_FORM_GLOBAL_ERROR);
    }

    // save anonymous-consent cookies as ConsentData
    final Cookie cookie = WebUtils.getCookie(request, WebConstants.ANONYMOUS_CONSENT_COOKIE);
    if (cookie != null) {
      try {
        final ObjectMapper mapper = new ObjectMapper();
        final List<AnonymousConsentData> anonymousConsentDataList = Arrays.asList(mapper.readValue(
            URLDecoder.decode(cookie.getValue(), StandardCharsets.UTF_8.displayName()),
            AnonymousConsentData[].class));
        anonymousConsentDataList.stream()
            .filter(consentData -> CONSENT_GIVEN.equals(consentData.getConsentState()))
            .forEach(consentData -> consentFacade.giveConsent(consentData.getTemplateCode(),
                Integer.valueOf(consentData.getTemplateVersion())));
      } catch (final UnsupportedEncodingException e) {
        LOG.error(String.format("Cookie Data could not be decoded : %s", cookie.getValue()), e);
      } catch (final IOException e) {
        LOG.error("Cookie Data could not be mapped into the Object", e);
      } catch (final Exception e) {
        LOG.error("Error occurred while creating Anonymous cookie consents", e);
      }
    }

    customerConsentDataStrategy.populateCustomerConsentDataInSession();

    return REDIRECT_PREFIX + "/";
  }

  protected String processOrderCode(final String orderCode, final Model model,
      final HttpServletRequest request,
      final RedirectAttributes redirectModel) throws CMSItemNotFoundException {
    final OrderData orderDetails;

    try {
      orderDetails = orderFacade.getOrderDetailsForCode(orderCode);
    } catch (final UnknownIdentifierException e) {
      LOG.warn(
          "Attempted to load an order confirmation that does not exist or is not visible. Redirect to home page.");
      return REDIRECT_PREFIX + ROOT;
    }

    addRegistrationConsentDataToModel(model);

    if (orderDetails.isGuestCustomer() && !StringUtils
        .substringBefore(orderDetails.getUser().getUid(), "|")
        .equals(getSessionService().getAttribute(WebConstants.ANONYMOUS_CHECKOUT_GUID))) {
      return getCheckoutRedirectUrl();
    }

    if (orderDetails.getEntries() != null && !orderDetails.getEntries().isEmpty()) {
      for (final OrderEntryData entry : orderDetails.getEntries()) {
        final String productCode = entry.getProduct().getCode();
        final ProductData product = productFacade.getProductForCodeAndOptions(productCode,
            Arrays.asList(ProductOption.BASIC, ProductOption.PRICE, ProductOption.CATEGORIES));
        entry.setProduct(product);
      }
    }

    model.addAttribute("orderCode", orderCode);
    model.addAttribute("orderData", orderDetails);
    model.addAttribute("allItems", orderDetails.getEntries());
    model.addAttribute("deliveryAddress", orderDetails.getDeliveryAddress());
    model.addAttribute("deliveryMode", orderDetails.getDeliveryMode());
    model.addAttribute("paymentInfo", orderDetails.getPaymentInfo());
    model.addAttribute("pageType", PageType.ORDERCONFIRMATION.name());

    final List<CouponData> giftCoupons = orderDetails.getAppliedOrderPromotions().stream()
        .filter(x -> CollectionUtils.isNotEmpty(x.getGiveAwayCouponCodes()))
        .flatMap(p -> p.getGiveAwayCouponCodes().stream())
        .collect(Collectors.toList());
    model.addAttribute("giftCoupons", giftCoupons);

    processEmailAddress(model, orderDetails);

    final String continueUrl = (String) getSessionService().getAttribute(WebConstants.CONTINUE_URL);
    model.addAttribute(CONTINUE_URL_KEY,
        (continueUrl != null && !continueUrl.isEmpty()) ? continueUrl : ROOT);

    final ContentPageModel checkoutOrderConfirmationPage = getContentPageForLabelOrId(
        CHECKOUT_ORDER_CONFIRMATION_CMS_PAGE_LABEL);
    storeCmsPageInModel(model, checkoutOrderConfirmationPage);
    setUpMetaDataForContentPage(model, checkoutOrderConfirmationPage);
    model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS,
        ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);

    if (ResponsiveUtils.isResponsive()) {
      return getViewForPage(model);
    }

    return ControllerConstants.Views.Pages.Checkout.CheckoutConfirmationPage;
  }

  protected void processEmailAddress(final Model model, final OrderData orderDetails) {
    final String uid;

    if (orderDetails.isGuestCustomer() && !model.containsAttribute("guestRegisterForm")) {
      final GuestRegisterForm guestRegisterForm = new GuestRegisterForm();
      guestRegisterForm.setOrderCode(orderDetails.getGuid());
      uid = orderDetails.getPaymentInfo().getBillingAddress().getEmail();
      guestRegisterForm.setUid(uid);
      model.addAttribute(guestRegisterForm);
    } else {
      uid = orderDetails.getUser().getUid();
    }
    model.addAttribute("email", uid);
  }

  /**
   * This method is used to apply gift card.
   *
   * @param code
   * @param request
   * @param model
   * @return
   * @throws CMSItemNotFoundException
   * @throws CalculationException
   */
  @PostMapping(value = "/apply")
  @ResponseBody
  public String apply(final String code, final HttpServletRequest request, final Model model)
      throws VoucherOperationException {
    boolean isReapplyGiftCard = false;
    final CartModel cartModel = blCartService.getSessionCart();
    final GiftCardModel giftCardModel = blGiftCardFacade.getGiftCard(code);
    final Locale locale = getI18nService().getCurrentLocale();
    final List<BLGiftCardData> blGiftCardDataList = cartFacade.getSessionCart().getGiftCardData();

    final String codeUpperCase = code.toUpperCase();
    final Optional<AbstractCouponModel> optional = couponService.getCouponForCode(codeUpperCase);
    if (optional.isPresent()) {
      // Check if any gift card is applied
      if (blGiftCardDataList != null && !blGiftCardDataList.isEmpty()) {
        // Remove gift card
        for (BLGiftCardData giftCardData : blGiftCardDataList) {
          blGiftCardFacade.removeGiftCard(giftCardData.getCode());
          isReapplyGiftCard = true;
        }
      }
			addCoupon(locale, codeUpperCase, blGiftCardDataList, isReapplyGiftCard);
    }

    if (optional.isEmpty()) {
      final List<String> giftCardDataList = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(blGiftCardDataList)) {
        for (BLGiftCardData giftCardData : blGiftCardDataList) {
          giftCardDataList.add(giftCardData.getCode());
        }
      }
      return handleGiftCardStatus(code, locale, giftCardDataList, cartModel, giftCardModel);
    }
    return BlControllerConstants.ERROR;
  }

  /**
	 * It sets message in the session attribute based on the gift card apply operation.
   * @param code
   * @param locale
   * @param giftCardData
   * @param cartModel
   * @param giftCardModel
   * @return String.
   */
  private String handleGiftCardStatus(final String code, final Locale locale,
      final List<String> giftCardData, final CartModel cartModel,
      final GiftCardModel giftCardModel) {
    if (CollectionUtils.isNotEmpty(giftCardData) && giftCardData.contains(code)
        && isOrderFullyPaid(cartModel)) {
      //if cart already have applied GC  and order total is 0.00
      sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
          getMessageSource().getMessage("text.gift.apply.applied.again", null, locale));
      return BlControllerConstants.ERROR;
    } else if (CollectionUtils.isNotEmpty(giftCardData) && giftCardData.contains(code)) {
      //if cart already have applied GC and GC have insufficient balance
      sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
          getMessageSource().getMessage("text.gift.cart.insufficient.balance", new Object[]
              {code}, locale));
      return BlControllerConstants.ERROR;
    }

    if (blGiftCardFacade.applyGiftCard(code)) {
      sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
          getMessageSource().getMessage("text.gift.apply.success", new Object[]
              {code}, locale));
      return BlControllerConstants.SUCCESS;
    } else {
      if (giftCardModel == null) {
        //if applied GC is not present in the system.
        sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
            getMessageSource().getMessage("text.gift.apply.applied.fail", null, locale));
        return BlControllerConstants.ERROR;
      } else if (isOrderFullyPaid(cartModel)) {
        //if order total is 0.00
        sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
            getMessageSource().getMessage("text.gift.apply.applied.again", null, locale));
        return BlControllerConstants.ERROR;
      }
    }
    return BlControllerConstants.ERROR;
  }

	/**
	 * It checks that cart total price shouldn't be zero or less than zero.
	 * @param cartModel
	 * @return boolean value
	 */
  private boolean isOrderFullyPaid(final CartModel cartModel) {
    return cartModel.getTotalPrice() <= 0;
  }

	/**
	 * It applies coupon code and sets message in session attributes accordingly.
	 * @param locale
	 * @param codeUpperCase
	 * @param blGiftCardData
	 * @param isReapplyGiftCard
	 * @throws VoucherOperationException
	 */
  private void addCoupon(final Locale locale, final String codeUpperCase,
      final List<BLGiftCardData> blGiftCardData, final boolean isReapplyGiftCard)
      throws VoucherOperationException {
    // apply coupon
    try {
      voucherFacade.applyVoucher(codeUpperCase);

      sessionService.setAttribute(BlCoreConstants.COUPON_APPLIED_MSG,
          getMessageSource().getMessage("text.voucher.apply.applied.success", new Object[]
              {codeUpperCase}, locale));
    } catch (final VoucherOperationException exception) {
      if (isReapplyGiftCard && CollectionUtils.isNotEmpty(blGiftCardData)) {
        for (BLGiftCardData blGiftCardDatum : blGiftCardData) {
          blGiftCardFacade.applyGiftCard(blGiftCardDatum.getCode());
        }
      }
      if (LOG.isDebugEnabled()) {
        LOG.debug(exception.getMessage(), exception);
      }
    }

    if (CollectionUtils.isNotEmpty(blGiftCardData)) {
      for (BLGiftCardData giftData : blGiftCardData) {
        blGiftCardFacade.applyGiftCard(giftData.getCode());
      }
    }
  }

	/**
	 * It removes applied gift card/coupon code from cart.
	 * @param code
	 * @param request
	 * @param model
	 * @return
	 */
  @PostMapping(value = "/remove")
  @ResponseBody
  public String remove(final String code, final HttpServletRequest request, final Model model) {
    String status = BlControllerConstants.TRUE_STRING;
    boolean isVoucher = false;
    try {
      final String codeUpperCase = code.toUpperCase(Locale.getDefault());
      if (voucherFacade.checkVoucherCode(codeUpperCase)) {
        isVoucher = true;
        voucherFacade.releaseVoucher(codeUpperCase);
        status = BlControllerConstants.TRUE_STRING;
      } else {
        blGiftCardFacade.removeGiftCard(code);
      }
    } catch (final VoucherOperationException exception) {
    	BlLogger.logFormatMessageInfo(LOG, Level.ERROR,"Error occurred while removing gift card/voucher code {}", code, exception);
      status = BlControllerConstants.FALSE_STRING;
    } finally {
      try {
        if (isVoucher) {
          final CartModel sessionCart = blCartService.getSessionCart();
          final CommerceCartParameter calculateCartParameter = new CommerceCartParameter();
          calculateCartParameter.setEnableHooks(true);
          calculateCartParameter.setCart(sessionCart);
          calculateCartParameter.setRecalculate(true);
          commerceCartCalculationStrategy.calculateCart(calculateCartParameter);
        }
      } catch (final CalculationException calculationException) {
      	BlLogger.logFormatMessageInfo(LOG,Level.ERROR, "Error calculating cart during  gift card/coupon code {} removal.", code, calculationException);
      }
    }
    return status;
  }


  protected GuestRegisterValidator getGuestRegisterValidator() {
    return guestRegisterValidator;
  }

  protected AutoLoginStrategy getAutoLoginStrategy() {
    return autoLoginStrategy;
  }

}