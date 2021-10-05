/**
 *
 */
package com.braintree.controllers.pages;

import static com.braintree.controllers.BraintreeaddonControllerConstants.PAY_PAL_HAED_ERROR;
import static com.braintree.controllers.BraintreeaddonControllerConstants.Views.Pages.MultiStepCheckout.CheckoutOrderPageErrorPage;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;
import com.braintree.controllers.BraintreeaddonControllerConstants;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.order.BlOrderFacade;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.braintree.constants.BraintreeConstants;
import com.braintree.constants.BraintreeaddonWebConstants;
import com.braintree.controllers.handler.PayPalResponseExpressCheckoutHandler;
import com.braintree.controllers.handler.PayPalUserLoginHandler;
import com.braintree.facade.BrainTreeUserFacade;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.facade.impl.BrainTreePaymentFacadeImpl;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.hybris.data.PayPalAddressData;
import com.braintree.hybris.data.PayPalCheckoutData;
import com.braintree.hybris.data.PayPalExpressResponse;
import com.braintree.hybris.data.PayPalMiniCartResponse;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.security.PayPalGUIDCookieStrategy;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.ThirdPartyConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractCheckoutController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.servicelayer.session.SessionService;
import java.io.IOException;
import java.math.BigDecimal;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import de.hybris.platform.commercefacades.order.OrderFacade;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import java.math.RoundingMode;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.core.enums.OrderStatus;


@Controller
@RequestMapping(value = "/braintree/paypal/checkout")
public class PayPalPaymentController extends AbstractCheckoutController
{
	protected static final String REDIRECT_URL_CART = REDIRECT_PREFIX + BraintreeaddonWebConstants.CART_URL;
	private static final String ANONYMOUS_USER = "anonymous";
	private static final String DEVICE_DATA = "device_data";
	private static final String EXTEND_RENTAL_ORDER_CONFIRMATION = "extendRentalOrderConfirmation";
	private static final int DECIMAL_PRECISION = 2;
	private static final String MY_ACCOUNT_MODIFY_PAYMENT = "/my-account/modifyPayment/";
	private static final String REDIRECT_TO_ORDER_DETAILS_PAGE = REDIRECT_PREFIX + "/my-account/order/";
	private static final String MY_ACCOUNT = "/my-account/";
	private static final String REDIRECT_TO_MODIFIED_ORDER_PAYMENT_PAGE = REDIRECT_PREFIX + MY_ACCOUNT;
	private static final String REDIRECT_TO_DEPOSIT_ORDER_PAYMENT_PAGE = REDIRECT_PREFIX + MY_ACCOUNT;
	private static final String MODIFIED_ORDER_PAYMET_PATH = "/modifiedOrderPayment";
	private static final String DEPOSIT_ORDER_PAYMET_PATH = "/depositPayment";


	private static final Logger LOG = Logger.getLogger(PayPalPaymentController.class);

	@Resource(name = "payPalUserLoginHandler")
	PayPalUserLoginHandler payPalUserLoginHandler;

	@Resource(name = "payPalResponseExpressCheckoutHandler")
	PayPalResponseExpressCheckoutHandler payPalResponseExpressCheckoutHandler;

	@Resource(name = "payPalGUIDCookieStrategy")
	private PayPalGUIDCookieStrategy payPalGUIDCookieStrategy;

	@Resource(name = "brainTreePaymentFacadeImpl")
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;

	@Resource(name = "brainTreeUserFacade")
	private BrainTreeUserFacade brainTreeUserFacade;

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "multiStepCheckoutBreadcrumbBuilder")
	private ResourceBreadcrumbBuilder resourceBreadcrumbBuilder;

	@Resource
	private SessionService sessionService;

	@Resource
	private CartService cartService;

	@Resource(name = "cartFacade")
	private BlCartFacade blCartFacade;

	@Resource(name = "blOrderFacade")
	private BlOrderFacade blOrderFacade;

	@Resource
	private BrainTreeTransactionService brainTreeTransactionService;

	
	@Resource(name = "orderFacade")
	private OrderFacade orderFacade;

	@Resource(name = "priceDataFactory")
	private PriceDataFactory priceDataFactory;

	@Resource(name = "modelService")
	private ModelService modelService;

	@PostMapping(value = "/express")
	public String doHandleHopResponse(final Model model, final RedirectAttributes redirectAttributes,
                                      final HttpServletRequest request, final HttpServletResponse response) throws CMSItemNotFoundException
	{

		PayPalExpressResponse payPalExpressResponse = null;
		boolean isSavePaymentInfo = false;
		String deviceData = null;

		try
		{
			payPalExpressResponse = payPalResponseExpressCheckoutHandler.handlePayPalResponse(request);
      		isSavePaymentInfo = true;
      		deviceData = request.getParameter(DEVICE_DATA);
		}
		catch (final IllegalArgumentException exception)
		{
		  BlLogger.logMessage(LOG, Level.ERROR, "Error occured while handling paypal response", exception);
		  addPayPalErrorMessage(BlControllerConstants.PAYPAL_ERROR_MESSAGE_KEY, redirectAttributes);
		  return REDIRECT_PREFIX + BlControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
		}
        final DeliveryModeModel selectedDeliveryMode = cartService.getSessionCart().getDeliveryMode();
		final String payPalEmail = payPalExpressResponse.getDetails().getEmail();
		if (ANONYMOUS_USER.equals(getSessionCartUserUid()))
		{
			try
			{
				if (StringUtils.isEmpty(payPalEmail))
				{
				  BlLogger.logMessage(LOG, Level.ERROR, "Pal pal email is empty!");
				  addPayPalErrorMessage(BlControllerConstants.PAYPAL_ERROR_MESSAGE_KEY, redirectAttributes);
				  return REDIRECT_PREFIX + BlControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
				}
				final String name = payPalExpressResponse.getDetails().getFirstName() != null ? payPalExpressResponse.getDetails()
						.getFirstName() : BraintreeConstants.PAYPAL_PAYMENT;
				getCustomerFacade().createGuestUserForAnonymousCheckout(payPalEmail, name);
				payPalGUIDCookieStrategy.setCookie(request, response);
				sessionService.setAttribute(WebConstants.ANONYMOUS_CHECKOUT_GUID,
						StringUtils.substringBefore(getSessionCartUserUid(), "|"));
				cartService.getSessionCart().setDeliveryMode(selectedDeliveryMode);
				cartService.saveOrder(cartService.getSessionCart());

			}
			catch (final DuplicateUidException exception)
			{
			  BlLogger.logMessage(LOG, Level.ERROR, "Paypal new registration failed", exception);
			  addPayPalErrorMessage(BlControllerConstants.PAYPAL_ERROR_MESSAGE_KEY, redirectAttributes);
			  return REDIRECT_PREFIX + BlControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
			}
			payPalUserLoginHandler.isHardLogin(model);
		}

		String paymentProvider = BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT;

		if(payPalExpressResponse.getType().equals(BraintreeConstants.APPLE_PAY_CARD))
		{
			paymentProvider = BraintreeConstants.APPLE_PAY_CARD;
		}
		else if (payPalExpressResponse.getType().equals(BraintreeConstants.ANDROID_PAY_CARD))
		{
			paymentProvider = BraintreeConstants.ANDROID_PAY_CARD;
		}

		final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo(payPalExpressResponse.getNonce(),
				paymentProvider,  isSavePaymentInfo, Boolean.FALSE, payPalEmail);

		final PayPalAddressData payPalShippingAddress = payPalExpressResponse.getDetails().getShippingAddress();
		AddressData hybrisShippingAddress = null;
		if (payPalShippingAddress != null && brainTreeCheckoutFacade.getCheckoutCart().getDeliveryAddress() == null)
		{
			hybrisShippingAddress = payPalResponseExpressCheckoutHandler.getPayPalAddress(payPalExpressResponse.getDetails(),
					payPalShippingAddress);
			brainTreeUserFacade.addAddress(hybrisShippingAddress);
			brainTreeCheckoutFacade.setDeliveryAddress(hybrisShippingAddress);
		}
		else
		{
			if (brainTreeCheckoutFacade.getCheckoutCart().getDeliveryAddress() == null
					&& brainTreeCheckoutFacade.getCheckoutCart().getPickupOrderGroups().isEmpty())
			{
			  BlLogger.logMessage(LOG, Level.ERROR, "Shipping address from pay pal is empty!");
			  addPayPalErrorMessage(BlControllerConstants.PAYPAL_ERROR_MESSAGE_KEY, redirectAttributes);
			  return REDIRECT_PREFIX + BlControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
			}
		}

		if (deviceData != null){
			subscriptionInfo.setDeviceData(deviceData);
		}

		final PayPalAddressData payPalBillingAddress = payPalExpressResponse.getDetails().getBillingAddress();
		if (payPalBillingAddress != null)
		{
			final AddressData hybrisBillingAddress = payPalResponseExpressCheckoutHandler.getPayPalAddress(
					payPalExpressResponse.getDetails(), payPalBillingAddress);
			hybrisBillingAddress.setEmail(payPalExpressResponse.getDetails().getEmail());
			subscriptionInfo.setAddressData(hybrisBillingAddress);
		}
		else
		{
			LOG.warn("No billing address provide by Pay Pal. Use billing address as shipping...");
			subscriptionInfo.setAddressData(hybrisShippingAddress);
		}

		if (selectedDeliveryMode == null){
			brainTreeCheckoutFacade.setCheapestDeliveryModeForCheckout();
			LOG.info("Set default cheapest delivery mode for PayPal simple flow");
		}

		if (cartService.getSessionCart().getDeliveryMode() == null){
			brainTreeCheckoutFacade.setDeliveryAddress(null);
		}

		try
		{
			brainTreePaymentFacade.completeCreateSubscription(subscriptionInfo);
		}
		catch (final Exception exception)
		{
		  BlLogger.logMessage(LOG, Level.ERROR, "Error occured while creating paypal payment subscription", exception);
		  addPayPalErrorMessage(BlControllerConstants.PAYPAL_ERROR_MESSAGE_KEY, redirectAttributes);
		  return REDIRECT_PREFIX + BlControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
		}
		blCartFacade.removePoNumber();
		return REDIRECT_PREFIX + "/checkout/multi/summary/braintree/view";

	}

    @PostMapping(value = "/add-payment-method")
    public String addPaymentMethod(final Model model, final RedirectAttributes redirectAttributes,
			                          @RequestParam(value = "selectedAddressCode", required = false) final String selectedAddressCode,
                                   final HttpServletRequest request, final HttpServletResponse response) throws CMSItemNotFoundException {
        PayPalExpressResponse payPalExpressResponse = null;
        AddressData hybrisBillingAddress = null;
        final String orderCode = request.getParameter("order_code");
        final String payBillTotal = request.getParameter("payBillTotal");
        final boolean isDepositPaymentPage = BooleanUtils.toBoolean(request.getParameter("isDepositPaymentPage"));
        final boolean isModifyOrderPaymentPage = BooleanUtils.toBoolean(request.getParameter("isModifyOrderPaymentPage"));
        if(StringUtils.isBlank(orderCode) || StringUtils.isBlank(payBillTotal))
        {
          if(isModifyOrderPaymentPage)
          {
            BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Error while making Payment for Modified Order : {} with PayPal", orderCode);
            GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
                getLocalizedString("text.account.modified.order.payment.paypal.error.message"));
            return REDIRECT_TO_MODIFIED_ORDER_PAYMENT_PAGE + orderCode + MODIFIED_ORDER_PAYMET_PATH;
          }
          if(isDepositPaymentPage)
          {
            BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Error while making Payment for Deposit for Order : {} with PayPal", orderCode);
            GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
                getLocalizedString("text.account.deposit.order.payment.paypal.error.message"));
            return REDIRECT_TO_DEPOSIT_ORDER_PAYMENT_PAGE + orderCode + DEPOSIT_ORDER_PAYMET_PATH;
          }
        }
		double payBillAmount = Double.parseDouble(payBillTotal);
        try {
            payPalExpressResponse = payPalResponseExpressCheckoutHandler.handlePayPalResponse(request);
        } catch (final IllegalArgumentException exeption) {
            handleErrors(exeption.getMessage(), model);
            return CheckoutOrderPageErrorPage;
        }

        String payPalEmail = payPalExpressResponse.getDetails().getEmail();

        String paymentProvider = BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT;

        if (payPalExpressResponse.getType().equals(BraintreeConstants.APPLE_PAY_CARD))
        {
        		paymentProvider = BraintreeConstants.APPLE_PAY_CARD;
        }
        else if (payPalExpressResponse.getType().equals(BraintreeConstants.VENMO_CHECKOUT))
        {
			paymentProvider = BraintreeConstants.VENMO_CHECKOUT;
			payPalEmail = payPalExpressResponse.getDetails().getUsername();
			hybrisBillingAddress = brainTreeUserFacade.getAddressForCode(selectedAddressCode);
		}
		else if(payPalExpressResponse.getType().equals(BraintreeConstants.ANDROID_PAY_CARD))
		{
			paymentProvider = BraintreeConstants.ANDROID_PAY_CARD;
		}

        final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo(payPalExpressResponse.getNonce(),
				  paymentProvider, Boolean.FALSE, Boolean.TRUE, payPalEmail);

        final PayPalAddressData payPalBillingAddress = payPalExpressResponse.getDetails().getBillingAddress();
		 if (payPalBillingAddress != null)
		 {
			 hybrisBillingAddress = payPalResponseExpressCheckoutHandler.getPayPalAddress(
					 payPalExpressResponse.getDetails(), payPalBillingAddress);
			 hybrisBillingAddress.setEmail(payPalExpressResponse.getDetails().getEmail());
			 subscriptionInfo.setAddressData(hybrisBillingAddress);
		 }
		 else if (paymentProvider.equals(BraintreeConstants.VENMO_CHECKOUT))
		 {
			 subscriptionInfo.setAddressData(hybrisBillingAddress);
		 } else {
			 LOG.warn("No billing address provide by Pay Pal. Use empty billing address...");
			 hybrisBillingAddress = new AddressData();
			 hybrisBillingAddress.setEmail(payPalEmail);
			 subscriptionInfo.setAddressData(hybrisBillingAddress);
		 }
			boolean isSuccess = false;
			AbstractOrderModel order = null;
			try {
				order = brainTreeCheckoutFacade.getOrderByCode(orderCode);
				if (null != order) {
					final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentFacade
							.completeCreateSubscription(
									subscriptionInfo, (CustomerModel) order.getUser(), order, false, false, isDepositPaymentPage, payBillAmount, isModifyOrderPaymentPage);
					if (null != paymentInfo) {
						isSuccess = brainTreeTransactionService.createAuthorizationTransactionOfOrder(order,
								BigDecimal.valueOf(payBillAmount).setScale(DECIMAL_PRECISION, RoundingMode.HALF_EVEN), true, paymentInfo);
					}
				}
			} catch (final Exception exception) {
			  BlLogger.logFormattedMessage(LOG, Level.ERROR, StringUtils.EMPTY, exception,
            "Error while making Payment for Order : {} with PayPal", orderCode);
			  if (isModifyOrderPaymentPage)
        {
			    GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
              getLocalizedString("text.account.modified.order.payment.paypal.error.message"));
          return REDIRECT_TO_MODIFIED_ORDER_PAYMENT_PAGE + orderCode + MODIFIED_ORDER_PAYMET_PATH;
        }
			  if(isDepositPaymentPage)
        {
          GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
              getLocalizedString("text.account.deposit.order.payment.paypal.error.message"));
          return REDIRECT_TO_DEPOSIT_ORDER_PAYMENT_PAGE + orderCode + DEPOSIT_ORDER_PAYMET_PATH;
        }
				final String errorMessage = getLocalizedString("braintree.billing.general.error");
				handleErrors(errorMessage, model);
				return CheckoutOrderPageErrorPage;
			}
			if (isSuccess) {
			  final OrderData orderDetails = orderFacade.getOrderDetailsForCode(orderCode);
        final PriceData billPayTotal  = convertDoubleToPriceData(payBillAmount, order);
        orderDetails.setOrderTotalWithTaxForPayBill(billPayTotal);
        model.addAttribute(BraintreeaddonControllerConstants.ORDER_DATA, orderDetails);
        if (isDepositPaymentPage)
        {
          model.addAttribute(BraintreeaddonControllerConstants.DEPOSIT_AMOUNT, billPayTotal);
          model.addAttribute(BraintreeaddonControllerConstants.PAYMENT_TYPE, BraintreeaddonControllerConstants.PAY_PAL);
          final ContentPageModel depositPaymentSuccessPage = getContentPageForLabelOrId(BraintreeaddonControllerConstants.DEPOSIT_SUCCESS_CMS_PAGE);
          storeCmsPageInModel(model, depositPaymentSuccessPage);
          setUpMetaDataForContentPage(model, depositPaymentSuccessPage);
          model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
          return getViewForPage(model);
        }
        else if (isModifyOrderPaymentPage)
        {
          model.addAttribute(BraintreeaddonControllerConstants.ORDER_DATA, orderDetails);
          model.addAttribute(BraintreeaddonControllerConstants.AMOUNT, billPayTotal);
          model.addAttribute(BraintreeaddonControllerConstants.MODIFIED_ORDER_PAYMENT_METHOD, BraintreeaddonControllerConstants.PAYPAL_PAYMENT_METHOD);
          final ContentPageModel modifiedOrderPaymentSuccessPage = getContentPageForLabelOrId(BraintreeaddonControllerConstants.MODIFIED_ORDER_PAYMENT_SUCCESS_CMS_PAGE);
          storeCmsPageInModel(model, modifiedOrderPaymentSuccessPage);
          setUpMetaDataForContentPage(model, modifiedOrderPaymentSuccessPage);
          return getViewForPage(model);
        }
				brainTreeCheckoutFacade.setPayBillFlagTrue(order);
				final ContentPageModel payBillSuccessPage = getContentPageForLabelOrId(
						BraintreeaddonControllerConstants.PAY_BILL_SUCCESS_CMS_PAGE);
				storeCmsPageInModel(model, payBillSuccessPage);
				setUpMetaDataForContentPage(model, payBillSuccessPage);
				model.addAttribute(BlControllerConstants.PAYMENT_METHOD, BlControllerConstants.PAY_PAL);
				model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS,
						ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
				return getViewForPage(model);
			} else {
			  if(isModifyOrderPaymentPage)
			  {
			    BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Error while making Payment for Modified Order : {} with PayPal", orderCode);
	        GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
	            getLocalizedString("text.account.modified.order.payment.paypal.error.message"));
	        return REDIRECT_TO_MODIFIED_ORDER_PAYMENT_PAGE + orderCode + MODIFIED_ORDER_PAYMET_PATH;
			  }
			  if(isDepositPaymentPage)
        {
          BlLogger.logFormatMessageInfo(LOG, Level.ERROR, "Error while making Payment for Deposit for Order : {} with PayPal", orderCode);
          GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
              getLocalizedString("text.account.deposit.order.payment.paypal.error.message"));
          return REDIRECT_TO_DEPOSIT_ORDER_PAYMENT_PAGE + orderCode + DEPOSIT_ORDER_PAYMET_PATH;
        }
				return REDIRECT_PREFIX + "/my-account/" + orderCode + "/payBill";
			}
    }    
    
    @PostMapping(value = "/modify-payment-method")
    public String modifyPaymentMethod(final Model model, final RedirectAttributes redirectAttributes,
			                          @RequestParam(value = "selectedAddressCode", required = false) final String selectedAddressCode,
                                   final HttpServletRequest request, final HttpServletResponse response) throws CMSItemNotFoundException {
        PayPalExpressResponse payPalExpressResponse = null;
        AddressData hybrisBillingAddress = null;
        final String orderCode = request.getParameter("order_code");
        String modifiedOrderTotal = request.getParameter("modifyOrderTotal");
		double modifyOrderTotal = Double.parseDouble(modifiedOrderTotal);
        try {
            payPalExpressResponse = payPalResponseExpressCheckoutHandler.handlePayPalResponse(request);
        } catch (final IllegalArgumentException exeption) {
            handleErrors(exeption.getMessage(), model);
            return CheckoutOrderPageErrorPage;
        }

        String payPalEmail = payPalExpressResponse.getDetails().getEmail();

        String paymentProvider = BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT;

        final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo(payPalExpressResponse.getNonce(),
				  paymentProvider, Boolean.FALSE, Boolean.TRUE, payPalEmail);

        final PayPalAddressData payPalBillingAddress = payPalExpressResponse.getDetails().getBillingAddress();
		 if (payPalBillingAddress != null)
		 {
			 hybrisBillingAddress = payPalResponseExpressCheckoutHandler.getPayPalAddress(
					 payPalExpressResponse.getDetails(), payPalBillingAddress);
			 hybrisBillingAddress.setEmail(payPalExpressResponse.getDetails().getEmail());
			 subscriptionInfo.setAddressData(hybrisBillingAddress);
		 }
		 else if (paymentProvider.equals(BraintreeConstants.VENMO_CHECKOUT))
		 {
			 subscriptionInfo.setAddressData(hybrisBillingAddress);
		 } else {
			 LOG.warn("No billing address provide by Pay Pal. Use empty billing address...");
			 hybrisBillingAddress = new AddressData();
			 hybrisBillingAddress.setEmail(payPalEmail);
			 subscriptionInfo.setAddressData(hybrisBillingAddress);
		 }
			boolean isSuccess = false;
			AbstractOrderModel order = null;
			try {
				order = brainTreeCheckoutFacade.getOrderByCode(orderCode);
				if (null != order) {
					final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentFacade
							.completeCreateSubscription(
									subscriptionInfo, (CustomerModel) order.getUser(), order, false, false);
					if (null != paymentInfo) {
						isSuccess = brainTreeTransactionService.createAuthorizationTransactionOfOrder(order,
								BigDecimal.valueOf(modifyOrderTotal).setScale(DECIMAL_PRECISION, RoundingMode.HALF_EVEN), true, paymentInfo);
					}
				}
			} catch (final Exception exception) {
				final String errorMessage = getLocalizedString("braintree.billing.general.error");
				handleErrors(errorMessage, model);
				return CheckoutOrderPageErrorPage;
			}
			if (isSuccess) {
				order.setStatus(OrderStatus.SHIPPED);
				order.setIsCaptured(Boolean.TRUE);
				modelService.save(order);
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
						getLocalizedString("text.account.modify.payment.success"));
				return REDIRECT_TO_ORDER_DETAILS_PAGE + orderCode;

			} else {
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
						getLocalizedString("text.account.modify.payment.error"));
				return REDIRECT_PREFIX  + MY_ACCOUNT_MODIFY_PAYMENT + orderCode;
			}
    }

	@GetMapping(value = "/mini/express")
	@RequireHardLogIn
	@ResponseBody
	public String doInitializeMiniCartPaypalShortcut() throws CMSItemNotFoundException, JsonGenerationException,
	                                                           JsonMappingException, IOException
	{
		return buildPayPalMiniCartResponse();
	}

	private String buildPayPalMiniCartResponse() throws JsonGenerationException, JsonMappingException, IOException
	{
		final ObjectMapper mapper = new ObjectMapper();
		final PayPalMiniCartResponse payPalMiniCartResponse = new PayPalMiniCartResponse();

		String clientToken = StringUtils.EMPTY;

		try
		{
			clientToken = brainTreeCheckoutFacade.generateClientToken();
		}
		catch (final AdapterException exception)
		{
			LOG.error("[Brain Tree Controller] Error during token generation!");
		}
		final PayPalCheckoutData payPalCheckoutData = brainTreeCheckoutFacade.getPayPalCheckoutData();
		payPalMiniCartResponse.setCheckoutData(payPalCheckoutData);
		payPalMiniCartResponse.setClientToken(clientToken);
		return mapper.writeValueAsString(payPalMiniCartResponse);
	}
	@ResponseBody
	@PostMapping(value = "/shippingAddressError")
	public void handleShippingAddressError(final Model model, final @RequestParam(value = "errorMessage", required = false) String errorMessage) throws CMSItemNotFoundException
	{
		LOG.error("Not correct shipping address. Error message: " + errorMessage);
		GlobalMessages.addMessage(model, "accErrorMsgs", "braintree.general.error.shippingAddress", new String[]{errorMessage});
		getSessionService().getCurrentSession().setAttribute("braintree.general.error.shippingAddress",
				model.asMap().get("accErrorMsgs"));
	}

	private BrainTreeSubscriptionInfoData buildSubscriptionInfo(final String nonce, final String paymentProvider,
			final boolean shouldBeSaved, final boolean isSaved, final String payPalEmail)
	{
		final BrainTreeSubscriptionInfoData subscriptionInfo = new BrainTreeSubscriptionInfoData();
		subscriptionInfo.setPaymentProvider(paymentProvider);
		subscriptionInfo.setSavePaymentInfo(isSaved);
		subscriptionInfo.setShouldBeSaved(shouldBeSaved);
		subscriptionInfo.setNonce(nonce);
		subscriptionInfo.setEmail(payPalEmail);

		return subscriptionInfo;
	}

	private void handleErrors(final String errorsDetail, final Model model) throws CMSItemNotFoundException
	{
		model.addAttribute("errorsDetail", errorsDetail);
		final String redirectUrl = REDIRECT_URL_CART;
		model.addAttribute("redirectUrl", redirectUrl.replace(REDIRECT_PREFIX, ""));
		model.addAttribute(WebConstants.BREADCRUMBS_KEY,
				getResourceBreadcrumbBuilder().getBreadcrumbs("checkout.multi.hostedOrderPageError.breadcrumb"));
		storeCmsPageInModel(model, getContentPageForLabelOrId(BraintreeaddonWebConstants.MULTI_CHECKOUT_SUMMARY_CMS_PAGE_LABEL));
		setUpMetaDataForContentPage(model,
				getContentPageForLabelOrId(BraintreeaddonWebConstants.MULTI_CHECKOUT_SUMMARY_CMS_PAGE_LABEL));

		GlobalMessages.addErrorMessage(model, getLocalizedString(PAY_PAL_HAED_ERROR));
	}


	/**
	 * This method created for extend order pay pal payment
	 */
	@PostMapping(value = "/extendOrder-payment")
	public String extendOrderPayment(final Model model, final RedirectAttributes redirectAttributes,
			@RequestParam(value = "selectedAddressCode", required = false) final String selectedAddressCode,
			final HttpServletRequest request, final HttpServletResponse response) throws CMSItemNotFoundException {
		PayPalExpressResponse payPalExpressResponse = null;
		AddressData hybrisBillingAddress = null;
		final String orderCode = request.getParameter("extend_Order_Code");
		try {
			payPalExpressResponse = payPalResponseExpressCheckoutHandler.handlePayPalResponse(request);
		} catch (final IllegalArgumentException exeption) {
			handleErrors(exeption.getMessage(), model);
			return REDIRECT_PREFIX + BlControllerConstants.MY_ACCOUNT_EXTEND_RENTAL + orderCode;
		}

		String payPalEmail = payPalExpressResponse.getDetails().getEmail();

		String paymentProvider = BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT;

		if (payPalExpressResponse.getType().equals(BraintreeConstants.APPLE_PAY_CARD))
		{
			paymentProvider = BraintreeConstants.APPLE_PAY_CARD;
		}
		else if (payPalExpressResponse.getType().equals(BraintreeConstants.VENMO_CHECKOUT))
		{
			paymentProvider = BraintreeConstants.VENMO_CHECKOUT;
			payPalEmail = payPalExpressResponse.getDetails().getUsername();
			hybrisBillingAddress = brainTreeUserFacade.getAddressForCode(selectedAddressCode);
		}
		else if(payPalExpressResponse.getType().equals(BraintreeConstants.ANDROID_PAY_CARD))
		{
			paymentProvider = BraintreeConstants.ANDROID_PAY_CARD;
		}

		final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo(payPalExpressResponse.getNonce(),
				paymentProvider, Boolean.FALSE, Boolean.TRUE, payPalEmail);

		final PayPalAddressData payPalBillingAddress = payPalExpressResponse.getDetails().getBillingAddress();
		if (payPalBillingAddress != null)
		{
			hybrisBillingAddress = payPalResponseExpressCheckoutHandler.getPayPalAddress(
					payPalExpressResponse.getDetails(), payPalBillingAddress);
			hybrisBillingAddress.setEmail(payPalExpressResponse.getDetails().getEmail());
			subscriptionInfo.setAddressData(hybrisBillingAddress);
		}
		else if (paymentProvider.equals(BraintreeConstants.VENMO_CHECKOUT))
		{
			subscriptionInfo.setAddressData(hybrisBillingAddress);
		}
		else
		{
			LOG.warn("No billing address provide by Pay Pal. Use empty billing address...");
			hybrisBillingAddress = new AddressData();
			hybrisBillingAddress.setEmail(payPalEmail);
			subscriptionInfo.setAddressData(hybrisBillingAddress);
		}
		boolean isSuccess = false;
		 OrderModel order = null;
		try {
			order = brainTreePaymentFacade.gerExtendOrderFromOrderCode(orderCode);
			if(null != order && BooleanUtils.isTrue(order.getIsExtendedOrder())) {
				final BrainTreePaymentInfoModel paymentInfo = brainTreePaymentFacade
						.completeCreateSubscription(subscriptionInfo,
								(CustomerModel) order.getUser(), order, false, false);
				if(null != paymentInfo) {
					isSuccess = brainTreeTransactionService.createAuthorizationTransactionOfOrder(order,
							BigDecimal.valueOf(order.getTotalPrice()), true, paymentInfo);
				}
			}
		} catch (final Exception exception) {
			final String errorMessage = getLocalizedString("braintree.billing.general.error");
			handleErrors(errorMessage, model);
			return REDIRECT_PREFIX + BlControllerConstants.MY_ACCOUNT_EXTEND_RENTAL + orderCode;
		}
		if(isSuccess) {
			blOrderFacade.updateOrderExtendDetails(order); //to update extend order details to DB
			final OrderData extendOrderData = blOrderFacade.getExtendedOrderDetailsFromOrderCode(orderCode);
			model.addAttribute(BlControllerConstants.EXTEND_ORDER_DATA, extendOrderData);
			model.addAttribute(BlControllerConstants.PAYMENT_METHOD , BlControllerConstants.PAY_PAL);
			final ContentPageModel extendOrderConfirmation = getContentPageForLabelOrId(EXTEND_RENTAL_ORDER_CONFIRMATION);
			storeCmsPageInModel(model, extendOrderConfirmation);
			model.addAttribute(ThirdPartyConstants.SeoRobots.META_ROBOTS, ThirdPartyConstants.SeoRobots.NOINDEX_NOFOLLOW);
			setUpMetaDataForContentPage(model, extendOrderConfirmation);
			return getViewForPage(model);
		} else {
			GlobalMessages.addErrorMessage(model, getLocalizedString("braintree.paypal.error"));
			return REDIRECT_PREFIX + BlControllerConstants.MY_ACCOUNT_EXTEND_RENTAL + orderCode;
		}
	}


	/**
	 * Adds the paypal global error message.
	 *
	 * @param errorMessageKey the error message key
	 * @param redirectAttributes the redirect attributes
	 */
	private void addPayPalErrorMessage(final String errorMessageKey, final RedirectAttributes redirectAttributes) {
	  GlobalMessages.addFlashMessage(redirectAttributes,GlobalMessages.ERROR_MESSAGES_HOLDER,getLocalizedString(errorMessageKey),null);
	}

	protected ResourceBreadcrumbBuilder getResourceBreadcrumbBuilder()
	{
		return resourceBreadcrumbBuilder;
	}

	private String getSessionCartUserUid()
	{
		return cartService.getSessionCart().getUser().getUid();
	}

	/**
	 * This method converts double to price data
	 */
	private PriceData convertDoubleToPriceData(final Double price , final AbstractOrderModel orderModel) {
		return priceDataFactory.create(PriceDataType.BUY ,BigDecimal.valueOf(price),orderModel.getCurrency());
	}
}
