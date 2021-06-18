/**
 *
 */
package com.braintree.controllers.pages;

import com.braintree.commands.impl.BraintreeErrorTranslator;
import com.braintree.constants.BraintreeaddonWebConstants;
import com.braintree.constants.BraintreeConstants;
import com.braintree.controllers.handler.PayPalResponseExpressCheckoutHandler;
import com.braintree.controllers.handler.PayPalUserLoginHandler;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.facade.impl.BrainTreePaymentFacadeImpl;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.hybris.data.PayPalAddressData;
import com.braintree.hybris.data.PayPalCheckoutData;
import com.braintree.hybris.data.PayPalExpressResponse;
import com.braintree.hybris.data.PayPalMiniCartResponse;
import com.braintree.security.PayPalGUIDCookieStrategy;
import com.fasterxml.jackson.core.JsonGenerationException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractCheckoutController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.user.UserFacade;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commerceservices.customer.DuplicateUidException;
import de.hybris.platform.core.model.order.delivery.DeliveryModeModel;
import de.hybris.platform.core.model.order.payment.PaymentInfoModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.servicelayer.session.SessionService;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

import static com.braintree.controllers.BraintreeaddonControllerConstants.*;
import static com.braintree.controllers.BraintreeaddonControllerConstants.Views.Pages.MultiStepCheckout.CheckoutOrderPageErrorPage;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;


@Controller
@RequestMapping(value = "/braintree/paypal/checkout")
public class PayPalPaymentController extends AbstractCheckoutController
{
	protected static final String REDIRECT_URL_CART = REDIRECT_PREFIX + BraintreeaddonWebConstants.CART_URL;
	private static final String REDIRECT_TO_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/payment-details";
	private static final String ANONYMOUS_USER = "anonymous";
	private static final String SAVE_PAYMENT_INFO = "isSaved";
	private static final String DEVICE_DATA = "device_data";

	private static final Logger LOG = Logger.getLogger(PayPalPaymentController.class);

	@Resource(name = "payPalUserLoginHandler")
	PayPalUserLoginHandler payPalUserLoginHandler;

	@Resource(name = "payPalResponseExpressCheckoutHandler")
	PayPalResponseExpressCheckoutHandler payPalResponseExpressCheckoutHandler;

	@Resource(name = "payPalGUIDCookieStrategy")
	private PayPalGUIDCookieStrategy payPalGUIDCookieStrategy;

	@Resource(name = "brainTreePaymentFacadeImpl")
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;

	@Resource(name = "userFacade")
	private UserFacade userFacade;

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "multiStepCheckoutBreadcrumbBuilder")
	private ResourceBreadcrumbBuilder resourceBreadcrumbBuilder;

	@Resource
	private SessionService sessionService;

	@Resource
	private CartService cartService;

	@RequestMapping(value = "/express", method = RequestMethod.POST)
	public String doHandleHopResponse(final Model model, final RedirectAttributes redirectAttributes,
                                      final HttpServletRequest request, final HttpServletResponse response) throws CMSItemNotFoundException
	{

		PayPalExpressResponse payPalExpressResponse = null;
		boolean isSavePaymentInfo = false;
		String deviceData = null;

		try
		{
			payPalExpressResponse = payPalResponseExpressCheckoutHandler.handlePayPalResponse(request);
      isSavePaymentInfo = true; /* Boolean.parseBoolean(request.getParameter(SAVE_PAYMENT_INFO)); */
			deviceData = request.getParameter(DEVICE_DATA);
		}
		catch (final IllegalArgumentException exeption)
		{
			handleErrors(exeption.getMessage(), model);
			return CheckoutOrderPageErrorPage;
		}
        final DeliveryModeModel selectedDeliveryMode = cartService.getSessionCart().getDeliveryMode();
		final String payPalEmail = payPalExpressResponse.getDetails().getEmail();
		if (ANONYMOUS_USER.equals(getSessionCartUserUid()))
		{
			try
			{
				if (StringUtils.isEmpty(payPalEmail))
				{
					LOG.error("Pal pal email is empty!");
					handleErrors(getLocalizedString(BraintreeErrorTranslator.DEFAULT_MESSAGE_KEY, new Object[]
					{ StringUtils.EMPTY }), model);
					return CheckoutOrderPageErrorPage;
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
				LOG.error("Guest registration failed: " + exception);
				handleErrors(getLocalizedString(PAY_PAL_GUEST_REGISTER_ERROR), model);
				return CheckoutOrderPageErrorPage;
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
			userFacade.addAddress(hybrisShippingAddress);
			brainTreeCheckoutFacade.setDeliveryAddress(hybrisShippingAddress);
		}
		else
		{
			if (brainTreeCheckoutFacade.getCheckoutCart().getDeliveryAddress() == null
					&& brainTreeCheckoutFacade.getCheckoutCart().getPickupOrderGroups().isEmpty())
			{
				LOG.error("Shipping address from pay pal is empty!");
				final String errorMessage = getLocalizedString(PAY_PAL_ADDRESS_ERROR);
				handleErrors(errorMessage, model);
				return CheckoutOrderPageErrorPage;
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
			final String errorMessage = getLocalizedString("braintree.billing.general.error");
			handleErrors(errorMessage, model);
			return CheckoutOrderPageErrorPage;
		}
		return REDIRECT_PREFIX + "/checkout/multi/summary/braintree/view";

	}

    @RequestMapping(value = "/add-payment-method", method = RequestMethod.POST)
    public String addPaymentMethod(final Model model, final RedirectAttributes redirectAttributes,
			                          @RequestParam(value = "selectedAddressCode", required = false) final String selectedAddressCode,
                                   final HttpServletRequest request, final HttpServletResponse response) throws CMSItemNotFoundException {
        PayPalExpressResponse payPalExpressResponse = null;
        AddressData hybrisBillingAddress = null;
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
			hybrisBillingAddress = userFacade.getAddressForCode(selectedAddressCode);
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

        try {
            brainTreePaymentFacade.completeCreateSubscription(subscriptionInfo, false);
        } catch (final Exception exception) {
            final String errorMessage = getLocalizedString("braintree.billing.general.error");
            handleErrors(errorMessage, model);
            return CheckoutOrderPageErrorPage;
        }
        GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
                getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.success"));
        return REDIRECT_TO_PAYMENT_INFO_PAGE;
    }

	@RequestMapping(value = "/mini/express", method = RequestMethod.GET)
	@RequireHardLogIn
	@ResponseBody
	public String doInitializeMiniCartPaypalShortcut() throws CMSItemNotFoundException, JsonGenerationException,
            JsonMappingException, IOException
	{
		final String jsonInString = buildPayPalMiniCartResponse();
		return jsonInString;
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
	@RequestMapping(value = "/shippingAddressError", method = RequestMethod.POST)
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

	protected ResourceBreadcrumbBuilder getResourceBreadcrumbBuilder()
	{
		return resourceBreadcrumbBuilder;
	}

	private String getSessionCartUserUid()
	{
		return cartService.getSessionCart().getUser().getUid();
	}
}
