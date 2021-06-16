package com.braintree.controllers.pages;

import com.braintree.controllers.BraintreeaddonControllerConstants;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.facade.impl.BrainTreePaymentFacadeImpl;
import com.braintree.transaction.service.impl.BrainTreeTransactionServiceImpl;
import de.hybris.platform.acceleratorfacades.order.AcceleratorCheckoutFacade;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.checkout.steps.CheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.checkout.steps.AbstractCheckoutStepController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.order.InvalidCartException;
import de.hybris.platform.servicelayer.user.UserConstants;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.web.csrf.CsrfToken;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import org.springframework.ws.support.WebUtils;

import javax.annotation.Resource;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.util.Map;

import static com.braintree.constants.ControllerConstants.Views.Pages.MultiStepCheckout.FallbackPage;
import static de.hybris.platform.addonsupport.controllers.AbstractAddOnController.REDIRECT_PREFIX;
import static de.hybris.platform.addonsupport.controllers.AbstractAddOnController.ROOT;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;


@Controller
@RequestMapping(value = "/braintree/checkout/lpm")
public class LocalPaymentController extends AbstractCheckoutStepController
{
	private static final Logger LOG = Logger.getLogger(LocalPaymentController.class);

	private final static String PAYMENT_METHOD = "payment-method";
	private final static String REDIRECT_TO_SUMMARY_VIEW = REDIRECT_PREFIX + "/checkout/multi/summary/braintree/view";

	@Resource(name = "brainTreePaymentFacadeImpl")
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;

	@Resource(name = "brainTreeTransactionService")
	private BrainTreeTransactionServiceImpl brainTreeTransactionService;

	@RequestMapping(value = "/savePayment", method = RequestMethod.POST)
	@RequireHardLogIn
	public ResponseEntity savePayment(HttpServletRequest request, HttpServletResponse response)
	{
		final String paymentId = request.getParameter("paymentId");
		brainTreePaymentFacade.createLocalPaymentMethodSubscription(paymentId);
		return new ResponseEntity(HttpStatus.OK);
	}

	@RequestMapping(value = "/process", method = RequestMethod.POST)
	@RequireHardLogIn
	public String processLpm(HttpServletRequest request, HttpServletResponse response,
			final RedirectAttributes redirectAttributes)
	{
		final String paymentMethodNonce = request.getParameter("bt_payment_method_nonce");
		final String deviceData = request.getParameter("device_data");
		final String payerEmail = request.getParameter("payer_email");

		if(brainTreePaymentFacade.getOrderByPaymentId(paymentMethodNonce) != null){
			redirectToOrderConfirmationPage(brainTreePaymentFacade.getOrderByPaymentId(paymentMethodNonce));
		}

		brainTreePaymentFacade.updateLocalPaymentMethodSubscription(paymentMethodNonce, deviceData, payerEmail);
		boolean isPaymentAuthorized = brainTreeTransactionService.createAuthorizationTransaction();
		if (!isPaymentAuthorized)
		{
			String localizedErrorMessage = getLocalizedString("checkout.error.authorization.failed");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);
			return REDIRECT_TO_SUMMARY_VIEW;
		}
		OrderData orderData;
		try
		{
			orderData = getCheckoutFacade().placeOrder();
			LOG.info("Order has been placed, number/code: " + orderData.getCode());
		}
		catch (final Exception e)
		{
			LOG.error("Failed to place Order, message: " + e.getMessage(), e);
			String localizedErrorMessage = getLocalizedString("checkout.placeOrder.failed");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);
			return REDIRECT_TO_SUMMARY_VIEW;
		}
		return redirectToOrderConfirmationPage(orderData);
	}

	@RequestMapping(value = "/fallback", method = RequestMethod.GET)
	public String fallback(@RequestParam Map<String,String> allParams, HttpServletRequest request, HttpServletResponse response,
			final RedirectAttributes redirectAttributes, Model model) throws InvalidCartException
	{
		final String paymentId = allParams.get("btLpPaymentId");

			model.addAttribute("paymentId",  paymentId );

	return FallbackPage;
	}


	@RequestMapping(value = "/processFallback", method = RequestMethod.POST)
	public String processFallback(HttpServletRequest request, HttpServletResponse response,
			final RedirectAttributes redirectAttributes)
	{
		final String paymentId = request.getParameter("paymentId");
				final String paymentMethodToken = request.getParameter("paymentNonce");

				CartModel cartByPaymentId = brainTreePaymentFacade.getCartByPaymentId(paymentId);
				if (cartByPaymentId != null)
				{
					brainTreePaymentFacade.updateLocalPaymentMethodSubscription(paymentMethodToken, cartByPaymentId);
					AcceleratorCheckoutFacade checkoutFacade = getCheckoutFacade();
					if (checkoutFacade instanceof BrainTreeCheckoutFacade)
					{
						BrainTreeCheckoutFacade brainTreeCheckoutFacade = (BrainTreeCheckoutFacade) checkoutFacade;

						boolean isAuthorized = brainTreeCheckoutFacade.authorizePayment(cartByPaymentId);

						if (!isAuthorized)
						{
							String localizedErrorMessage = getLocalizedString("checkout.error.authorization.failed");
							GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);
							return REDIRECT_TO_SUMMARY_VIEW;
						}
						OrderData orderData;
						try
						{
							orderData = brainTreeCheckoutFacade.placeOrderByCart(cartByPaymentId);
							;
							LOG.info("Order has been placed, number/code: " + orderData.getCode());
						}
						catch (final Exception e)
						{
							LOG.error("Failed to place Order, message: " + e.getMessage(), e);
							String localizedErrorMessage = getLocalizedString("checkout.placeOrder.failed");
							GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);
							return REDIRECT_TO_SUMMARY_VIEW;
						}
						return redirectToFallbackOrderConfirmationPage(orderData);
					}
				}
				return REDIRECT_PREFIX + ROOT;
	}

	protected String redirectToFallbackOrderConfirmationPage(final OrderData orderData)
	{
		return REDIRECT_URL_ORDER_CONFIRMATION + (orderData.isGuestCustomer() ? orderData.getGuid() : orderData.getCode());
	}

	@Override public String enterStep(Model model, RedirectAttributes redirectAttributes)
			throws CMSItemNotFoundException, CommerceCartModificationException
	{
		return StringUtils.EMPTY;
	}

	@Override public String back(RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().previousStep();
	}

	@Override public String next(RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().nextStep();
	}

	protected CheckoutStep getCheckoutStep()
	{
		return getCheckoutStep(PAYMENT_METHOD);
	}
}
