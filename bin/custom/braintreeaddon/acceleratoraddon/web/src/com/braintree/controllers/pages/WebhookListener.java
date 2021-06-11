package com.braintree.controllers.pages;


import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.facade.impl.BrainTreePaymentFacadeImpl;
import com.braintree.transaction.service.BrainTreeTransactionService;
import com.braintreegateway.Configuration;
import com.braintreegateway.WebhookNotification;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.core.model.order.CartModel;
import org.apache.log4j.Logger;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


@Controller
@RequestMapping(value = "/paypal/webhook")
public class WebhookListener
{
	private static final Logger LOG = Logger.getLogger(WebhookListener.class);

	@Resource(name = "brainTreeConfigService")
	private BrainTreeConfigService brainTreeConfigService;

	@Resource(name = "brainTreeTransactionService")
	BrainTreeTransactionService brainTreeTransactionService;

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "brainTreePaymentFacadeImpl")
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;

	private Configuration configuration = null;

	@RequestMapping(value = "/response")
	public ResponseEntity<String> doHandleBillingAgreemenHopResponse(final HttpServletRequest request,
			final HttpServletResponse response) throws Exception
	{
		if ("POST".equalsIgnoreCase(request.getMethod()))
		{
			final WebhookNotification webhookNotification = brainTreePaymentFacade.getWebhookNotification(request.getParameterMap());
			final String paymentId = webhookNotification.getLocalPaymentCompleted().getPaymentId();
			final String paymentMethodNonce = webhookNotification.getLocalPaymentCompleted().getPaymentMethodNonce();

			CartModel cart = brainTreePaymentFacade.getCartByPaymentId(paymentId);

			if (cart != null)
			{
				brainTreePaymentFacade.updateLocalPaymentMethodSubscription(paymentMethodNonce, cart);

				boolean authorized = brainTreeCheckoutFacade.authorizePayment(cart);
				if (authorized)
				{
					final OrderData orderData = brainTreeCheckoutFacade.placeOrderByCart(cart);
					LOG.info(orderData.getCode() + " was placed due to the webhook  : " + webhookNotification.getKind());
				}
			}

		}
		return ResponseEntity.ok("200");
	}

}
