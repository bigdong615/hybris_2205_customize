package com.braintree.controllers.pages;

import com.braintree.enums.BrainTreePaymentMethod;
import com.braintree.facade.impl.BrainTreePaymentFacadeImpl;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import static de.hybris.platform.util.localization.Localization.getLocalizedString;


@Controller
@RequestMapping(value = "/braintree/checkout/venmo")
public class VenmoPaymentController extends AbstractPageController
{

	private static final Logger LOG = Logger.getLogger(VenmoPaymentController.class);

	private final static String REDIRECT_TO_SUMMARY_VIEW = REDIRECT_PREFIX + "/checkout/multi/summary/braintree/view";
	private final static String REDIRECT_TO_SILENT_ORDER_PAGE =  REDIRECT_PREFIX + "/checkout/multi/payment-method/add";

	@Resource(name = "brainTreePaymentFacadeImpl")
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;

	@RequestMapping(value = "/process", method = RequestMethod.POST)
	public String processVenmo(HttpServletRequest request, HttpServletResponse response,
			final RedirectAttributes redirectAttributes)
	{
		final String paymentMethodNonce = request.getParameter("bt_payment_method_nonce");
		final String userName = request.getParameter("username");
		final String deviceData = request.getParameter("device_data");
		final boolean isSavePaymentInfo = Boolean.parseBoolean(request.getParameter("isSaved"));

		BrainTreeSubscriptionInfoData subscriptionInfo = brainTreePaymentFacade.buildVenmoSubscriptionInfo(paymentMethodNonce,
				BrainTreePaymentMethod.VENMOACCOUNT.toString(), isSavePaymentInfo, userName, deviceData);
		try
		{
			brainTreePaymentFacade.completeCreateSubscription(subscriptionInfo);
		}
		catch (final Exception exception)
		{
			String localizedErrorMessage = getLocalizedString("braintree.billing.general.error");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);
			return REDIRECT_TO_SILENT_ORDER_PAGE;
		}
		return REDIRECT_TO_SUMMARY_VIEW;
	}
}
