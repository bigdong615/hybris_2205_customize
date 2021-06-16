package com.braintree.controllers.cms;

import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.configuration.service.BrainTreeSupportedLocaleConfig;
import com.braintree.constants.ControllerConstants;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.model.PayPalCreditMessageComponentModel;
import de.hybris.platform.addonsupport.controllers.cms.AbstractCMSAddOnComponentController;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.servicelayer.i18n.I18NService;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

import java.util.Locale;

import static com.braintree.constants.BraintreeaddonWebConstants.BRAINTREE_LOCALE;
import static com.braintree.constants.BraintreeaddonWebConstants.CREDIT_ENABLED;
import static com.braintree.constants.BraintreeaddonWebConstants.IS_CREDIT_MESSAGE_ENABLED;
import static com.braintree.controllers.BraintreeaddonControllerConstants.CLIENT_TOKEN;


@Controller("PayPalCreditMessageComponentController")
@Scope("tenant")
@RequestMapping(value = ControllerConstants.Actions.Cms.PayPalCreditMessageComponent)
public class PayPalCreditMessageComponentController extends AbstractCMSAddOnComponentController<PayPalCreditMessageComponentModel>
{
	private final static Logger LOG = Logger.getLogger(PayPalCreditMessageComponentController.class);

	private static final String CHECKOUT_DATA = "checkoutData";

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "brainTreeConfigService")
	private BrainTreeConfigService brainTreeConfigService;

	@Resource
	private I18NService i18NService;

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * de.hybris.platform.addonsupport.controllers.cms.AbstractCMSAddOnComponentController#fillModel(javax.servlet.http
	 * .HttpServletRequest, org.springframework.ui.Model,
	 * de.hybris.platform.cms2.model.contents.components.AbstractCMSComponentModel)
	 */
	@Override
	protected void fillModel(final HttpServletRequest request, final Model model,
							 final PayPalCreditMessageComponentModel component)
	{
		setBraintreeLocale(model);
		model.addAttribute(CHECKOUT_DATA, brainTreeCheckoutFacade.getPayPalCheckoutData());
		model.addAttribute(IS_CREDIT_MESSAGE_ENABLED, component.getEnabled());
		model.addAttribute(CREDIT_ENABLED, brainTreeConfigService.getCreditEnabled());
		try
		{
			model.addAttribute(CLIENT_TOKEN, brainTreeCheckoutFacade.generateClientToken());
		}
		catch (final AdapterException exception)
		{
			LOG.error("[View Handler] Error during token generation!");
		}
	}

	private void setBraintreeLocale(final Model model)
	{
		final String brainTreeLocale = brainTreeConfigService.getBrainTreeLocale();
		try
		{
			if (StringUtils.isNotEmpty(brainTreeLocale) && BrainTreeSupportedLocaleConfig.supportLocale(brainTreeLocale))
			{
				model.addAttribute(BRAINTREE_LOCALE, brainTreeLocale);
			}
			else
			{
				final Locale locale = BrainTreeSupportedLocaleConfig
						.getSupportedLocaleByLanguage(i18NService.getCurrentLocale().getLanguage());
				model.addAttribute(BRAINTREE_LOCALE, locale);
			}
		}
		catch (final Exception e)
		{
			model.addAttribute(BRAINTREE_LOCALE,
					BrainTreeSupportedLocaleConfig.getDefaultLocale(i18NService.getCurrentLocale().getLanguage()));
		}
	}
}

