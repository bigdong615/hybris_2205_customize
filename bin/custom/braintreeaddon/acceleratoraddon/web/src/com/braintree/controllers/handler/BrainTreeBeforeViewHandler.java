/**
 *
 */
package com.braintree.controllers.handler;

import braintreehac.services.PayPalButtonConfigurationService;
import com.braintree.configuration.BrainTreeConfigurationListener;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.configuration.service.BrainTreeSupportedLocaleConfig;
import com.braintree.constants.BraintreeConstants;
import com.braintree.constants.ControllerConstants;
import com.braintree.controllers.BraintreeaddonControllerConstants;
import com.braintree.facade.BrainTreeAccountFacade;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.facade.impl.BrainTreePaymentFacadeImpl;
import com.braintree.facade.impl.BrainTreeUserFacadeImpl;
import com.braintree.hybris.data.PayPalCheckoutData;
import com.braintree.hybris.data.PayPalConfigurationData;
import com.braintree.model.BraintreeLocalPaymentMethodsModel;
import com.braintree.model.PayPalCreditMessageComponentModel;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.addonsupport.interceptors.BeforeViewHandlerAdaptee;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.servicelayer.services.CMSComponentService;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.core.Registry;
import de.hybris.platform.payment.AdapterException;
import de.hybris.platform.servicelayer.i18n.I18NService;
import de.hybris.platform.servicelayer.session.SessionService;
import de.hybris.platform.util.config.ConfigIntf;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.ModelMap;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_CAPTURE;
import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_ORDER;
import static com.braintree.constants.BraintreeConstants.PAYPAL_INTENT_SALE;
import static com.braintree.constants.BraintreeaddonWebConstants.ACCEPTED_PAYMENTS_METHODS_IMAGES_URL;
import static com.braintree.constants.BraintreeaddonWebConstants.APPLE_PAY_ENABLE;
import static com.braintree.constants.BraintreeaddonWebConstants.BILLING_AGREEMENT_DESCRIPTION;
import static com.braintree.constants.BraintreeaddonWebConstants.BRAINTREE_LOCALE;
import static com.braintree.constants.BraintreeaddonWebConstants.CREDIT_ENABLED;
import static com.braintree.constants.BraintreeaddonWebConstants.CREDIT_MESSAGE_COMPONENT;
import static com.braintree.constants.BraintreeaddonWebConstants.CURRENCY_MERCHANT_ACCOUNT;
import static com.braintree.constants.BraintreeaddonWebConstants.DISABLE_MARK_FUNDING;
import static com.braintree.constants.BraintreeaddonWebConstants.GOOGLE_PAY_COUNTRY_CODE;
import static com.braintree.constants.BraintreeaddonWebConstants.GOOGLE_PAY_ENABLE;
import static com.braintree.constants.BraintreeaddonWebConstants.GOOGLE_PAY_MERCHANT_ID;
import static com.braintree.constants.BraintreeaddonWebConstants.HOSTED_FIELDS_ENABLE;
import static com.braintree.constants.BraintreeaddonWebConstants.IS_CREDIT_MESSAGE_ENABLED;
import static com.braintree.constants.BraintreeaddonWebConstants.LOCAL_PAYMENTS_ENABLED;
import static com.braintree.constants.BraintreeaddonWebConstants.PAYMENT_INFOS;
import static com.braintree.constants.BraintreeaddonWebConstants.PAY_PAL_EXPRESS_ENABLE;
import static com.braintree.constants.BraintreeaddonWebConstants.PAY_PAL_STANDARD_ENABLE;
import static com.braintree.constants.BraintreeaddonWebConstants.VENMO_ENABLE;
import static com.braintree.controllers.BraintreeaddonControllerConstants.ADD_PAYMENT_METHOD_SHOW;
import static com.braintree.controllers.BraintreeaddonControllerConstants.CLIENT_TOKEN;
import static com.braintree.controllers.BraintreeaddonControllerConstants.PAY_PAL_CHECKOUT_DATA;
import static com.braintree.controllers.BraintreeaddonControllerConstants.PAY_PAL_CONFIGURATION_DATA;
import static com.braintree.controllers.BraintreeaddonControllerConstants.Views.Pages.Checkout.CheckoutConfirmationPage;
import static com.braintree.controllers.BraintreeaddonControllerConstants.Views.Pages.MultiStepCheckout.CheckoutSummaryPage;
import static com.braintree.controllers.BraintreeaddonControllerConstants.Views.Pages.MultiStepCheckout.FallbackPage;
import static com.braintree.controllers.BraintreeaddonControllerConstants.Views.Pages.MultiStepCheckout.SilentOrderPostPage;
import static org.apache.commons.lang.StringUtils.isNotBlank;


public class BrainTreeBeforeViewHandler implements BeforeViewHandlerAdaptee

{
	private final static Logger LOG = Logger.getLogger(BrainTreeBeforeViewHandler.class);
	private I18NService i18NService;
	private SessionService sessionService;

	private static final String B2C_CHECKOUT_CONFIRMATION_PAGE = "pages/checkout/checkoutConfirmationPage";
	private static final String CART_PAGE = "pages/cart/cartPage";
	private static final String BRAIN_TREE_PAYMENT_DATA = "brainTreePaymentInfoData";
	private static final String ORDER_DATA = "orderData";
	private static final String B2C_ADD_TO_CART_POPUP_PAGE = "fragments/cart/addToCartPopup";
	private static final String B2C_CART_POPUP_PAGE = "fragments/cart/cartPopup";
	private static final String AccountLayoutPage = "pages/account/accountLayoutPage";
	private static final String B2C_CHECKOUT_CONFIRMATION_LAYOUT_PAGE = "pages/checkout/checkoutConfirmationLayoutPage";
	private static final String CART_BUTTON_CONFIG = "cart.button.config";
	private static final String MARK_BUTTON_CONFIG = "mark.button.config";
	private static final String MINI_CART_BUTTON_CONFIG = "mini.cart.button.config";
	private static final String PAYPAL_BUTTON_CONFIG = "payPalButtonConfig";
	private static final String PAYPAL_MARK_BUTTON_CONFIG = "payPalMarkButtonConfig";
	private static final String B2C_CHECKOUT_DELIVERY_ADDRESS_PAGE = "pages/checkout/multi/addEditDeliveryAddressPage";
	private static final String LOCAL_PAYMENT_METHODS = "localPaymentMethods";
	private static final String VENMO_PROFILE_ID = "venmoProfileId";
	private static final String ENDPOINT_URL = "endpointURL";
	private static final String PAYPAL_CREDIT_MESSAGE = "PayPalCartPageCreditMessageComponent";


	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;
	@Resource(name = "brainTreePaymentFacadeImpl")
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;
	@Resource(name = "brainTreeUserFacade")
	private BrainTreeUserFacadeImpl brainTreeUserFacade;
	@Resource(name = "brainTreeConfigService")
	private BrainTreeConfigService brainTreeConfigService;
	@Resource(name = "cmsComponentService")
	private CMSComponentService cmsComponentService;
	@Resource(name = "brainTreeAccountFacade")
	private BrainTreeAccountFacade brainTreeAccountFacade;
	@Autowired
	private PayPalButtonConfigurationService payPalButtonConfigurationService;

	private ConfigIntf.ConfigChangeListener configurationChangeListener;

	@Override
	public String beforeView(final HttpServletRequest request, final HttpServletResponse response, final ModelMap model,
			final String viewName) throws Exception
	{

		if ((!B2C_CART_POPUP_PAGE.equals(viewName) && !B2C_ADD_TO_CART_POPUP_PAGE.equals(viewName))
				&& !isPaymentMethodsAvailable(viewName))
		{
			return viewName;
		}
		return handleBrainTreeCheckoutScenario(model, viewName);

	}

  private String handleBrainTreeCheckoutScenario(final ModelMap model, final String viewName) throws CMSItemNotFoundException
  {
    try
    {
      if (configurationChangeListener == null)
      {
        registerConfigChangeLister();
      }

      if (CART_PAGE.equals(viewName))
      {
        setPayPalExpressEnabled(model);
        fillPaymentMethodsInfo(model);
        model.addAttribute(PAYPAL_BUTTON_CONFIG, payPalButtonConfigurationService.getFormattedProperty(CART_BUTTON_CONFIG));
        final PayPalCreditMessageComponentModel creditMessageComponent = cmsComponentService.getSimpleCMSComponent(PAYPAL_CREDIT_MESSAGE);
        model.addAttribute(CREDIT_MESSAGE_COMPONENT, creditMessageComponent);
        model.addAttribute(IS_CREDIT_MESSAGE_ENABLED, creditMessageComponent.getEnabled());
      }

      else if (ControllerConstants.Views.Pages.MultiStepCheckout.SilentOrderPostPage.equals(viewName))
      {
        setHostedFieldEnabled(model);
        fillPaymentMethodsInfo(model);
        final Map<String, String> paymentsImagesURL = brainTreeCheckoutFacade.getAcceptedPaymentMethodImages();
        model.addAttribute(ACCEPTED_PAYMENTS_METHODS_IMAGES_URL, paymentsImagesURL);
        model.addAttribute(PAYPAL_MARK_BUTTON_CONFIG, payPalButtonConfigurationService.getFormattedProperty(MARK_BUTTON_CONFIG));
        model.addAttribute(DISABLE_MARK_FUNDING, brainTreeConfigService.getDisableFunding());
        final List<BraintreeLocalPaymentMethodsModel> listLocalPaymentInfoModel = brainTreePaymentFacade.getLocalPaymentMethods();
        model.addAttribute(LOCAL_PAYMENT_METHODS, listLocalPaymentInfoModel);
        model.addAttribute(VENMO_PROFILE_ID, brainTreeConfigService.getVenmoProfileId());
        model.addAttribute(CURRENCY_MERCHANT_ACCOUNT, brainTreeConfigService.getCurrencyMerchantAccountId());

        List<String> lpmids = new ArrayList<>();
        for (BraintreeLocalPaymentMethodsModel lpm : listLocalPaymentInfoModel)
        {
          lpmids.add(lpm.getCode());
        }
        ObjectMapper om = new ObjectMapper();
        try
        {
          model.addAttribute("lpmids", om.writeValueAsString(lpmids));
        }
        catch (IOException e)
        {
          LOG.error("Error during write value");
        }
        return SilentOrderPostPage;
      }

      else if (ControllerConstants.Views.Pages.MultiStepCheckout.AccountLayoutPage.equals(viewName))
      {
        // add BRAIN_TREE_PAYMENT_DATA for order view page
        final OrderData orderData = (OrderData) model.get(ORDER_DATA);
        if (orderData != null && BooleanUtils.isFalse(orderData.getIsExtendOrderPage()))
        {
          final String orderCode = orderData.getCode();
          if (isNotBlank(orderCode))
          {
            model.addAttribute(BRAIN_TREE_PAYMENT_DATA, brainTreePaymentFacade.getBrainTreePaymentInfoData(orderCode));
            return viewName;
          }
        }

        if (Boolean.FALSE.equals(brainTreeConfigService.getPayPalStandardEnabled())
            && Boolean.FALSE.equals(brainTreeConfigService.getHostedFieldEnabled()))
        {
          return viewName;
        }

        String clientToken = StringUtils.EMPTY;
        try
        {
          clientToken = brainTreeCheckoutFacade.generateClientToken();
        }
        catch (final AdapterException exception)
        {
          LOG.error("[View Handler] Error during token generation!");
        }
        model.addAttribute(CLIENT_TOKEN, clientToken);

        setBraintreeLocale(model);
        setHostedFieldEnabled(model);
        final PayPalConfigurationData payPalConfigurationData = brainTreeAccountFacade.getPayPalConfigurationData();
        boolean addPaymentShow = payPalConfigurationData.getSecure3d() && payPalConfigurationData.getIntent().equals(PAYPAL_INTENT_ORDER);
        model.addAttribute(ADD_PAYMENT_METHOD_SHOW, addPaymentShow);
        model.addAttribute(PAY_PAL_CONFIGURATION_DATA, payPalConfigurationData);
        final Map<String, String> paymentsImagesURL = brainTreeCheckoutFacade.getAcceptedPaymentMethodImages();
        model.addAttribute(ACCEPTED_PAYMENTS_METHODS_IMAGES_URL, paymentsImagesURL);
        model.addAttribute(VENMO_ENABLE, brainTreeConfigService.getVenmoEnabled());
        model.addAttribute(PAYMENT_INFOS, brainTreeUserFacade.getBrainTreeCCPaymentInfos(true));
        model.addAttribute(BILLING_AGREEMENT_DESCRIPTION, brainTreeConfigService.getBillingAgreementDescription());
        model.addAttribute(GOOGLE_PAY_MERCHANT_ID, brainTreeConfigService.getGooglePayMerchantId());
        model.addAttribute(GOOGLE_PAY_ENABLE, brainTreeConfigService.getGooglePayEnabled());
        model.addAttribute(GOOGLE_PAY_COUNTRY_CODE, brainTreeConfigService.getGooglePayCountryCode());
        return AccountLayoutPage;
      }

      else if (B2C_CHECKOUT_CONFIRMATION_LAYOUT_PAGE.equals(viewName))
      {
        final String orderCode = ((OrderData) model.get(ORDER_DATA)).getCode();
        if (isNotBlank(orderCode))
        {
          model.addAttribute(BRAIN_TREE_PAYMENT_DATA, brainTreePaymentFacade.getBrainTreePaymentInfoData(orderCode));
        }
        return viewName;
      }

      else if (ControllerConstants.Views.Pages.MultiStepCheckout.CheckoutSummaryPage.equals(viewName))
      {
        model.addAttribute(BRAIN_TREE_PAYMENT_DATA, brainTreePaymentFacade.getBrainTreePaymentInfoData());
        return CheckoutSummaryPage;
      }

      else if (B2C_CHECKOUT_CONFIRMATION_PAGE.equals(viewName))
      {
        final String orderCode = ((OrderData) model.get(ORDER_DATA)).getCode();
        if (isNotBlank(orderCode))
        {
          model.addAttribute(BRAIN_TREE_PAYMENT_DATA, brainTreePaymentFacade.getBrainTreePaymentInfoData(orderCode));
        }
        return CheckoutConfirmationPage;
      }

      else if (B2C_ADD_TO_CART_POPUP_PAGE.equals(viewName))
      {
        if (isPaymentMethodsAvailable(viewName))
        {
          fillPaymentMethodsInfo(model);
          setPayPalExpressEnabled(model);
          model.addAttribute(PAYPAL_BUTTON_CONFIG, payPalButtonConfigurationService.getFormattedProperty(MINI_CART_BUTTON_CONFIG));
          return BraintreeaddonControllerConstants.Views.Fragments.Cart.AddToCartPopup;
        }
        return viewName;
      }

      else if (B2C_CART_POPUP_PAGE.equals(viewName))
      {
        if (isPaymentMethodsAvailable(viewName))
        {
          fillPaymentMethodsInfo(model);
          setPayPalExpressEnabled(model);
          model.addAttribute(PAYPAL_BUTTON_CONFIG, payPalButtonConfigurationService.getFormattedProperty(MINI_CART_BUTTON_CONFIG));
          return BraintreeaddonControllerConstants.Views.Fragments.Cart.CartPopup;
        }
        return viewName;
      }
      else if (B2C_CHECKOUT_DELIVERY_ADDRESS_PAGE.equals(viewName)
          && getSessionService().getAttribute("braintree.general.error.shippingAddress") != null)
      {
        model.addAttribute("accErrorMsgs", getSessionService().getAttribute("braintree.general.error.shippingAddress"));
        getSessionService().removeAttribute("braintree.general.error.shippingAddress");
      }
      else if (ControllerConstants.Views.Pages.MultiStepCheckout.FallbackPage.equals(viewName))
      {
        String clientToken = StringUtils.EMPTY;
        try
        {
          clientToken = brainTreeCheckoutFacade.generateClientToken();
        }
        catch (final AdapterException exception)
        {
          LOG.error("[View Handler] Error during token generation!");
        }
        model.addAttribute(CLIENT_TOKEN, clientToken);
        return FallbackPage;
      }
    }
    catch (final Exception exception)
    {
      LOG.error(exception.getMessage(), exception);
    }
    return viewName;
  }



	private void setBraintreeLocale(final ModelMap model)
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
						.getSupportedLocaleByLanguage(getI18NService().getCurrentLocale().getLanguage());
				model.addAttribute(BRAINTREE_LOCALE, locale);
			}
		}
		catch (final Exception e)
		{
			model.addAttribute(BRAINTREE_LOCALE,
					BrainTreeSupportedLocaleConfig.getDefaultLocale(getI18NService().getCurrentLocale().getLanguage()));
		}
	}


	private void setPayPalExpressEnabled(final ModelMap model)
	{
		model.addAttribute(PAY_PAL_EXPRESS_ENABLE, brainTreeConfigService.getPayPalExpressEnabled());
	}

	private void setHostedFieldEnabled(final ModelMap model)
	{
		model.addAttribute(HOSTED_FIELDS_ENABLE, brainTreeConfigService.getHostedFieldEnabled());
		model.addAttribute(PAY_PAL_STANDARD_ENABLE, brainTreeConfigService.getPayPalStandardEnabled());
	}

	private void fillPaymentMethodsInfo(final ModelMap model)
	{
		String clientToken = StringUtils.EMPTY;
		boolean localPaymentsEnabled = brainTreeConfigService.getLocalPaymentsEnabled() &&
				brainTreeConfigService.getSettlementConfigParameter() &&
				(PAYPAL_INTENT_SALE.equalsIgnoreCase(brainTreeConfigService.getIntent()) ||
						PAYPAL_INTENT_CAPTURE.equalsIgnoreCase(brainTreeConfigService.getIntent()));

		try
		{
			clientToken = brainTreeCheckoutFacade.generateClientToken();
		}
		catch (final AdapterException exception)
		{
			LOG.error("[View Handler] Error during token generation!");
		}
		final PayPalCheckoutData payPalCheckoutData = brainTreeCheckoutFacade.getPayPalCheckoutData();
		setBraintreeLocale(model);
		model.addAttribute(CLIENT_TOKEN, clientToken);
		model.addAttribute(PAY_PAL_CHECKOUT_DATA, payPalCheckoutData);
		List<CCPaymentInfoData> brainTreeCCPaymentInfos = brainTreeUserFacade.getBrainTreeCCPaymentInfos(true);
		brainTreeCCPaymentInfos.forEach(brainTreePaymentFacade::setPaymentMethodNonce);
		model.addAttribute("payPalShouldBeSaved", !isPayPalAccountSaved(brainTreeCCPaymentInfos));
		model.addAttribute(PAYMENT_INFOS, brainTreePaymentFacade.getAvailablePayments(brainTreeCCPaymentInfos));
		model.addAttribute(APPLE_PAY_ENABLE, brainTreeConfigService.getApplePayEnabled());
		model.addAttribute(VENMO_ENABLE, brainTreeConfigService.getVenmoEnabled());
		model.addAttribute(LOCAL_PAYMENTS_ENABLED, localPaymentsEnabled);
		model.addAttribute(BILLING_AGREEMENT_DESCRIPTION, brainTreeConfigService.getBillingAgreementDescription());
		model.addAttribute(ENDPOINT_URL, brainTreeConfigService.getEndpointURL());
		model.addAttribute(GOOGLE_PAY_MERCHANT_ID, brainTreeConfigService.getGooglePayMerchantId());
		model.addAttribute(GOOGLE_PAY_ENABLE, brainTreeConfigService.getGooglePayEnabled());
		model.addAttribute(GOOGLE_PAY_COUNTRY_CODE, brainTreeConfigService.getGooglePayCountryCode());
		model.addAttribute(CREDIT_ENABLED, brainTreeConfigService.getCreditEnabled());
	}

	private boolean isPayPalAccountSaved(List<CCPaymentInfoData> savedPayments)
	{
		for (CCPaymentInfoData payment : savedPayments)
		{
			if (BraintreeConstants.PAYPAL_PAYMENT.equals(payment.getSubscriptionId()) || BraintreeConstants.PAY_PAL_EXPRESS_CHECKOUT
					.equals(payment.getSubscriptionId()))
				return true;
		}
		return false;
	}

	private boolean isPaymentMethodsAvailable(String viewName)
	{
		if (B2C_CART_POPUP_PAGE.equals(viewName) || B2C_ADD_TO_CART_POPUP_PAGE.equals(viewName))
		{
			return brainTreeConfigService.getPayPalExpressEnabled() || brainTreeConfigService.getApplePayEnabled()
					|| brainTreeConfigService.getGooglePayEnabled();
		}
		else if (AccountLayoutPage.equals(viewName))
		{
			return brainTreeConfigService.getPayPalStandardEnabled() || brainTreeConfigService.getHostedFieldEnabled()
					|| brainTreeConfigService.getVenmoEnabled();
		}
		else
		{
			return brainTreeConfigService.isOneOfPaymentMethodsEnabled();
		}
	}

	private void registerConfigChangeLister()
	{
		final ConfigIntf config = Registry.getMasterTenant().getConfig();
		configurationChangeListener = new BrainTreeConfigurationListener();
		config.registerConfigChangeListener(configurationChangeListener);
	}

	public I18NService getI18NService()
	{
		return i18NService;
	}

	public void setI18NService(final I18NService i18nService)
	{
		i18NService = i18nService;
	}

	public SessionService getSessionService()
	{
		return sessionService;
	}

	public void setSessionService(SessionService sessionService)
	{
		this.sessionService = sessionService;
	}
}
