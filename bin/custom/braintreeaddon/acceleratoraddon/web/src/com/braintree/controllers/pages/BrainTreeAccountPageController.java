package com.braintree.controllers.pages;

import com.braintree.exceptions.ResourceErrorMessage;
import com.braintree.facade.BrainTreeUserFacade;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.hybris.data.BrainTreeSubscriptionInfoData;
import com.braintree.model.BrainTreePaymentInfoModel;
import com.braintree.payment.validators.PaymentMethodValidator;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.Breadcrumb;
import de.hybris.platform.acceleratorstorefrontcommons.breadcrumb.ResourceBreadcrumbBuilder;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.AbstractPageController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.order.data.CCPaymentInfoData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.payment.AdapterException;
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
import java.io.IOException;
import java.util.List;

import static com.braintree.controllers.BraintreeaddonControllerConstants.CLIENT_TOKEN;
import static de.hybris.platform.util.localization.Localization.getLocalizedString;


@Controller
@RequestMapping("/my-account")
public class BrainTreeAccountPageController extends AbstractPageController
{
	protected static final Logger LOGGER = Logger.getLogger(BrainTreeAccountPageController.class);
	private static final String REDIRECT_TO_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/payment-details";
	private static final String REDIRECT_TO_ADD_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/add-payment-method";
	private static final String REDIRECT_TO_EDIT_PAYMENT_INFO_PAGE = REDIRECT_PREFIX + "/my-account/edit-payment-method";

	// CMS Pagesb
	private static final String ADD_EDIT_PAYMENT_METHOD_CMS_PAGE = "add-edit-payment-method";
	private static final String EDIT_PAYMENT_METHOD_CMS_PAGE = "edit-payment-method";

	@Resource(name = "userFacade")
	protected BrainTreeUserFacade userFacade;

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "accountBreadcrumbBuilder")
	private ResourceBreadcrumbBuilder accountBreadcrumbBuilder;

	@Resource(name = "paymentMethodValidator")
	private PaymentMethodValidator paymentMethodValidator;

	@RequestMapping(value = "/remove-payment-method-bt", method = RequestMethod.POST)
	@RequireHardLogIn
	public String removePaymentMethod(@RequestParam(value = "paymentInfoId") final String paymentInfoId,
                                      @RequestParam(value = "paymentMethodToken") final String paymentMethodNonce, final RedirectAttributes redirectAttributes)
			throws CMSItemNotFoundException
	{
		userFacade.unlinkCCPaymentInfo(paymentInfoId);
		userFacade.removeBTCCPaymentInfo(paymentMethodNonce);
		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				getLocalizedString("text.account.profile.paymentCart.remove"));

		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}


	@RequestMapping(value = "/edit-payment-method", method = RequestMethod.GET)
	@RequireHardLogIn
	public String editPaymentMethod(final Model model, @RequestParam(value = "paymentInfoId") final String paymentMethodId,
                                    @RequestParam(value = "cardholder", required = false) final String cardholder,
                                    @RequestParam(value = "errorMessage", required = false) final String errorMessage,
                                    @RequestParam(value = "expirationDate", required = false) final String expirationDate) throws CMSItemNotFoundException
	{
		final List<Breadcrumb> breadcrumbs = accountBreadcrumbBuilder.getBreadcrumbs(null);
		breadcrumbs.add(new Breadcrumb("/my-account/payment-details",
				getMessageSource().getMessage("text.account.paymentDetails", null, getI18nService().getCurrentLocale()), null));
		breadcrumbs.add(new Breadcrumb("#",getLocalizedString("text.account.paymentMethod.editPaymentMethod"), null));

		storeCmsPageInModel(model, getContentPageForLabelOrId(EDIT_PAYMENT_METHOD_CMS_PAGE));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(EDIT_PAYMENT_METHOD_CMS_PAGE));
		List<AddressData> addressBook = userFacade.getAddressBook();
		CCPaymentInfoData ccPaymentInfo = userFacade.getCCPaymentInfoForCode(paymentMethodId);

		if (errorMessage != null && !model.containsAttribute("accErrorMsgs"))
		{
			GlobalMessages.addErrorMessage(model, errorMessage);
		}

		model.addAttribute("errorMessage", cardholder);
		model.addAttribute("cardholder", cardholder);
		model.addAttribute("expirationDate", expirationDate);
		model.addAttribute("ccPaymentInfo", ccPaymentInfo);
		model.addAttribute("deliveryAddresses", addressBook);
		model.addAttribute("breadcrumbs", breadcrumbs);
		model.addAttribute("selectedPaymentMethodId", paymentMethodId);
		model.addAttribute("metaRobots", "noindex,nofollow");
		return getViewForPage(model);
	}

	@RequestMapping(value = "/edit-payment-method", method = RequestMethod.POST)
	@RequireHardLogIn
	public String editPaymentMethod(final Model model, @RequestParam(value = "paymentInfoId") final String paymentMethodId,
                                    @RequestParam(value = "billingAddressId") final String billingAddressId,
                                    @RequestParam(value = "cardholder") final String cardholder,
                                    @RequestParam(value = "expirationDate") final String expirationDate, @RequestParam(value = "cvv") final String cvv,
                                    final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		try
		{
			ResourceErrorMessage validationMessage = paymentMethodValidator.validate(cardholder, expirationDate, cvv);
			if (validationMessage != null && StringUtils.isNotBlank(validationMessage.getMessageKey()))
			{
				String localizedErrorMessage = getLocalizedString(validationMessage.getMessageKey());
				LOGGER.error("Failed to edit payment method. Error occurred while contacting external payment services.");
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);
				return redirectToEditPage(paymentMethodId, cardholder, expirationDate, redirectAttributes, localizedErrorMessage);
			}
			else
			{
				CCPaymentInfoData ccPaymentInfo = userFacade.getCCPaymentInfoForCode(paymentMethodId);

				List<AddressData> addressBook = userFacade.getAddressBook();
				AddressData addressData = getAddressForPaymentInfo(ccPaymentInfo, billingAddressId, addressBook);

				userFacade.editPaymentMethod(ccPaymentInfo, cardholder, expirationDate, cvv, addressData);
			}
		}
		catch (AdapterException e)
		{
			String localizedErrorMessage = getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.general")
					+ e.getMessage();
			LOGGER.error("Failed to edit payment method. Error occurred while contacting external payment services.", e);
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER, localizedErrorMessage);

			return redirectToEditPage(paymentMethodId, cardholder, expirationDate, redirectAttributes, localizedErrorMessage);
		}

		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}

	private String redirectToEditPage(final String paymentMethodId, final String cardholder, final String expirationDate,
                                      RedirectAttributes redirectAttributes, final String localizedErrorMessage)
	{
        redirectAttributes.addAttribute("paymentInfoId", paymentMethodId);
        redirectAttributes.addAttribute("cardholder", cardholder);
        redirectAttributes.addAttribute("expirationDate", expirationDate);
        redirectAttributes.addAttribute("errorMessage", localizedErrorMessage);
		return REDIRECT_TO_EDIT_PAYMENT_INFO_PAGE;
	}

	@RequestMapping(value = "/receive-address", method = RequestMethod.GET)
	@RequireHardLogIn
	@ResponseBody
	public String receiveAddress(@RequestParam(value = "selectedAddressCode", required = false) String selectedAddressCode)
			throws CMSItemNotFoundException, IOException
	{
		if (StringUtils.isNotBlank(selectedAddressCode))
		{
			final ObjectMapper mapper = new ObjectMapper();
			List<AddressData> addressBook = userFacade.getAddressBook();
			AddressData addressForPaymentInfo = getAddressForPaymentInfo(selectedAddressCode, addressBook);
			return mapper.writeValueAsString(addressForPaymentInfo);
		}
		return StringUtils.EMPTY;
	}

	@RequestMapping(value = "/add-payment-method", method = RequestMethod.GET)
	@RequireHardLogIn
	public String addPaymentMethod(final Model model, final String selectedAddressCode) throws CMSItemNotFoundException
	{

		final List<Breadcrumb> breadcrumbs = accountBreadcrumbBuilder.getBreadcrumbs(null);
		breadcrumbs.add(new Breadcrumb("/my-account/payment-details",
				getMessageSource().getMessage("text.account.paymentDetails", null, getI18nService().getCurrentLocale()), null));
		breadcrumbs.add(new Breadcrumb("#",
				getMessageSource().getMessage("text.account.profile.paymentMethod.add", null, getI18nService().getCurrentLocale()), null));

		storeCmsPageInModel(model, getContentPageForLabelOrId(ADD_EDIT_PAYMENT_METHOD_CMS_PAGE));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(ADD_EDIT_PAYMENT_METHOD_CMS_PAGE));
		setupAdditionalFields(model);
		final List<AddressData> addressBook = userFacade.getAddressBook();
		model.addAttribute("deliveryAddresses", addressBook);
		model.addAttribute("selectedAddressCode", selectedAddressCode);
		model.addAttribute("addressForm", new AddressForm());
		model.addAttribute("breadcrumbs", breadcrumbs);
		model.addAttribute("metaRobots", "noindex,nofollow");
		return getViewForPage(model);
	}

	private void setupAdditionalFields(final Model model)
	{
		String clientToken = StringUtils.EMPTY;

		try
		{
			clientToken = brainTreeCheckoutFacade.generateClientToken();
		}
		catch (final AdapterException exception)
		{
			LOGGER.error("[Brain Tree Controller] Error during token generation!");
		}

		model.addAttribute(CLIENT_TOKEN, clientToken);
	}

	@RequestMapping(value = "/add-payment-method", method = RequestMethod.POST)
	@RequireHardLogIn
	public String addPaymentMethod(@RequestParam(value = "bt_payment_method_nonce") final String nonce,
			@RequestParam(value = "payment_type") final String paymentProvider,
			@RequestParam(value = "paypal_email") final String payPalEmail,
			@RequestParam(value = "card_type") final String cardType,
			@RequestParam(value = "card_details") final String cardDetails,
			@RequestParam(value = "device_data") final String deviceData,
			@RequestParam(value = "liability_shifted") final String liabilityShifted,
			@RequestParam(value = "selectedAddressCode") final String selectedAddressCode,
			@RequestParam(value = "cardholder", required = false) final String cardholder,
			final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		if (StringUtils.isEmpty(nonce))
		{
			LOGGER.error("Failed to create payment method. Error occurred while contacting external payment services.");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.noNonce"));
			return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
		}

		final String storeInVault = getSiteConfigService().getProperty("braintree.store.in.vault");
		if (storeInVault != null && !Boolean.parseBoolean(storeInVault))
		{
			LOGGER.error(
					"Failed to create payment method. Store in vault is forbidden. For adding new payment methods enable store in vault.");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.forbidden"));
			return REDIRECT_TO_PAYMENT_INFO_PAGE;
		}

		final AddressData addressData = userFacade.getAddressForCode(selectedAddressCode);
		if (addressData == null)
		{
			LOGGER.error("Failed to create payment method. Error occurred while address selection");
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.noAddress"));
			return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
		}

		final BrainTreeSubscriptionInfoData subscriptionInfo = buildSubscriptionInfo(nonce, paymentProvider, cardDetails, cardType,
				payPalEmail, deviceData, liabilityShifted, addressData, cardholder);

		try
		{
			final BrainTreePaymentInfoModel paymentMethod = userFacade.addPaymentMethod(subscriptionInfo);
			if (paymentMethod == null)
			{
				LOGGER.error("Failed to create payment method. Error occurred while payment method creation");
				GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
						getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.error"));
				return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
			}
		}
		catch (final AdapterException e)
		{
			LOGGER.error("Failed to create payment method. Error occurred while payment method creation: " + e.getMessage());
			GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.ERROR_MESSAGES_HOLDER,
					getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.general.error"));
			return REDIRECT_TO_ADD_PAYMENT_INFO_PAGE;
		}

		GlobalMessages.addFlashMessage(redirectAttributes, GlobalMessages.CONF_MESSAGES_HOLDER,
				getLocalizedString("text.account.profile.paymentCart.addPaymentMethod.success"));
		return REDIRECT_TO_PAYMENT_INFO_PAGE;
	}

	private BrainTreeSubscriptionInfoData buildSubscriptionInfo(final String nonce, final String paymentProvider,
                                                                final String cardDetails, final String cardType, final String email, final String deviceData,
                                                                final String liabilityShifted, final AddressData addressData, final String cardholder)
	{
		final BrainTreeSubscriptionInfoData subscriptionInfo = new BrainTreeSubscriptionInfoData();
		subscriptionInfo.setPaymentProvider(paymentProvider);
		subscriptionInfo.setCardNumber(cardDetails);
		subscriptionInfo.setDeviceData(deviceData);
		subscriptionInfo.setCardType(cardType);
		subscriptionInfo.setNonce(nonce);
		subscriptionInfo.setEmail(email);
		subscriptionInfo.setAddressData(addressData);
		subscriptionInfo.setSavePaymentInfo(Boolean.TRUE);
		subscriptionInfo.setShouldBeSaved(Boolean.TRUE);
		subscriptionInfo.setCardholder(cardholder);
		if (StringUtils.isNotBlank(liabilityShifted))
		{
			subscriptionInfo.setLiabilityShifted(Boolean.valueOf(liabilityShifted));
		}

		return subscriptionInfo;
	}

	private AddressData getAddressForPaymentInfo(String selectedAddressCode, List<AddressData> addressBook)
	{
		if (StringUtils.isNotBlank(selectedAddressCode))
		{
			for (AddressData addressData : addressBook)
			{
				if (selectedAddressCode.equals(addressData.getId()))
				{
					return addressData;
				}
			}
		}
		return null;
	}

	private AddressData getAddressForPaymentInfo(CCPaymentInfoData ccPaymentInfo, String selectedAddressCode,
                                                 List<AddressData> addressBook)
	{
		AddressData addressForPaymentInfo = getAddressForPaymentInfo(selectedAddressCode, addressBook);
		if (addressForPaymentInfo == null)
		{
			return ccPaymentInfo.getBillingAddress();
		}
		return addressForPaymentInfo;
	}
}
