package com.braintree.controllers.pages;

import com.bl.core.utils.BlRentalDateUtils;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.braintree.configuration.service.BrainTreeConfigService;
import com.braintree.constants.ControllerConstants;
import com.braintree.controllers.form.BraintreePlaceOrderForm;
import com.braintree.customfield.service.CustomFieldsService;
import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import com.braintree.facade.impl.BrainTreePaymentFacadeImpl;
import com.braintree.hybris.data.BrainTreePaymentInfoData;
import de.hybris.platform.acceleratorservices.enums.CheckoutPciOptionEnum;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.PreValidateCheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.checkout.steps.CheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.checkout.steps.AbstractCheckoutStepController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.PlaceOrderForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commercefacades.order.data.OrderEntryData;
import de.hybris.platform.commercefacades.product.ProductOption;
import de.hybris.platform.commercefacades.product.data.ProductData;
import de.hybris.platform.commerceservices.order.CommerceCartModificationException;
import de.hybris.platform.order.InvalidCartException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


@Controller
@RequestMapping(value = "checkout/multi/summary/braintree")
public class BrainTreeSummaryCheckoutStepController extends AbstractCheckoutStepController
{
	private final static Logger LOG = Logger.getLogger(BrainTreeSummaryCheckoutStepController.class);

	@Resource(name = "brainTreePaymentFacadeImpl")
	private BrainTreePaymentFacadeImpl brainTreePaymentFacade;

	private final static String SUMMARY = "summary";

	@Resource(name = "customFieldsService")
	private CustomFieldsService customFieldsService;

	@Resource(name = "brainTreeCheckoutFacade")
	private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

	@Resource(name = "brainTreeConfigService")
	private BrainTreeConfigService brainTreeConfigService;

	@Resource(name = "checkoutFacade")
	private BlCheckoutFacade blCheckoutFacade;
	
	@ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
	private RentalDateDto getRentalsDuration()
	{
		return BlRentalDateUtils.getRentalsDuration();
	}

	public static final String REDIRECT_PREFIX = "redirect:";

	@RequestMapping(value = "/view", method = RequestMethod.GET)
	@RequireHardLogIn
	@Override
	@PreValidateCheckoutStep(checkoutStep = SUMMARY)
	public String enterStep(final Model model, final RedirectAttributes redirectAttributes)
			throws CMSItemNotFoundException, CommerceCartModificationException
	{
		final List<String> removedGiftCardCodeList = blCheckoutFacade.recalculateCartForGiftCard();
		if(CollectionUtils.isNotEmpty(removedGiftCardCodeList)) {
			return redirectToPaymentPageOnGiftCardRemove(redirectAttributes, removedGiftCardCodeList);
		}
		final CartData cartData = getCheckoutFacade().getCheckoutCart();
		if (cartData.getEntries() != null && !cartData.getEntries().isEmpty())
		{
			for (final OrderEntryData entry : cartData.getEntries())
			{
				final String productCode = entry.getProduct().getCode();
				final ProductData product = getProductFacade().getProductForCodeAndOptions(productCode,
						Arrays.asList(ProductOption.BASIC, ProductOption.PRICE));
				entry.setProduct(product);
			}
		}
		BrainTreePaymentInfoData brainTreePaymentInfoData = brainTreePaymentFacade.getBrainTreePaymentInfoData();
		model.addAttribute("cartData", cartData);
		model.addAttribute("allItems", cartData.getEntries());
		model.addAttribute("deliveryAddress", cartData.getDeliveryAddress());
		model.addAttribute("deliveryMode", cartData.getDeliveryMode());
		model.addAttribute("paymentInfo", cartData.getPaymentInfo());

        model.addAttribute("shipsFromPostalCode", "");

		// Only request the security code if the SubscriptionPciOption is set to Default.
		final boolean requestSecurityCode = (CheckoutPciOptionEnum.DEFAULT
				.equals(getCheckoutFlowFacade().getSubscriptionPciOption()));
		model.addAttribute("requestSecurityCode", Boolean.valueOf(requestSecurityCode));

		Map<String, String> defaultCustomFields = customFieldsService.getDefaultCustomFieldsMap();

		final BraintreePlaceOrderForm placeOrderForm = new BraintreePlaceOrderForm();
		placeOrderForm.setCustomFields(defaultCustomFields);
		model.addAttribute("placeOrderForm", placeOrderForm);

		storeCmsPageInModel(model, getContentPageForLabelOrId(MULTI_CHECKOUT_SUMMARY_CMS_PAGE_LABEL));
		setUpMetaDataForContentPage(model, getContentPageForLabelOrId(MULTI_CHECKOUT_SUMMARY_CMS_PAGE_LABEL));
		model.addAttribute(WebConstants.BREADCRUMBS_KEY,
				getResourceBreadcrumbBuilder().getBreadcrumbs("checkout.multi.summary.breadcrumb"));
		model.addAttribute("metaRobots", "noindex,nofollow");
		setCheckoutStepLinksForModel(model, getCheckoutStep());
		return ControllerConstants.Views.Pages.MultiStepCheckout.CheckoutSummaryPage;
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
			redirectAttributes.addFlashAttribute(BlControllerConstants.GIFT_CARD_REMOVE, removeGiftCardMessage);
			} catch (final Exception exception) {
			BlLogger.logFormatMessageInfo(LOG, Level.ERROR,
					"Error occurred while adding message to redirect attribute for removed gift card",
					exception);
		}
		return REDIRECT_PREFIX + BlControllerConstants.PAYMENT_METHOD_CHECKOUT_URL;
	}

	@RequestMapping(value = "/placeOrder")
	@RequireHardLogIn
	public String placeOrder(@ModelAttribute("placeOrderForm") final BraintreePlaceOrderForm placeOrderForm, final Model model,
                             final HttpServletRequest request, final RedirectAttributes redirectModel)
					throws CMSItemNotFoundException, InvalidCartException, CommerceCartModificationException
	{
		final List<String> removedGiftCardCodeList = blCheckoutFacade.recalculateCartForGiftCard();
		if (CollectionUtils.isNotEmpty(removedGiftCardCodeList)) {
			return redirectToPaymentPageOnGiftCardRemove(redirectModel, removedGiftCardCodeList);
		}

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
		        LOG.error("placeOrderForm.getShippingPostalCode: " + placeOrderForm.getShipsFromPostalCode());
                LOG.error("what is this ? placeOrderForm.getSecurityCode: " + placeOrderForm.getSecurityCode());	
                LOG.error("what is this ? getMergedCustomFields(placeOrderForm.getCustomFields): " + getMergedCustomFields(	
                                placeOrderForm.getCustomFields()));

		final OrderData orderData;
		try
		{
			LOG.error("getCheckoutFacade: " + getCheckoutFacade());

			brainTreeCheckoutFacade.storeIntentToCart();
			brainTreeCheckoutFacade.storeCustomFieldsToCart(getMergedCustomFields(placeOrderForm.getCustomFields()));
			brainTreeCheckoutFacade.storeShipsFromPostalCodeToCart(placeOrderForm.getShipsFromPostalCode());

			orderData = getCheckoutFacade().placeOrder();
			LOG.error("Order has been placed, number/code: " + orderData.getCode());
		}
		catch (final Exception e)
		{
			LOG.error("Failed to place Order, message: " + e.getMessage(), e);
			GlobalMessages.addErrorMessage(model, "checkout.placeOrder.failed");
			return enterStep(model, redirectModel);
		}

		return redirectToOrderConfirmationPage(orderData);
	}

	private Map<String, String> getMergedCustomFields (Map<String, String> customFieldsFromUI)
	{
		Map<String, String> customFields = customFieldsService.getDefaultCustomFieldsMap();

		for (String key: customFieldsFromUI.keySet()) {
			customFields.put(key, customFieldsFromUI.get(key));
		}

		return customFields;
	}

	protected boolean validateOrderForm(final PlaceOrderForm placeOrderForm, final Model model)
	{
		final String securityCode = placeOrderForm.getSecurityCode();
		boolean invalid = false;

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

		if (getCheckoutFlowFacade().hasNoPaymentInfo())
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

	@RequestMapping(value = "/back", method = RequestMethod.GET)
	@RequireHardLogIn
	@Override
	public String back(final RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().previousStep();
	}

	@RequestMapping(value = "/next", method = RequestMethod.GET)
	@RequireHardLogIn
	@Override
	public String next(final RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().nextStep();
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
}
