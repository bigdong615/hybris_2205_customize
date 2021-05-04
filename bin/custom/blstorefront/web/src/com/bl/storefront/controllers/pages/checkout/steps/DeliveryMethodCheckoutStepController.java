/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages.checkout.steps;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.core.enums.AddressTypeEnum;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.facades.shipping.data.BlPartnerPickUpStoreData;
import com.bl.facades.shipping.data.BlShippingGroupData;
import com.bl.facades.shipping.impl.DefaultBlCheckoutFacade;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.bl.storefront.controllers.pages.checkout.BlCheckoutStepController;
import com.bl.storefront.forms.BlAddressForm;
import com.bl.storefront.forms.BlPickUpByForm;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.PreValidateCheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.PreValidateQuoteCheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.checkout.steps.CheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.checkout.steps.AbstractCheckoutStepController;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.order.data.CartData;
import com.bl.storefront.controllers.ControllerConstants;

import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import de.hybris.platform.enumeration.EnumerationService;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.Collection;


@Controller
@RequestMapping(value = "/checkout/multi/delivery-method")
public class DeliveryMethodCheckoutStepController extends AbstractCheckoutStepController implements BlCheckoutStepController
{
	private static final String DELIVERY_METHOD = "delivery-method";
	private static final String DeliveryOrPickupPage = "deliveryOrPickup";

	@Resource(name = "defaultBlCheckoutFacade")
	private BlCheckoutFacade blCheckoutFacade;

	@Resource(name="enumerationService")
	private EnumerationService enumerationService;

	@GetMapping(value = "/chooseShipping")
	@RequireHardLogIn
	@Override
	@PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
	public String getAllShippingGroups(final Model model, final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException {
		final CartData cartData = getCheckoutFacade().getCheckoutCart();
		model.addAttribute("cartData", cartData);
		model.addAttribute("shippingGroup", getBlCheckoutFacade().getAllShippingGroups());
		model.addAttribute("deliveryAddresses", getUserFacade().getAddressBook());
		model.addAttribute("partnerPickUpLocation", getBlCheckoutFacade().getAllPartnerPickUpStore());
		model.addAttribute("addressForm", new BlAddressForm());
		model.addAttribute("blPickUpByForm", new BlPickUpByForm());
		model.addAttribute("regions", getI18NFacade().getRegionsForCountryIso("US"));
		this.prepareDataForPage(model);
		final ContentPageModel deliveryOrPickUpPage = getContentPageForLabelOrId(DeliveryOrPickupPage);
		storeCmsPageInModel(model, deliveryOrPickUpPage);
		setUpMetaDataForContentPage(model, deliveryOrPickUpPage);
		//setCheckoutStepLinksForModel(model, getCheckoutStep());
		return ControllerConstants.Views.Pages.MultiStepCheckout.DeliveryOrPickupPage;
	}

	@GetMapping(value = "/chooseShippingDelivery")
	@RequireHardLogIn
	@Override
	@PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
	@ResponseBody
	public Collection<? extends DeliveryModeData> getSupportedDeliveryModes(final Model model, final RedirectAttributes redirectAttributes,
																			@RequestParam(value = "shippingGroup", defaultValue = "")
																			final String shippingGroup,
																			@RequestParam(value = "partnerZone", defaultValue = "")
																			final String partnerZone,
																			@RequestParam(value = "pinCode", defaultValue = "")
																			final String pinCode) {
		final CartData cartData = getCheckoutFacade().getCheckoutCart();
		final Collection<? extends DeliveryModeData> deliveryModes = getBlCheckoutFacade().getSupportedDeliveryModes(
				shippingGroup, pinCode, partnerZone);
		model.addAttribute("cartData", cartData);
		model.addAttribute("deliveryMethods", deliveryModes);
		return deliveryModes;
	}

	@GetMapping(value = "/checkValidZip")
	@RequireHardLogIn
	@PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
	@ResponseBody
	public boolean checkValidityOfZipCode(final Model model, final RedirectAttributes redirectAttributes,
										 @RequestParam(value = "pinCode", defaultValue = "") final String pinCode) {
		return getBlCheckoutFacade().checkPartnerPickCodeValidity(pinCode);
	}

	/**
	 * This method gets called when the "Use Selected Delivery Method" button is clicked. It sets the selected delivery
	 * mode on the checkout facade and reloads the page highlighting the selected delivery Mode.
	 *
	 * @param selectedDeliveryMethod
	 *           - the id of the delivery mode.
	 * @return - a URL to the page to load.
	 */
	@PostMapping(value = "/saveDeliveryMethod")
	@RequireHardLogIn
	public String saveDeliveryMethodOnCart(@RequestParam("delivery_method") final String selectedDeliveryMethod)
	{
		if (StringUtils.isNotEmpty(selectedDeliveryMethod))
		{
			getBlCheckoutFacade().setDeliveryMethod(selectedDeliveryMethod);
		}
		return getCheckoutStep().nextStep();
	}

	@GetMapping(value = "/choosePartner")
	@RequireHardLogIn
	@Override
	@PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
	public Collection<BlPartnerPickUpStoreData> getAllPartnerPickUpStore() {
		return getBlCheckoutFacade().getAllPartnerPickUpStore();
	}

	@GetMapping(value = "/choose")
	@RequireHardLogIn
	@Override
	@PreValidateQuoteCheckoutStep
	@PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
	public String enterStep(final Model model, final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException
	{
		// Try to set default delivery mode
		getCheckoutFacade().setDeliveryModeIfAvailable();

		final CartData cartData = getCheckoutFacade().getCheckoutCart();
		model.addAttribute("cartData", cartData);
		model.addAttribute("deliveryMethods", getCheckoutFacade().getSupportedDeliveryModes());
		this.prepareDataForPage(model);
		final ContentPageModel multiCheckoutSummaryPage = getContentPageForLabelOrId(MULTI_CHECKOUT_SUMMARY_CMS_PAGE_LABEL);
		storeCmsPageInModel(model, multiCheckoutSummaryPage);
		setUpMetaDataForContentPage(model, multiCheckoutSummaryPage);
		model.addAttribute(WebConstants.BREADCRUMBS_KEY,
				getResourceBreadcrumbBuilder().getBreadcrumbs("checkout.multi.deliveryMethod.breadcrumb"));
		model.addAttribute("metaRobots", "noindex,nofollow");
		setCheckoutStepLinksForModel(model, getCheckoutStep());

		return ControllerConstants.Views.Pages.MultiStepCheckout.ChooseDeliveryMethodPage;
	}

	/**
	 * This method gets called when the "Use Selected Delivery Method" button is clicked. It sets the selected delivery
	 * mode on the checkout facade and reloads the page highlighting the selected delivery Mode.
	 *
	 * @param selectedDeliveryMethod
	 *           - the id of the delivery mode.
	 * @return - a URL to the page to load.
	 */
	@GetMapping(value = "/select")
	@RequireHardLogIn
	public String doSelectDeliveryMode(@RequestParam("delivery_method") final String selectedDeliveryMethod)
	{
		if (StringUtils.isNotEmpty(selectedDeliveryMethod))
		{
			getCheckoutFacade().setDeliveryMode(selectedDeliveryMethod);
		}

		return getCheckoutStep().nextStep();
	}

	@GetMapping(value = "/back")
	@RequireHardLogIn
	@Override
	public String back(final RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().previousStep();
	}

	@GetMapping(value = "/next")
	@RequireHardLogIn
	@Override
	public String next(final RedirectAttributes redirectAttributes)
	{
		return getCheckoutStep().nextStep();
	}

	protected CheckoutStep getCheckoutStep()
	{
		return getCheckoutStep(DELIVERY_METHOD);
	}

	public BlCheckoutFacade getBlCheckoutFacade() {
		return blCheckoutFacade;
	}

	public void setBlCheckoutFacade(BlCheckoutFacade blCheckoutFacade) {
		this.blCheckoutFacade = blCheckoutFacade;
	}
}
