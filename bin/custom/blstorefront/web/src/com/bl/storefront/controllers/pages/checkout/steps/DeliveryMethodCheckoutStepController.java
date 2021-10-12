/*
 * Copyright (c) 2019 SAP SE or an SAP affiliate company. All rights reserved.
 */
package com.bl.storefront.controllers.pages.checkout.steps;

import com.bl.constants.BlDeliveryModeLoggingConstants;
import com.bl.constants.BlInventoryScanLoggingConstants;
import com.bl.core.constants.BlCoreConstants;
import com.bl.core.datepicker.BlDatePickerService;
import com.bl.core.enums.AddressTypeEnum;
import com.bl.core.model.GiftCardModel;
import com.bl.core.services.blackout.BlBlackoutDateService;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.utils.BlRentalDateUtils;
import com.bl.core.utils.BlReplaceMentOrderUtils;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.giftcard.BlGiftCardFacade;
import com.bl.facades.locator.data.UpsLocatorResposeData;
import com.bl.facades.product.data.RentalDateDto;
import com.bl.facades.shipping.BlCheckoutFacade;
import com.bl.facades.shipping.data.BlPartnerPickUpStoreData;
import com.bl.facades.ups.address.data.AVSResposeData;
import com.bl.storefront.controllers.ControllerConstants;
import com.bl.storefront.controllers.pages.BlControllerConstants;
import com.bl.storefront.controllers.pages.checkout.BlCheckoutStepController;
import com.bl.storefront.forms.BlAddressForm;
import com.bl.storefront.forms.BlPickUpByForm;
import com.bl.storefront.util.BlAddressDataUtil;
import com.braintree.facade.BrainTreeUserFacade;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.PreValidateCheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.PreValidateQuoteCheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.checkout.steps.CheckoutStep;
import de.hybris.platform.acceleratorstorefrontcommons.checkout.steps.validation.ValidationResults;
import de.hybris.platform.acceleratorstorefrontcommons.constants.WebConstants;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.pages.checkout.steps.AbstractCheckoutStepController;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;
import de.hybris.platform.acceleratorstorefrontcommons.forms.AddressForm;
import de.hybris.platform.acceleratorstorefrontcommons.forms.VoucherForm;
import de.hybris.platform.cms2.exceptions.CMSItemNotFoundException;
import de.hybris.platform.cms2.model.pages.ContentPageModel;
import de.hybris.platform.commercefacades.address.data.AddressVerificationResult;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.DeliveryModeData;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commerceservices.address.AddressVerificationDecision;
import de.hybris.platform.core.model.c2l.CountryModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.deliveryzone.model.ZoneDeliveryModeModel;
import de.hybris.platform.store.services.BaseStoreService;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;


@Controller
@RequestMapping(value = "/checkout/multi/delivery-method")
public class DeliveryMethodCheckoutStepController extends AbstractCheckoutStepController implements BlCheckoutStepController {
    private static final String DELIVERY_METHOD = "delivery-method";
    private static final String DELIVERY_OR_PICKUP = "deliveryOrPickup";
    private static final String SHOW_SAVE_TO_ADDRESS_BOOK_ATTR = "showSaveToAddressBook";
    private static final String CART_DATA = "cartData";
    private static final String SUCCESS = "SUCCESS";
    private static final String REDIRECT_CART_URL = REDIRECT_PREFIX + "/cart";

    @Resource(name = "checkoutFacade")
    private BlCheckoutFacade checkoutFacade;

    @Resource(name = "blAddressDataUtil")
    private BlAddressDataUtil addressDataUtil;

    @Resource(name = "baseStoreService")
    private BaseStoreService baseStoreService;

    @Resource(name = "cartService")
    private BlCartService blCartService;

    @Resource(name = "blGiftCardFacade")
    private BlGiftCardFacade blGiftCardFacade;

    @Resource(name ="cartFacade")
    private BlCartFacade blCartFacade;
    
    @Resource(name = "blDatePickerService")
    private BlDatePickerService blDatePickerService;
    
    @Resource(name = "blBlackoutDateService")
    private BlBlackoutDateService blBlackoutDateService;

    @Resource(name = "userFacade")
    private BrainTreeUserFacade brainTreeUserFacade;
    
    @ModelAttribute(name = BlControllerConstants.RENTAL_DATE)
 	 private RentalDateDto getRentalsDuration() 
 	 {
 		 return BlRentalDateUtils.getRentalsDuration();
 	 }

    @GetMapping(value = "/chooseShipping")
    @RequireHardLogIn
    @Override
    @PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
    public String getAllShippingGroups(final Model model, final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException {
		 final RentalDateDto rentalDateDto = blDatePickerService.getRentalDatesFromSession();
		 if (blBlackoutDateService.checkForBlackoutDate(rentalDateDto))
		 {
			 return REDIRECT_CART_URL;
		 }
   	 model.addAttribute("pageType",BlControllerConstants.SHIPPING_PAGE);
        CartModel cartModel = blCartService.getSessionCart();
        if (cartModel != null) {
            List<GiftCardModel> giftCardModelList = cartModel.getGiftCard();
            if (CollectionUtils.isNotEmpty(giftCardModelList)) {
                blGiftCardFacade.removeAppliedGiftCardFromCartOrShippingPage(cartModel, giftCardModelList);
                model.addAttribute(BlControllerConstants.IS_GIFT_CARD_REMOVE, true);
            }
            final ZoneDeliveryModeModel zoneDeliveryModeData = (ZoneDeliveryModeModel) cartModel.getDeliveryMode();
            if(zoneDeliveryModeData != null) {
                model.addAttribute("shippingMethod", zoneDeliveryModeData.getShippingGroup().getCode()+ BlCoreConstants.HYPHEN
                        +zoneDeliveryModeData.getCode());
            }
        }
        final CartData cartData = getCheckoutFacade().getCheckoutCart();
        model.addAttribute(CART_DATA, cartData);

        if((boolean) getSessionService().getAttribute(BlInventoryScanLoggingConstants.IS_PAYMENT_PAGE_VISITED)) {
            model.addAttribute("previousPage", Boolean.TRUE);
        }

        model.addAttribute("shippingGroup", getCheckoutFacade().getAllShippingGroups());
        model.addAttribute("deliveryAddresses", brainTreeUserFacade.getShippingAddressBook());
        model.addAttribute("partnerPickUpLocation", getCheckoutFacade().getAllPartnerPickUpStore());
        model.addAttribute("addressForm", new BlAddressForm());
        model.addAttribute("blPickUpByForm", new BlPickUpByForm());
        final Collection<CountryModel> countries = baseStoreService.getCurrentBaseStore().getDeliveryCountries();
        if(CollectionUtils.isNotEmpty(countries)) {
            model.addAttribute("regions", getI18NFacade().getRegionsForCountryIso(countries.iterator().next().getIsocode()));
        }
        this.prepareDataForPage(model);
        final ContentPageModel deliveryOrPickUpPage = getContentPageForLabelOrId(DELIVERY_OR_PICKUP);
        storeCmsPageInModel(model, deliveryOrPickUpPage);
        setUpMetaDataForContentPage(model, deliveryOrPickUpPage);
        if(Boolean.TRUE.equals(cartData.getIsRentalCart())){
            model.addAttribute(BlCoreConstants.BL_PAGE_TYPE, BlCoreConstants.RENTAL_SUMMARY_DATE);
        }
        if(BooleanUtils.isTrue(BlReplaceMentOrderUtils.isReplaceMentOrder()) && Objects.nonNull(cartModel) &&
            Objects.nonNull(cartModel.getReturnRequestForOrder())){
            model.addAttribute("isReplacementOrderCart" , true);
        }

        model.addAttribute(BlControllerConstants.VOUCHER_FORM, new VoucherForm());


        if(null != getSessionService().getAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION) && BooleanUtils.isTrue(getSessionService().getAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION))){
            model.addAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION , true);
            getSessionService().removeAttribute(BlControllerConstants.IS_AVALARA_EXCEPTION);
        }
        return ControllerConstants.Views.Pages.MultiStepCheckout.DeliveryOrPickupPage;
    }

    /**
     * javadoc
     * This method will send address for back traversal scenario
     * @param model model
     * @param redirectAttributes attr
     * @return AddressData
     */
    @GetMapping(value = "/checkAddressWithCartAddress")
    @RequireHardLogIn
    @PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
    @ResponseBody
    public AddressData checkAddressWithCartAddress(final Model model, final RedirectAttributes redirectAttributes) {
        final AddressData cartAddressData = getCheckoutFacade().getCheckoutCart() != null ? getCheckoutFacade().getCheckoutCart()
                .getDeliveryAddress() : null;
        if(cartAddressData != null) {
            final List<AddressData> addressData = getUserFacade().getAddressBook();
            for(final AddressData address : addressData) {
                if(cartAddressData.getId().equals(address.getId())) {
                    return address;
                }
            }
        }
        return cartAddressData;
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
                                                                                final String partnerZone) {
        final CartData cartData = getCheckoutFacade().getCheckoutCart();

        boolean isPayByCustomer = true;

        if(BooleanUtils.isTrue(BlReplaceMentOrderUtils.isReplaceMentOrder()) &&
            Objects.nonNull(blCartService.getSessionCart().getReturnRequestForOrder())) {
                isPayByCustomer = false;
                model.addAttribute("isReplacementOrderCart", true);
        }
        Collection<? extends DeliveryModeData> deliveryModes = getCheckoutFacade()
                .getSupportedDeliveryModes(
                    shippingGroup, partnerZone, isPayByCustomer);
            model.addAttribute(CART_DATA, cartData);
            model.addAttribute("deliveryMethods", deliveryModes);
        return deliveryModes;
    }

    @GetMapping(value = "/checkValidZip")
    @RequireHardLogIn
    @PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
    @ResponseBody
    public UpsLocatorResposeData checkValidityOfZipCode(final Model model, final RedirectAttributes redirectAttributes,
                                                        @RequestParam(value = "pinCode", defaultValue = "") final String pinCode) {
        return getCheckoutFacade().checkPartnerPickCodeValidity(pinCode);
    }

    @GetMapping(value = "/checkValidZipFromFedexService")
    @RequireHardLogIn
    @PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
    @ResponseBody
    public boolean checkValidityOfZipCodeFromFedexService(final Model model, final RedirectAttributes redirectAttributes,
                                                          @RequestParam(value = "pinCode", defaultValue = "") final String pinCode,
                                                          @RequestParam(value = "shippingGroup", defaultValue = "") final String shippingGroup) {
        if (BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY.equals(shippingGroup)) {
            return getCheckoutFacade().checkSFOrNYCPinCodeValidity(pinCode, BlDeliveryModeLoggingConstants.SF);
        } else {
            return getCheckoutFacade().checkSFOrNYCPinCodeValidity(pinCode, BlDeliveryModeLoggingConstants.NYC);
        }
    }

    @GetMapping(value = "/choosePartner")
    @RequireHardLogIn
    @Override
    @PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
    public Collection<BlPartnerPickUpStoreData> getAllPartnerPickUpStore() {
        return getCheckoutFacade().getAllPartnerPickUpStore();
    }

    @GetMapping(value = "/choose")
    @RequireHardLogIn
    @Override
    @PreValidateQuoteCheckoutStep
    @PreValidateCheckoutStep(checkoutStep = DELIVERY_METHOD)
    public String enterStep(final Model model, final RedirectAttributes redirectAttributes) throws CMSItemNotFoundException {
        // Try to set default delivery mode
        getCheckoutFacade().setDeliveryModeIfAvailable();

        final CartData cartData = getCheckoutFacade().getCheckoutCart();
        model.addAttribute(CART_DATA, cartData);
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
     * @param selectedDeliveryMethod - the id of the delivery mode.
     * @return - a URL to the page to load.
     */
    @GetMapping(value = "/select")
    @RequireHardLogIn
    @ResponseBody
    public String doSelectDeliveryMode(@RequestParam("delivery_method") final String selectedDeliveryMethod,
                                       @RequestParam("internalStoreAddress") final boolean internalStoreAddress) {
   	 if (StringUtils.isNotEmpty(selectedDeliveryMethod)) {
     	  return getCheckoutFacade().setDeliveryMode(selectedDeliveryMethod, internalStoreAddress)
     			  ? BlControllerConstants.SUCCESS : BlControllerConstants.ERROR;
       }
       return BlControllerConstants.ERROR;
    }

    @PostMapping(value = "/addPickUpDetails")
    @RequireHardLogIn
    @Override
    @ResponseBody
    public String savePickUpByFormOnCart(@RequestBody final BlPickUpByForm blPickUpByForm, final BindingResult bindingResult,
                                         final Model model, final RedirectAttributes redirectModel) {
        return getCheckoutFacade().savePickUpInfoOnCart(blPickUpByForm);
    }

    @PostMapping(value = "/removeDeliveryDetails")
    @RequireHardLogIn
    @Override
    @ResponseBody
    public String removeDeliveryDetailsFromCart() {
        return getCheckoutFacade().removeDeliveryDetails();
    }

    @PostMapping(value = "/avsCheck")
    @RequireHardLogIn
    @ResponseBody
    public AVSResposeData getAVSResponse(@RequestBody final BlAddressForm addressForm, final BindingResult bindingResult,
                                         final Model model, final RedirectAttributes redirectModel) throws CMSItemNotFoundException {
        getAddressValidator().validate(addressForm, bindingResult);
        if (bindingResult.hasErrors()) {
            final AVSResposeData avsResposeData = new AVSResposeData();
            avsResposeData.setErrorDescription("address.error.formentry.invalid");
            return avsResposeData;
        }
        return getCheckoutFacade().getAVSResponse(addressDataUtil.convertToAddressData(addressForm));
    }

    @PostMapping(value = "/add")
    @RequireHardLogIn
    @ResponseBody
    public String add(@RequestBody final BlAddressForm addressForm, final BindingResult bindingResult, final Model model,
                      final RedirectAttributes redirectModel) throws CMSItemNotFoundException {
        getAddressValidator().validate(addressForm, bindingResult);
        populateCommonModelAttributes(model, getCheckoutFacade().getCheckoutCart(), addressForm);
        if (bindingResult.hasErrors()) {
            GlobalMessages.addErrorMessage(model, "address.error.formentry.invalid");
            return ControllerConstants.Views.Pages.MultiStepCheckout.DeliveryOrPickupPage;
        }
        final AddressData newAddress = addressDataUtil.convertToAddressData(addressForm);
        processAddressVisibilityAndDefault(addressForm, newAddress);

        // Verify the address data.
        final AddressVerificationResult<AddressVerificationDecision> verificationResult = getAddressVerificationFacade()
                .verifyAddressData(newAddress);
        final boolean addressRequiresReview = getAddressVerificationResultHandler().handleResult(verificationResult, newAddress,
                model, redirectModel, bindingResult, getAddressVerificationFacade().isCustomerAllowedToIgnoreAddressSuggestions(),
                "checkout.multi.address.updated");

        if (addressRequiresReview) {
            return ControllerConstants.Views.Pages.MultiStepCheckout.DeliveryOrPickupPage;
        }

//        if(BlDeliveryModeLoggingConstants.UPS.equals(newAddress.getLastName())) {
//            getCheckoutFacade().setUPSAddressOnCartForIam(newAddress);
//        }

        getUserFacade().addAddress(newAddress);
        final AddressData previousSelectedAddress = getCheckoutFacade().getCheckoutCart().getDeliveryAddress();
        // Set the new address as the selected checkout delivery address
        getCheckoutFacade().setDeliveryAddress(newAddress);
        if (previousSelectedAddress != null && !previousSelectedAddress.isVisibleInAddressBook()) { // temporary address should be removed
            getUserFacade().removeAddress(previousSelectedAddress);
        }

        return SUCCESS;
    }

    /**
     * This method gets called when the "Use this Address" button is clicked. It sets the selected delivery address on
     * the checkout facade - if it has changed, and reloads the page highlighting the selected delivery address.
     *
     * @param selectedAddressCode - the id of the delivery address.
     * @return - a URL to the page to load.
     */
    @GetMapping(value = "/selectAddress")
    @RequireHardLogIn
    @ResponseBody
    public String doSelectDeliveryAddress(@RequestParam("selectedAddressCode") final String selectedAddressCode,
                                          @RequestParam("shippingGroup") final String shippingGroup,
                                          @RequestParam("deliveryMode") final String deliveryMode,
                                          @RequestParam("rushZip") final String rushZip,
                                          @RequestParam("businessType") final boolean businessType,
                                          final RedirectAttributes redirectAttributes) {
        final ValidationResults validationResults = getCheckoutStep().validate(redirectAttributes);
        if (getCheckoutStep().checkIfValidationErrors(validationResults)) {
            return getCheckoutStep().onValidation(validationResults);
        }

        if (StringUtils.isNotBlank(selectedAddressCode)) {
            final AddressData selectedAddressData = getCheckoutFacade().getDeliveryAddressForCode(selectedAddressCode);
            if (selectedAddressData != null) {
                final String addressType = selectedAddressData.getAddressType();
                final String pinCode = selectedAddressData.getPostalCode();
                String pinError = checkErrorIfAnyBeforeSavingAddress(shippingGroup, businessType, rushZip, addressType, pinCode);
                if (pinError != null) {
                    return pinError;
                }
                setDeliveryAddress(selectedAddressData);
            }
        }
        return SUCCESS;
    }

    /**
     *
     * @param shippingGroup name
     * @param businessType yes/no
     * @param rushZip if rush delivery
     * @param addressType business/not
     * @param pinCode if rush
     * @return
     */
    private String checkErrorIfAnyBeforeSavingAddress(final String shippingGroup, final boolean businessType,
                                                      final String rushZip, final String addressType, final String pinCode) {
        if (BlDeliveryModeLoggingConstants.SAME_DAY_DELIVERY.equals(shippingGroup) ||
                BlDeliveryModeLoggingConstants.NEXT_DAY_RUSH_DELIVERY.equals(shippingGroup)) {
            if(!(StringUtils.isNotEmpty(pinCode) && pinCode.equals(rushZip))) {
               return BlDeliveryModeLoggingConstants.PIN_ERROR;
            }
        } else {
            if(businessType && (StringUtils.isEmpty(addressType) || !addressType.equals(AddressTypeEnum.BUSINESS.getCode()))) {
                return BlDeliveryModeLoggingConstants.AM_ERROR;
            }
        }
        return null;
    }

    @GetMapping(value = "/saveDeliveryDetails")
    @RequireHardLogIn
    @ResponseBody
    public String doSaveDeliveryDetails(@RequestParam("deliveryNote") final String deliveryNote,
                                        @RequestParam("statusUpdate") final boolean statusUpdate,
                                          final RedirectAttributes redirectAttributes) {
        return getCheckoutFacade().setDeliveryDetails(deliveryNote, statusUpdate);
    }

    @GetMapping(value = "/back")
    @RequireHardLogIn
    @Override
    public String back(final RedirectAttributes redirectAttributes) {
        return getCheckoutStep().previousStep();
    }

    @GetMapping(value = "/next")
    @RequireHardLogIn
    @Override
    public String next(final RedirectAttributes redirectAttributes) {
        return getCheckoutStep().nextStep();
    }
    
    @GetMapping(value = "/checkAvailability")
 	 @ResponseBody
 	 public String checkDeliveryModeAvailability(@RequestParam("deliveryMethod") final String selectedDeliveryMethod,
 			final Model model) 
 	 {
 	 	 return StringUtils.isNotBlank(selectedDeliveryMethod) 
 	 		 && getCheckoutFacade().checkAvailabilityForDeliveryMode(selectedDeliveryMethod) ? BlControllerConstants.SUCCESS : BlControllerConstants.ERROR;
 	 }
    
    @GetMapping(value = "/checkShippingBlackout")
 	 @ResponseBody
 	 public String checkShippingBlackout(@RequestParam("deliveryMethod") final String selectedDeliveryMethod,
 			final Model model) 
 	 {
 	 	 return StringUtils.isNotBlank(selectedDeliveryMethod) 
 	 		 && getCheckoutFacade().isShippingOnBlackoutDate(selectedDeliveryMethod) ? BlControllerConstants.ERROR : BlControllerConstants.SUCCESS;
 	 }

    /**
     * Will set the delivery address
     *
     * @param selectedAddressData
     */
    protected void setDeliveryAddress(final AddressData selectedAddressData) {
        final AddressData cartCheckoutDeliveryAddress = getCheckoutFacade().getCheckoutCart().getDeliveryAddress();
        if (isAddressIdChanged(cartCheckoutDeliveryAddress, selectedAddressData)) {
            getCheckoutFacade().setDeliveryAddress(selectedAddressData);
            if (cartCheckoutDeliveryAddress != null && !cartCheckoutDeliveryAddress.isVisibleInAddressBook()) {
                // temporary address should be removed
                getUserFacade().removeAddress(cartCheckoutDeliveryAddress);
            }
        }
    }

    protected void processAddressVisibilityAndDefault(final AddressForm addressForm, final AddressData newAddress) {
        if (addressForm.getSaveInAddressBook() != null) {
            newAddress.setVisibleInAddressBook(addressForm.getSaveInAddressBook().booleanValue());
            if (addressForm.getSaveInAddressBook().booleanValue() && CollectionUtils.isEmpty(getUserFacade().getAddressBook())) {
                newAddress.setDefaultAddress(true);
            }
        } else if (getCheckoutCustomerStrategy().isAnonymousCheckout()) {
            newAddress.setDefaultAddress(true);
            newAddress.setVisibleInAddressBook(true);
        }
    }

    protected void populateCommonModelAttributes(final Model model, final CartData cartData, final AddressForm addressForm)
            throws CMSItemNotFoundException {
        model.addAttribute(CART_DATA, cartData);
        model.addAttribute("deliveryAddresses", getDeliveryAddresses(cartData.getDeliveryAddress()));
        model.addAttribute("noAddress", Boolean.valueOf(getCheckoutFacade().hasNoDeliveryAddress()));
        model.addAttribute("addressFormEnabled", Boolean.valueOf(getCheckoutFacade().isNewAddressEnabledForCart()));
        model.addAttribute("removeAddressEnabled", Boolean.valueOf(getCheckoutFacade().isRemoveAddressEnabledForCart()));
        model.addAttribute(SHOW_SAVE_TO_ADDRESS_BOOK_ATTR, Boolean.TRUE);
        model.addAttribute(WebConstants.BREADCRUMBS_KEY, getResourceBreadcrumbBuilder().getBreadcrumbs(getBreadcrumbKey()));
        model.addAttribute("metaRobots", "noindex,nofollow");
        if (StringUtils.isNotBlank(addressForm.getCountryIso())) {
            model.addAttribute("regions", getI18NFacade().getRegionsForCountryIso(addressForm.getCountryIso()));
            model.addAttribute("country", addressForm.getCountryIso());
        }
    }


    @GetMapping(value = "/selectReplacementAddress")
    @RequireHardLogIn
    @ResponseBody
    public String doSelectDeliveryAddressForOrderReplaceMent(@RequestParam("selectedAddressCode") final String selectedAddressCode,
        @RequestParam("shippingGroup") final String shippingGroup,
        @RequestParam("deliveryMode") final String deliveryMode,
        @RequestParam("rushZip") final String rushZip,
        @RequestParam("businessType") final boolean businessType,
        final RedirectAttributes redirectAttributes) {
        if (StringUtils.isNotBlank(selectedAddressCode)) {
            final AddressData selectedAddressData = getCheckoutFacade().getDeliveryAddressForCode(selectedAddressCode);
            if (selectedAddressData != null) {
                final String addressType = selectedAddressData.getAddressType();
                final String pinCode = selectedAddressData.getPostalCode();
                String pinError = checkErrorIfAnyBeforeSavingAddress(shippingGroup, businessType, rushZip, addressType, pinCode);
                if (pinError != null) {
                    return pinError;
                }
                setDeliveryAddress(selectedAddressData);
            }
        }
        return SUCCESS;
    }


    protected String getBreadcrumbKey() {
        return "checkout.multi." + getCheckoutStep().getProgressBarId() + ".breadcrumb";
    }


    protected CheckoutStep getCheckoutStep() {
        return getCheckoutStep(DELIVERY_METHOD);
    }

    @Override
    public BlCheckoutFacade getCheckoutFacade() {
        return checkoutFacade;
    }

    public void setCheckoutFacade(BlCheckoutFacade checkoutFacade) {
        this.checkoutFacade = checkoutFacade;
    }



}
