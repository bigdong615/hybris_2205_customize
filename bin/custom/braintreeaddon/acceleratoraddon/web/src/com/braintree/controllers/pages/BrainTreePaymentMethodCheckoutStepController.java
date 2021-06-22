package com.braintree.controllers.pages;

import com.braintree.facade.impl.BrainTreeCheckoutFacade;

import de.hybris.platform.acceleratorstorefrontcommons.forms.SopPaymentDetailsForm;
import de.hybris.platform.commercefacades.user.data.AddressData;
import de.hybris.platform.commercefacades.user.data.CountryData;
import de.hybris.platform.commercefacades.user.data.RegionData;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import de.hybris.platform.acceleratorstorefrontcommons.controllers.util.GlobalMessages;

import com.bl.logging.BlLogger;
import com.bl.storefront.controllers.pages.checkout.steps.PaymentMethodCheckoutStepController;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import static de.hybris.platform.util.localization.Localization.getLocalizedString;

import javax.annotation.Resource;
import javax.validation.Valid;

@Controller
@RequestMapping(value = "/checkout/multi/payment-method/braintree")
public class BrainTreePaymentMethodCheckoutStepController extends PaymentMethodCheckoutStepController
{
  @Resource(name = "brainTreeCheckoutFacade")
  private BrainTreeCheckoutFacade brainTreeCheckoutFacade;
  
  private static final Logger LOG = Logger.getLogger(BrainTreePaymentMethodCheckoutStepController.class);
  
  @RequestMapping(value = "/choose-cc", method = RequestMethod.POST)
  @RequireHardLogIn
  public String doSelectPaymentMethod(@RequestParam("selectedPaymentMethodId") final String selectedPaymentMethodId,
      @RequestParam("selectedPaymentMethodNonce") final String selectedPaymentMethodNonce, final Model model, final RedirectAttributes redirectAttributes)
  {
    try
    {
      if (StringUtils.isNotBlank(selectedPaymentMethodId))
      {
        if (StringUtils.isNotBlank(selectedPaymentMethodNonce))
        {
          brainTreeCheckoutFacade.setPaymentDetails(selectedPaymentMethodId, selectedPaymentMethodNonce);
        }
        else
        {
          getCheckoutFacade().setPaymentDetails(selectedPaymentMethodId);
        }
      }
    }
    catch(final Exception exception)
    {
      BlLogger.logMessage(LOG, Level.ERROR, "Error occured while setting selected creditcard on user", exception);
      GlobalMessages.addFlashMessage(redirectAttributes,GlobalMessages.ERROR_MESSAGES_HOLDER,getLocalizedString("braintree.billing.general.error"),null);
    }
    return getCheckoutStep().currentStep();
  }

  @RequestMapping(value = "/reviewSavedPayment", method = RequestMethod.POST)
  @RequireHardLogIn
  public String doSelectSavedPaymentMethod(@RequestParam("savedCCCardId") final String savedCCCardId,@RequestParam("company_name") final String companyName,
      @RequestParam("savedCCCardNonce") final String savedCCCardNonce, @Valid final SopPaymentDetailsForm sopPaymentDetailsForm, 
      @RequestParam(value = "selected_Billing_Address_Id") final String selectedBillingAddressId, final Model model, final RedirectAttributes redirectAttributes)
  {
    try
    {
      final AddressData newBillingAddressData = interpretResponseAddressData(selectedBillingAddressId, sopPaymentDetailsForm, companyName);
      if (StringUtils.isNotBlank(savedCCCardId))
      {
        if (StringUtils.isNotBlank(savedCCCardNonce))
        {
          brainTreeCheckoutFacade.setPaymentDetails(savedCCCardId, savedCCCardNonce, selectedBillingAddressId, newBillingAddressData);
        }
        else
        {
          getCheckoutFacade().setPaymentDetails(savedCCCardId);
        }
      }
    }
    catch(final Exception exception)
    {
      BlLogger.logMessage(LOG, Level.ERROR, "Error occured while setting selected creditcard on user with updating address", exception);
      GlobalMessages.addFlashMessage(redirectAttributes,GlobalMessages.ERROR_MESSAGES_HOLDER,getLocalizedString("braintree.billing.general.error"),null);
      return getCheckoutStep().currentStep();
    }
    return getCheckoutStep().nextStep();
  }
  
  /**
   * Convert sop response data form to address data.
   *
   * @param selectedAddressId the selected address id
   * @param sopPaymentDetailsForm the sop payment details form
   * @return the address data
   */
  private AddressData interpretResponseAddressData(final String selectedAddressId, final SopPaymentDetailsForm sopPaymentDetailsForm,
      final String companyName)
  {
    if (StringUtils.isBlank(selectedAddressId))
    {
      final AddressData address = new AddressData();
      final CountryData country = new CountryData();
      country.setIsocode(sopPaymentDetailsForm.getBillTo_country());
      address.setCountry(country);
      final RegionData region = new RegionData();
      region.setIsocode(sopPaymentDetailsForm.getBillTo_state());
      address.setRegion(region);
      address.setTitleCode(sopPaymentDetailsForm.getBillTo_titleCode());
      address.setFirstName(sopPaymentDetailsForm.getBillTo_firstName());
      address.setLastName(sopPaymentDetailsForm.getBillTo_lastName());
      address.setCompanyName(companyName);
      address.setTown(sopPaymentDetailsForm.getBillTo_city());
      address.setLine1(sopPaymentDetailsForm.getBillTo_street1());
      address.setLine2(sopPaymentDetailsForm.getBillTo_street2());
      address.setPostalCode(sopPaymentDetailsForm.getBillTo_postalCode());
      address.setEmail(sopPaymentDetailsForm.getBillTo_email());
      address.setPhone(sopPaymentDetailsForm.getBillTo_phoneNumber());
      address.setBillingAddress(Boolean.TRUE);
      address.setShippingAddress(Boolean.FALSE);
      address.setPickStoreAddress(Boolean.FALSE);
      address.setUpsStoreAddress(Boolean.FALSE);
      address.setVisibleInAddressBook(sopPaymentDetailsForm.isSavePaymentInfo());
      return address;
    }
    return null;    
  }
}