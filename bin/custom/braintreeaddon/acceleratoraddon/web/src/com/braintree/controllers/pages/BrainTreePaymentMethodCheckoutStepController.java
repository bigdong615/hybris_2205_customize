package com.braintree.controllers.pages;

import com.braintree.facade.impl.BrainTreeCheckoutFacade;
import de.hybris.platform.acceleratorstorefrontcommons.annotations.RequireHardLogIn;
import com.bl.storefront.controllers.pages.checkout.steps.PaymentMethodCheckoutStepController;
import org.apache.commons.lang.StringUtils;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import javax.annotation.Resource;

@Controller
@RequestMapping(value = "/checkout/multi/payment-method/braintree")
public class BrainTreePaymentMethodCheckoutStepController extends PaymentMethodCheckoutStepController
{
  @Resource(name = "brainTreeCheckoutFacade")
  private BrainTreeCheckoutFacade brainTreeCheckoutFacade;

  @RequestMapping(value = "/choose-cc", method = RequestMethod.GET)
  @RequireHardLogIn
  public String doSelectPaymentMethod(@RequestParam("selectedPaymentMethodId") final String selectedPaymentMethodId,
      @RequestParam("selectedPaymentMethodNonce") final String selectedPaymentMethodNonce)
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
    return getCheckoutStep().currentStep();
  }
}
