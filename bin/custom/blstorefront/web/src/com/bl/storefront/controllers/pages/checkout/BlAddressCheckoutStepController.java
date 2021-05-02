package com.bl.storefront.controllers.pages.checkout;

import com.bl.storefront.forms.BlPickUpByForm;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

public interface BlAddressCheckoutStepController {

    /**
     * This method will save pickup person details on cart
     *
     * @param blPickUpByForm pickup person details
     * @param bindingResult binding attribute
     * @param model model attribute
     * @param redirectModel redirect parameters
     * @return string of success or failure
     */
    String savePickUpByFormOnCart(final BlPickUpByForm blPickUpByForm, final BindingResult bindingResult, final Model model,
                                  final RedirectAttributes redirectModel);

    /**
     * This method will remove added delivery details fromm cart
     *
     * @return error or success
     */
    String removeDeliveryDetailsFromCart();
}
