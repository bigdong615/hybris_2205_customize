package com.bl.facades.giftcard;

import org.springframework.ui.Model;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import javax.servlet.http.HttpServletRequest;

/**
 * @author Neeraj Singh
 */
public interface BlGiftCardFacade {

    /**
     *  This method is to remove applied gift card from cart.
     * @param giftCardCode
     */
    void removeGiftcard(final String giftCardCode);

    /**
     * This method is used to apply gift card.
     * @param giftCardCode
     * @return true or false.
     */
    boolean applyGiftCard(final String giftCardCode);

    /**
     * This method is used to place Order.
     * @param redirectModel
     * @param request
     * @param model
     * @return String value
     */
    String placeOrder(Model model, HttpServletRequest request, RedirectAttributes redirectModel);

}
