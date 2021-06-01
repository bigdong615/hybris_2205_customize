package com.bl.facades.giftcard;

import com.bl.core.model.GiftCardModel;
import org.springframework.ui.Model;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import javax.servlet.http.HttpServletRequest;

/**
 * This class is responsible to handle gift card related functionality.
 * @author Neeraj Singh
 */
public interface BlGiftCardFacade {

    /**
     *  This method is to remove applied gift card from cart.
     * @param giftCardCode
     */
    void removeGiftCard(final String giftCardCode);

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
    //Commented this method, if it will not be required then we can remove it later on.
    // String placeOrder(Model model, HttpServletRequest request, RedirectAttributes redirectModel);

    /**
     * It checks that applied  gift card exist in the system or not.
     * @param giftCardCode
     * @return giftCardModel
     */
    GiftCardModel getGiftCard(final String giftCardCode);

}
