package com.bl.facades.giftcard;

import com.bl.core.model.GiftCardModel;
import de.hybris.platform.core.model.order.CartModel;
import java.util.List;
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
    void removeGiftCard(final String giftCardCode, final CartModel cartModel);

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
     * It fetches the GiftCardModel
     * @param giftCardCode
     * @return giftCardModel
     */
    GiftCardModel getGiftCard(final String giftCardCode);

    /**
     * It removes applied gift card from cart/shipping page.
     * @param cartModel
     * @param giftCardModelList
     */
    void removeAppliedGiftCardFromCartOrShippingPage(final CartModel cartModel, final List<GiftCardModel> giftCardModelList);
}
