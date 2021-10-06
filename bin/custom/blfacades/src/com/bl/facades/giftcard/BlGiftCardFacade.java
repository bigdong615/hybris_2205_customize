package com.bl.facades.giftcard;

import com.bl.core.model.GiftCardModel;

import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;

import java.math.BigDecimal;
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
     * This method is used to apply gift card.
     * @param giftCardCode for current Gift Card
     * @return true or false.
     */
    boolean applyGiftCardForModifyOrder(final String giftCardCode, final AbstractOrderModel orderModel);

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
    
    /**
     *  This method is to remove applied gift card from Modify Payment page.
     * @param giftCardCode for the current Gift Card
     */
    void removeGiftCardforModifyOrder(final String giftCardCode, final AbstractOrderModel orderModel);
    
	 /**
	  * Checks if is gc already applied.
	  *
	  * @param gcCode
	  *           the gc code
	  * @param orderModel
	  *           the order model
	  * @return true, if is gc already applied
	  */
	 boolean isGcAlreadyApplied(final String gcCode, final AbstractOrderModel orderModel);

	 /**
	  * Gets the gc remaining balanace.
	  *
	  * @param giftCard
	  *           the gift card
	  * @return the gc remaining balanace
	  */
	 double getGcRemainingBalanace(final GiftCardModel giftCard);

	 /**
	  * Checks if is modified amount is fully paid.
	  *
	  * @param orderModel
	  *           the order model
	  * @param amountToPay
	  *           the amount to pay
	  * @return the big decimal
	  */
	 BigDecimal isModifiedAmountIsFullyPaid(final AbstractOrderModel orderModel, final BigDecimal amountToPay);

	 /**
	  * Apply gift card for modified order payment.
	  *
	  * @param gcCode
	  *           the gc code
	  * @param orderModel
	  *           the order model
	  * @param amountToPay
	  *           the amount to pay
	  * @return true, if successful
	  */
	 boolean applyGiftCardForModifiedOrderPayment(final String gcCode, final AbstractOrderModel orderModel,
			 final BigDecimal amountToPay);

	 /**
	  * Removes the gift card for modified order.
	  *
	  * @param giftCardCode
	  *           the gift card code
	  * @param orderModel
	  *           the order model
	  */
	 void removeGiftCardForModifiedOrder(final String giftCardCode, final AbstractOrderModel orderModel);

	 /**
	  * Commit applied gift card.
	  *
	  * @param orderModel
	  *           the order model
	  */
	 void commitAppliedGiftCard(final AbstractOrderModel orderModel);

}
