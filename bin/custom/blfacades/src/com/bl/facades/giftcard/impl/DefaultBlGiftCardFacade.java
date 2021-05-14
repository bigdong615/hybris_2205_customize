package com.bl.facades.giftcard.impl;

import com.bl.core.services.gitfcard.BlGiftCardService;
import com.bl.core.services.gitfcard.impl.DefaultBlGiftCardService;
import com.bl.facades.giftcard.BlGiftCardFacade;
import com.bl.logging.BlLogger;
import com.bl.logging.impl.LogErrorCodeEnum;
import de.hybris.platform.commercefacades.order.CartFacade;
import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.commercefacades.order.data.CartData;
import de.hybris.platform.commercefacades.order.data.OrderData;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.servicelayer.model.ModelService;
import org.apache.log4j.Level;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ui.Model;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;

/**
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardFacade implements BlGiftCardFacade {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultBlGiftCardFacade.class);

    @Resource
    private ModelService modelService;

    @Resource(name = "checkoutFacade")
    private CheckoutFacade checkoutFacade;

    @Resource(name = "cartFacade")
    private CartFacade cartFacade;

    @Resource(name = "giftCardService")
    private BlGiftCardService giftCardService;

    @Resource(name = "checkoutCustomerStrategy")
    private CheckoutCustomerStrategy checkoutCustomerStrategy;

    public static final String REDIRECT_PREFIX = "redirect:";
    protected static final String REDIRECT_URL_ORDER_CONFIRMATION = REDIRECT_PREFIX + "/checkout/orderConfirmation/";

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeGiftcard(String giftCardCode) {
       try {
           giftCardService.removeGiftcard(giftCardCode);
       }catch (Exception exception){
           //BlLogger.logMessage(LOG, Level.ERROR, "Unable to remove gift Card {} from cart",exception);
           LOG.error("Unable to remove gift Card code:" +giftCardCode+ "from cart",exception);
       }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean applyGiftCard(String giftCardCode) {

        return giftCardService.applyGiftCard(giftCardCode);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String placeOrder(Model model, HttpServletRequest request, RedirectAttributes redirectModel) {

        CartData cartData = null;
        cartData = cartFacade.getSessionCart();

        if (!validateAppliedGiftcard(cartData))
        {

            return REDIRECT_PREFIX + "/cart";
        }



        OrderData orderData = null;
        try
        {
            orderData = this.checkoutFacade.placeOrder();

        }
        catch (final Exception e)
        {
            //return enterStep(model, redirectModel);
            return REDIRECT_PREFIX + "/cart";
        }

        return this.redirectToOrderConfirmationPage(orderData);

    }

    /**
     *
     * @param orderData
     * @return
     */
    protected String redirectToOrderConfirmationPage(final OrderData orderData)
    {
        return REDIRECT_URL_ORDER_CONFIRMATION
                + (getCheckoutCustomerStrategy().isAnonymousCheckout() ? orderData.getGuid() : orderData.getCode());
    }

    /**
     * Validate applied gift card.
     * @param cartData
     * @return true or false.
     */
    protected boolean validateAppliedGiftcard(final CartData cartData)
    {
        if (cartData != null)
        {
            final double giftCardDiscount = cartData.getGiftCardDiscount().getValue().doubleValue();
            final double grandTotal = cartData.getGrandTotal().getValue().doubleValue();

            if (giftCardDiscount != 0 && giftCardDiscount == grandTotal)
            {
                // gift card discount eq granttotal
                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            return false;
        }
    }

    public CheckoutCustomerStrategy getCheckoutCustomerStrategy() {
        return checkoutCustomerStrategy;
    }

    public void setCheckoutCustomerStrategy(CheckoutCustomerStrategy checkoutCustomerStrategy) {
        this.checkoutCustomerStrategy = checkoutCustomerStrategy;
    }
}

