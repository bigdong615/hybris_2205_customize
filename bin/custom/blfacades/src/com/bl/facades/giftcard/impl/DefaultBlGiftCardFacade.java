package com.bl.facades.giftcard.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.services.gitfcard.BlGiftCardService;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.giftcard.BlGiftCardFacade;
import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.servicelayer.model.ModelService;

/**
 * It is a default implementation of {@link BlGiftCardFacade}.
 *
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardFacade implements BlGiftCardFacade {

  private ModelService modelService;
  private CheckoutFacade checkoutFacade;
  private BlCartFacade blCartFacade;
  private BlGiftCardService giftCardService;
  private CheckoutCustomerStrategy checkoutCustomerStrategy;

  public static final String REDIRECT_PREFIX = "redirect:";
  //protected static final String REDIRECT_URL_ORDER_CONFIRMATION = REDIRECT_PREFIX + "/checkout/orderConfirmation/";

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeGiftCard(final String giftCardCode, final CartModel cartModel) {
    giftCardService.removeGiftCard(giftCardCode, cartModel);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean applyGiftCard(final String giftCardCode) {
    return giftCardService.applyGiftCard(giftCardCode);
  }

  /**
   * Commented this method, we can remove later on if not required.
   * {@inheritDoc}
   */
  /*@Override
  public String placeOrder(Model model, HttpServletRequest request,
      RedirectAttributes redirectModel) {

    CartData cartData = null;
    cartData = getBlCartFacade().getSessionCart();
    if (Boolean.FALSE.equals(validateAppliedGiftcard(cartData))) {
        return REDIRECT_PREFIX + "/cart";
    }
    OrderData orderData = null;
    try {
      orderData = this.checkoutFacade.placeOrder();

    } catch (final Exception e) {
      //return enterStep(model, redirectModel);
      return REDIRECT_PREFIX + "/cart";
    }
    return this.redirectToOrderConfirmationPage(orderData);
  }*/

  /**
   *  Commented this method, we can remove later on if not required.
   * @param orderData
   * @return
   */
  /*protected String redirectToOrderConfirmationPage(final OrderData orderData) {
    return REDIRECT_URL_ORDER_CONFIRMATION
        + (getCheckoutCustomerStrategy().isAnonymousCheckout() ? orderData.getGuid()
        : orderData.getCode());
  }*/

  /**
   * Commented this method, we can remove later on if not required.
   * Validate applied gift card.
   *
   * @param cartData
   * @return true or false.
   */
  /*protected boolean validateAppliedGiftCard(final CartData cartData) {
    if (cartData != null) {
      final double giftCardDiscount = cartData.getGiftCardDiscount().getValue().doubleValue();
      final double grandTotal = cartData.getGrandTotal().getValue().doubleValue();

      if (giftCardDiscount != 0 && giftCardDiscount == grandTotal) {
        // gift card discount eq granttotal
        return true;
      } else {
        return false;
      }
    } else {
      return false;
    }
  }*/

  /**
   * {@inheritDoc}
   */
  @Override
  public GiftCardModel getGiftCard(String giftCardCode) {
    return giftCardService.getGiftCard(giftCardCode);
  }

  public CheckoutCustomerStrategy getCheckoutCustomerStrategy() {
    return checkoutCustomerStrategy;
  }

  public void setCheckoutCustomerStrategy(CheckoutCustomerStrategy checkoutCustomerStrategy) {
    this.checkoutCustomerStrategy = checkoutCustomerStrategy;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(ModelService modelService) {
    this.modelService = modelService;
  }

  public CheckoutFacade getCheckoutFacade() {
    return checkoutFacade;
  }

  public void setCheckoutFacade(CheckoutFacade checkoutFacade) {
    this.checkoutFacade = checkoutFacade;
  }

  public BlCartFacade getBlCartFacade() {
    return blCartFacade;
  }

  public void setBlCartFacade(BlCartFacade blCartFacade) {
    this.blCartFacade = blCartFacade;
  }

  public BlGiftCardService getGiftCardService() {
    return giftCardService;
  }

  public void setGiftCardService(BlGiftCardService giftCardService) {
    this.giftCardService = giftCardService;
  }
}

