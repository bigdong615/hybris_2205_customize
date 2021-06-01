package com.bl.core.services.gitfcard.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.services.dao.BLGiftCardDao;
import com.bl.core.services.gitfcard.BlGiftCardService;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.order.CommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * Default implementation class of {@link BlGiftCardService}.
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardService implements BlGiftCardService {

  private static final Logger LOGGER = Logger.getLogger(DefaultBlGiftCardService.class);

  private BLGiftCardDao giftCardDao;
  private BlCartService blCartService;
  private ModelService modelService;
  private CommerceCartCalculationStrategy blCheckoutCartCalculationStrategy;
  private CommonI18NService commonI18NService;

  public static final String REDIRECT_PREFIX = "redirect:";

  /**
     * {@inheritDoc}
     */
  public void calculateGiftCard(final AbstractOrderModel order, double totalplustax) {
    final List<GiftCardModel> giftCards = order.getGiftCard();
    if (!CollectionUtils.isEmpty(giftCards)) {
      order.setGrandTotal(totalplustax);
      int transactionId = 0;
      double giftCardAmount = 0;
      for (final GiftCardModel giftCardModel : giftCards) {
        // refresh the uncommited giftcards before recreate movements
        clearUncommitedMovements(giftCardModel);
        final double giftCardBalanceAmount = calculateGiftCardBalance(giftCardModel);
        // balance is less than total
        if (giftCardBalanceAmount > 0 && giftCardBalanceAmount <= totalplustax && totalplustax > 0) {
          // create movement of balance amount and reduce the amount of total
          totalplustax -= giftCardBalanceAmount;
          final GiftCardMovementModel movement = getModelService().create(GiftCardMovementModel.class);
          movement.setCommited(Boolean.FALSE);
          movement.setAmount((-1 * giftCardBalanceAmount));
          movement.setCurrency(order.getCurrency());
          movement.setGiftCard(giftCardModel);
          movement.setTransactionId(order.getCode() + "_" + ++transactionId);
          getModelService().save(movement);
          giftCardAmount += giftCardBalanceAmount;
        }
        // user full amount
        else if (giftCardBalanceAmount > totalplustax && totalplustax > 0) {
          final GiftCardMovementModel movement = getModelService().create(GiftCardMovementModel.class);
          movement.setCommited(Boolean.FALSE);
          movement.setAmount((-1 * totalplustax));
          movement.setCurrency(order.getCurrency());
          movement.setGiftCard(giftCardModel);
          movement.setTransactionId(order.getCode() + "_" + ++transactionId);
          getModelService().save(movement);
          giftCardAmount += totalplustax;
          totalplustax = 0;
        }
        getModelService().refresh(giftCardModel);
      }
      order.setGiftCardAmount(giftCardAmount);
      order.setTotalPrice(totalplustax);
    } else {
      order.setGiftCardAmount((double) 0);
      order.setGrandTotal((double) 0);
      order.setTotalPrice(totalplustax);
    }
    getModelService().save(order);
    getModelService().refresh(order);
  }

    /**
     *{@inheritDoc}
     */
  @Override
  public boolean applyGiftCard(final String giftCardCode) {
    if (StringUtils.isEmpty(giftCardCode)) {
      return false;
    }
    final CartModel cartModel = getBlCartService().getSessionCart();
    try{
    final GiftCardModel giftCardModel = getGiftCardDao().getGiftCard(giftCardCode);
    if (giftCardModel != null) {
      this.clearUncommitedMovements(giftCardModel);
      clearInactiveCarts(giftCardModel, cartModel);
      if (giftCardModel.getActive() != null) {
        return validateGiftCardAndApply(giftCardModel, cartModel);
      }
    }
  }catch(Exception exception){
      BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR,"Error while applying gift card code {}", giftCardCode, exception);
  }
    return false;
  }

    /**
     * It applies eligible gift card to cart.
     * @param cartModel
     * @param giftCardModel
     * @return boolean value
     */
  private boolean applyGiftCardToCart(final CartModel cartModel, final GiftCardModel giftCardModel) {
    final List<GiftCardModel> giftCardModelList = new ArrayList<>(cartModel.getGiftCard());
    giftCardModelList.add(giftCardModel);
    cartModel.setGiftCard(giftCardModelList);
    cartModel.setCalculated(Boolean.FALSE);
    List<String> appliedGiftCardCodeList;
    if (CollectionUtils.isNotEmpty(cartModel.getAppliedCouponCodes())) {
        appliedGiftCardCodeList = new ArrayList<>(cartModel.getAppliedCouponCodes());
    } else {
        appliedGiftCardCodeList = new ArrayList<>();
    }
    for (GiftCardModel gcCardModel : giftCardModelList) {
        appliedGiftCardCodeList.add(gcCardModel.getCode());
    }
    cartModel.setAppliedCouponCodes(appliedGiftCardCodeList);
    getModelService().save(cartModel);
    final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
    commerceCartParameter.setCart(cartModel);
    commerceCartParameter.setBaseSite(cartModel.getSite());
    commerceCartParameter.setEnableHooks(true);
    commerceCartParameter.setRecalculate(true);
    getBlCheckoutCartCalculationStrategy().calculateCart(commerceCartParameter);
    getModelService().refresh(cartModel);
    return true;
  }

    /**
     * It validates gift card.
     * @param giftCardModel
     * @param cartModel
     * @return boolean value.
     */
  private boolean validateGiftCardAndApply(final GiftCardModel giftCardModel, final CartModel cartModel) {

    if (!Boolean.FALSE.equals(giftCardModel.getActive()) && giftCardModel.getCurrency() != null
        && giftCardModel.getCurrency() != commonI18NService.getCurrentCurrency()) {
      return false;
    }
    if (Boolean.FALSE.equals(isGiftCardNotEligibleToApply(giftCardModel, cartModel))) {
      return false;
    }
    // apply gift card
    return applyGiftCardToCart(cartModel, giftCardModel);
  }

    /**
     * It checks whether gift card eligible to apply or not.
     * @param giftCardModel
     * @param cartModel
     * @return boolean value.
     */
  private boolean isGiftCardNotEligibleToApply(final GiftCardModel giftCardModel, final CartModel cartModel) {

    if (isGiftCardApplied(giftCardModel, cartModel)) {
      // this gift card is applied already do not apply twice
      return false;
    }

    if (isOrderFullyPaid(cartModel)) {
      // order is fully paid do not apply this gift card
      return false;
    }

    if (calculateGiftCardBalance(giftCardModel) <= 0) {
      return false;
    }
    return true;
  }

    /**
     * It checks whether gift card already applied to cart.
     * @param giftCardModel
     * @param cartModel
     * @return boolean value.
     */
  private boolean isGiftCardApplied(final GiftCardModel giftCardModel, final CartModel cartModel) {
    // check if gift card is already applied
    final List<GiftCardModel> giftCardModelList = cartModel.getGiftCard();
    // if cart has no gift card applied
    if (CollectionUtils.isEmpty(giftCardModelList)) {
      return false;
    }
    for (final GiftCardModel giftcard : giftCardModelList) {
      if (giftcard.getCode().equals(giftCardModel.getCode())) {
        return true;
      }
    }
    return false;
  }

    /**
     * It checks whether cart total amount is eligible to apply gift card or not.
     * @param cartModel
     * @return true/false
     */
    private boolean isOrderFullyPaid(final CartModel cartModel) {
    return cartModel.getTotalPrice() <= 0;
  }

    /**
     * It checks whether gift card has enough balance to apply or not.
     * @param giftCardModel
     * @return gift card balance.
     */
  public double calculateGiftCardBalance(final GiftCardModel giftCardModel) {
    double giftCardBalance = 0;
    final List<GiftCardMovementModel> giftCardMovementModelList = giftCardModel.getMovements();
    if (CollectionUtils.isNotEmpty(giftCardMovementModelList)) {
      for (final GiftCardMovementModel giftCardMovementModel : giftCardMovementModelList) {
        giftCardBalance += giftCardMovementModel.getAmount();
      }
    }
    return giftCardBalance;
  }

    /**
     *{@inheritDoc}
     */
  public void clearUncommitedMovements(final GiftCardModel giftCardModel) {
    final List<GiftCardMovementModel> giftCardMovementModelList = giftCardModel.getMovements();
    if (!CollectionUtils.isEmpty(giftCardMovementModelList)) {
      for (final GiftCardMovementModel giftCardMovementModel : giftCardMovementModelList) {
        if (Boolean.FALSE.equals(giftCardMovementModel.getCommited())) {
          getModelService().remove(giftCardMovementModel);
        }
      }
    }
    getModelService().refresh(giftCardModel);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeGiftCard(final String giftCardCode) {
    List<GiftCardModel> giftCardModelList = new ArrayList<>();
    try {
      final GiftCardModel giftCard = getGiftCardDao().getGiftCard(giftCardCode);
      final CartModel cartModel = getBlCartService().getSessionCart();
      if (giftCard != null) {
        final List<GiftCardModel> giftCardPresentInCart = cartModel.getGiftCard();
        this.clearUncommitedMovements(giftCard);
        Collection<String> appliedGiftCardCodes = new ArrayList<>(
            cartModel.getAppliedCouponCodes());
        for (GiftCardModel giftCardModel : giftCardPresentInCart) {
          GiftCardModel gift = null;
          if (!giftCardModel.getCode().equals(giftCardCode)) {
            gift = giftCardModel;
            giftCardModelList.add(gift);
          } else {
            appliedGiftCardCodes.removeIf(gcCode -> gcCode.equals(giftCardModel.getCode()));
          }
        }
        cartModel.setGiftCard(giftCardModelList);
        cartModel.setAppliedCouponCodes(appliedGiftCardCodes);
      }
      cartModel.setCalculated(Boolean.FALSE);
      getModelService().save(cartModel);
      final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
      commerceCartParameter.setCart(cartModel);
      commerceCartParameter.setBaseSite(cartModel.getSite());
      commerceCartParameter.setEnableHooks(true);
      commerceCartParameter.setRecalculate(true);
      getBlCheckoutCartCalculationStrategy().calculateCart(commerceCartParameter);
      getModelService().refresh(cartModel);
    }catch (Exception exception){
      BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR,"Error while removing applied gift card code {}", giftCardCode, exception);
    }
  }

  /**
   *{@inheritDoc}
   */
  @Override
  public boolean validateGiftPlaceOrder(final OrderModel order) {
    if (CollectionUtils.isEmpty(order.getGiftCard())) {
      return false;
    } else {
        return order.getGiftCardAmount().equals(order.getGrandTotal());
    }
  }

  /**
   * Method to clear inactiveCarts from gift card.
   *
   * @param giftCardModel
   * @param currentCart
   */
  private void clearInactiveCarts(final GiftCardModel giftCardModel, final CartModel currentCart) {
    final List<AbstractOrderModel> abstractOrderModelList = giftCardModel.getOrder();
    if (CollectionUtils.isEmpty(abstractOrderModelList)) {
      return;
    }
    final List<AbstractOrderModel> filteredList = abstractOrderModelList.stream()
        .filter(abstractOrder -> StringUtils.equals(abstractOrder.getCode(), currentCart.getCode())
            || abstractOrder instanceof OrderModel)
        .collect(Collectors.toList());
    giftCardModel.setOrder(filteredList);
    getModelService().save(giftCardModel);
    getModelService().refresh(giftCardModel);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public GiftCardModel getGiftCard(final String giftCardCode) {
    try {
      return getGiftCardDao().getGiftCard(giftCardCode);
    }catch (Exception exception){
      BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR,"Error while fetching gift card code {} from backend", giftCardCode, exception);
    }
    return null;
  }

    public BLGiftCardDao getGiftCardDao() {
        return giftCardDao;
    }

    public void setGiftCardDao(BLGiftCardDao giftCardDao) {
        this.giftCardDao = giftCardDao;
    }

    public BlCartService getBlCartService() {
        return blCartService;
    }

    public void setBlCartService(BlCartService blCartService) {
        this.blCartService = blCartService;
    }

    public ModelService getModelService() {
        return modelService;
    }

    public void setModelService(ModelService modelService) {
        this.modelService = modelService;
    }

    public CommerceCartCalculationStrategy getBlCheckoutCartCalculationStrategy() {
        return blCheckoutCartCalculationStrategy;
    }

    public void setBlCheckoutCartCalculationStrategy(
        CommerceCartCalculationStrategy blCheckoutCartCalculationStrategy) {
        this.blCheckoutCartCalculationStrategy = blCheckoutCartCalculationStrategy;
    }

    public CommonI18NService getCommonI18NService() {
        return commonI18NService;
    }

    public void setCommonI18NService(
        CommonI18NService commonI18NService) {
        this.commonI18NService = commonI18NService;
    }
}