package com.bl.core.services.gitfcard.impl;

import com.bl.core.constants.BlCoreConstants;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.cart.BlCartService;
import com.bl.core.services.dao.BlGiftCardDao;
import com.bl.core.services.gitfcard.BlGiftCardService;
import com.bl.logging.BlLogger;
import de.hybris.platform.commerceservices.order.CommerceCartCalculationStrategy;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.exceptions.ModelRemovalException;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import de.hybris.platform.commerceservices.service.data.CommerceOrderParameter;

/**
 * Default implementation class of {@link BlGiftCardService}.
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardService implements BlGiftCardService {

  private static final Logger LOGGER = Logger.getLogger(DefaultBlGiftCardService.class);

  private BlGiftCardDao giftCardDao;
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
    if (CollectionUtils.isNotEmpty(giftCards)) {
      order.setGrandTotal(totalplustax);
      int transactionId = 0;
      double giftCardAmount = 0;
      for (final GiftCardModel giftCardModel : giftCards) {
        try{
          // refresh the uncommitted giftcards before recreate movements
          clearUncommittedMovements(giftCardModel);
          final double giftCardBalanceAmount = calculateGiftCardBalance(giftCardModel);
          // balance is less than total
          if (giftCardBalanceAmount > 0 && giftCardBalanceAmount <= totalplustax && totalplustax > 0) {
            // create movement of balance amount and reduce the amount of total
            totalplustax -= giftCardBalanceAmount;
            final GiftCardMovementModel movement = getModelService().create(GiftCardMovementModel.class);
            movement.setCommitted(Boolean.FALSE);
            movement.setAmount((-1 * giftCardBalanceAmount));
             Double movementAmount = 0.0;
             Double giftCardBalance = 0.0;
            if(null != movement.getAmount()){
              movementAmount = movement.getAmount();
            }
            if(null != giftCardModel.getBalance()){
              giftCardBalance = giftCardModel.getBalance();
            }
            movement.setBalanceAmount(movementAmount + giftCardBalance); // To set balance amount for every movement
            movement.setCurrency(order.getCurrency());
            movement.setGiftCard(giftCardModel);
            movement.setTransactionId(order.getCode() + "_" + ++transactionId);
            getModelService().save(movement);
            giftCardAmount += giftCardBalanceAmount;
          }
          // user full amount
          else if (giftCardBalanceAmount > totalplustax && totalplustax > 0) {
            final GiftCardMovementModel movement = getModelService().create(GiftCardMovementModel.class);
            movement.setCommitted(Boolean.FALSE);
            movement.setAmount((-1 * totalplustax));
            Double movementAmount = 0.0;
            Double giftCardBalance = 0.0;
            if(null != movement.getAmount()){
              movementAmount = movement.getAmount();
            }
            if(null != giftCardModel.getBalance()){
              giftCardBalance = giftCardModel.getBalance();
            }
            movement.setBalanceAmount(movementAmount + giftCardBalance); // To set balance amount for every movement
            movement.setCurrency(order.getCurrency());
            movement.setGiftCard(giftCardModel);
            movement.setTransactionId(order.getCode() + "_" + ++transactionId);
            getModelService().save(movement);
            giftCardAmount += totalplustax;
            totalplustax = 0;
          }
            getModelService().refresh(giftCardModel);
        }catch(final Exception exception){
            BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR,"Exception occurred while doing calculation for a gift card for the cart: {}", order.getCode(), exception);
        }
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
    try {
      final GiftCardModel giftCardModel = getGiftCardDao().getGiftCard(giftCardCode);
      if (giftCardModel != null && cartModel != null) {
        this.clearUncommittedMovements(giftCardModel);
        clearInactiveCarts(giftCardModel, cartModel);
        return validateGiftCardAndApply(giftCardModel, cartModel);
      }
    } catch (final Exception exception) {
      BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR, "Error while applying gift card code {}",
          giftCardCode, exception);
    }
    return false;
  }
  
  /**
   *{@inheritDoc}
   */
@Override
public boolean applyGiftCardForModifyOrder(final String giftCardCode, final AbstractOrderModel orderModel) {
  if (StringUtils.isEmpty(giftCardCode)) {
    return false;
  }
  
  try {
    final GiftCardModel giftCardModel = getGiftCardDao().getGiftCard(giftCardCode);
    if (giftCardModel != null && orderModel != null) {
      this.clearUncommittedMovements(giftCardModel);
      clearInactiveCartsForModifyOrder(giftCardModel, orderModel);
      return validateGiftCardAndApplyForModifyOrder(giftCardModel, orderModel);
    }
  } catch (final Exception exception) {
    BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR, "Error while applying gift card code for modify order {}",
        giftCardCode, exception);
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
   * It applies eligible gift card to cart.
   * @param orderModel current order
   * @param giftCardModel current gift card
   * @return boolean value
   */
private boolean applyGiftCardToModifyOrder(final AbstractOrderModel orderModel, final GiftCardModel giftCardModel) {
  final List<GiftCardModel> giftCardModelList = new ArrayList<>(orderModel.getGiftCard());
  giftCardModelList.add(giftCardModel);
  orderModel.setGiftCard(giftCardModelList);
  orderModel.setCalculated(Boolean.FALSE);
  getModelService().save(orderModel);
  final CommerceOrderParameter commerceCartParameter = new CommerceOrderParameter();
  commerceCartParameter.setOrder(orderModel);
 
   double originalOrderTotal =  orderModel.getTotalPrice();
    if(giftCardModelList.size() == 1){
      saveOriginalOrderTotal(originalOrderTotal, orderModel);
    }
    calculateGiftCard(orderModel, orderModel.getOrderTotalBeforeGCAdd());
  getModelService().refresh(orderModel);
  return true;
}

 /**
  * This method is use to store original orderTotal temp. before applying gift card. 
 * @param originalOrderTotal for order total
 * @param orderModel current order
 */
private void saveOriginalOrderTotal(final double originalOrderTotal, final AbstractOrderModel orderModel) {
	 orderModel.setOrderTotalBeforeGCAdd(originalOrderTotal);
	 getModelService().save(orderModel);
 }
    /**
     * It validates gift card.
     * @param giftCardModel
     * @param cartModel
     * @return boolean value.
     */
  private boolean validateGiftCardAndApply(final GiftCardModel giftCardModel, final CartModel cartModel) {

    if (Boolean.FALSE.equals(giftCardModel.getActive()) && giftCardModel.getCurrency() != null
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
   * It validates gift card.
   * @param giftCardModel for the GiftCard
   * @param orderModel for the current order
   * @return boolean value.
   */
private boolean validateGiftCardAndApplyForModifyOrder(final GiftCardModel giftCardModel, final AbstractOrderModel orderModel) {

  if (Boolean.FALSE.equals(giftCardModel.getActive()) && giftCardModel.getCurrency() != null
      && giftCardModel.getCurrency() != commonI18NService.getCurrentCurrency()) {
    return false;
  }
  if (Boolean.FALSE.equals(isGiftCardNotEligibleToApplyForModifyOrder(giftCardModel, orderModel))) {
    return false;
  }
  // apply gift card
  return applyGiftCardToModifyOrder(orderModel, giftCardModel);
}

    /**
     * It checks whether gift card eligible to apply or not.
     * @param giftCardModel
     * @param cartModel
     * @return boolean value.
     */
  private boolean isGiftCardNotEligibleToApply(final GiftCardModel giftCardModel, final CartModel cartModel) {

    if (isGiftCardApplied(giftCardModel, cartModel)) {
      // this gift card is already applied, it can't be applied twice.
      return false;
    }

    if (isOrderFullyPaid(cartModel)) {
      // order is fully paid, this gift card can't be applied.
      return false;
    }

    return (calculateGiftCardBalance(giftCardModel) > 0);

  }
  
  /**
   * It checks whether gift card eligible to apply or not.
   * @param giftCardModel current giftCard
   * @param orderModel  current order
   * @return boolean value.
   */
private boolean isGiftCardNotEligibleToApplyForModifyOrder(final GiftCardModel giftCardModel, final AbstractOrderModel orderModel) {

  if (isGiftCardAppliedForModifyOrder(giftCardModel, orderModel)) {
    // this gift card is already applied, it can't be applied twice.
    return false;
  }

  if (isOrderFullyPaidForModifyOrder(orderModel)) {
    // order is fully paid, this gift card can't be applied.
    return false;
  }

  return (calculateGiftCardBalance(giftCardModel) > 0);

}
  /**
  * Generate code for gift card purchase.
   * @return String value.
   */
  @Override
  public String getUniqueGiftCodeGenertaor() {
    Random secureRandom = new SecureRandom();
    return secureRandom.ints(16, 0, BlCoreConstants.ALPHANUMERIC_VALUES.length())
        .mapToObj(i -> BlCoreConstants.ALPHANUMERIC_VALUES.charAt(i))
        .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append).toString();
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
   * It checks whether gift card already applied to cart.
   * @param giftCardModel  the Gift Card code
   * @param orderModel current order
   * @return boolean value.
   */
private boolean isGiftCardAppliedForModifyOrder(final GiftCardModel giftCardModel, final AbstractOrderModel orderModel) {
  // check if gift card is already applied
  final List<GiftCardModel> giftCardModelList = orderModel.getGiftCard();
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
     * It checks whether cart total amount is eligible to apply gift card or not.
     * @param orderModel current order
     * @return true/false
     */
    private boolean isOrderFullyPaidForModifyOrder(final AbstractOrderModel orderModel) {
    return orderModel.getTotalPrice() <= 0;
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
  @Override
  public void clearUncommittedMovements(final GiftCardModel giftCardModel) throws ModelRemovalException {
    final List<GiftCardMovementModel> giftCardMovementModelList = giftCardModel.getMovements();
    try {
      if (CollectionUtils.isNotEmpty(giftCardMovementModelList)) {
        for (final GiftCardMovementModel giftCardMovementModel : giftCardMovementModelList) {
          if (Boolean.FALSE.equals(giftCardMovementModel.getCommitted())) {
            getModelService().remove(giftCardMovementModel);
          }
        }
      }
      getModelService().refresh(giftCardModel);
    }catch (final Exception exception){
      BlLogger.logFormatMessageInfo(LOGGER,Level.ERROR,"Exception while removing uncommitted movements for gift card: {}", giftCardModel.getCode());
      throw new ModelRemovalException("Unable to remove uncommitted movement",exception);
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeGiftCard(final String giftCardCode, final CartModel cartModel) {
    List<GiftCardModel> giftCardModelList = new ArrayList<>();
    final GiftCardModel giftCard = getGiftCardDao().getGiftCard(giftCardCode);
    if (giftCard != null) {
        final Collection<GiftCardModel> giftCardPresentInCart = new ArrayList<>(cartModel.getGiftCard());
        this.clearUncommittedMovements(giftCard);
        for (GiftCardModel giftCardModel : giftCardPresentInCart) {
          //Remove gift card, if already present in current cart.
          if (giftCardModel.getCode().equals(giftCardCode)) {
             giftCardPresentInCart.removeIf(gcCode -> gcCode.getCode().equals(giftCard.getCode()));
             break;
          }
        }
        giftCardModelList.addAll(giftCardPresentInCart);
        cartModel.setGiftCard(giftCardModelList);
        cartModel.setCalculated(Boolean.FALSE);
        getModelService().save(cartModel);
        final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
        commerceCartParameter.setCart(cartModel);
        commerceCartParameter.setBaseSite(cartModel.getSite());
        commerceCartParameter.setEnableHooks(true);
        commerceCartParameter.setRecalculate(true);
        getBlCheckoutCartCalculationStrategy().calculateCart(commerceCartParameter);
        getModelService().refresh(cartModel);
    }
  }

  
  /**
   * {@inheritDoc}
   */
  @Override
  public void removeGiftCardForModifyOrder(final String giftCardCode, final AbstractOrderModel orderModel) {
    List<GiftCardModel> giftCardModelList = new ArrayList<>();
    final GiftCardModel giftCard = getGiftCardDao().getGiftCard(giftCardCode);
    if (giftCard != null) {
        final Collection<GiftCardModel> giftCardPresentInCart = new ArrayList<>(orderModel.getGiftCard());
        this.clearUncommittedMovements(giftCard);
        for (GiftCardModel giftCardModel : giftCardPresentInCart) {
          //Remove gift card, if already present in current cart.
          if (giftCardModel.getCode().equals(giftCardCode)) {
             giftCardPresentInCart.removeIf(gcCode -> gcCode.getCode().equals(giftCard.getCode()));
             break;
          }
        }
        giftCardModelList.addAll(giftCardPresentInCart);
        orderModel.setGiftCard(giftCardModelList);
        orderModel.setCalculated(Boolean.FALSE);
        getModelService().save(orderModel);
        final CommerceOrderParameter commerceCartParameter = new CommerceOrderParameter();
        commerceCartParameter.setOrder(orderModel);
        final double originalOrderTotal =  orderModel.getOrderTotalBeforeGCAdd();
        calculateGiftCard(orderModel, originalOrderTotal);
        getModelService().refresh(orderModel);
        
    }
  }
  /**
   *{@inheritDoc}
   */
  @Override
  public boolean validateGiftCardBeforePlaceOrder(final OrderModel order) {
    return CollectionUtils.isNotEmpty(order.getGiftCard()) && order.getGiftCardAmount() != null &&
        order.getGiftCardAmount().compareTo(order.getGrandTotal()) == 0;
  }

  /**
   * Method to clear inactiveCarts from gift card.
   *
   * @param giftCardModel
   * @param currentCart
   */
  private void clearInactiveCarts(final GiftCardModel giftCardModel, final CartModel currentCart) {
    List<AbstractOrderModel> abstractOrderModelList = new ArrayList<>(giftCardModel.getOrder());
    if (CollectionUtils.isEmpty(abstractOrderModelList)) {
      return;
    }
    abstractOrderModelList.add(currentCart);
    giftCardModel.setOrder(abstractOrderModelList);
    getModelService().save(giftCardModel);
    getModelService().refresh(giftCardModel);
  }

  /**
   * Method to clear inactiveCarts from gift card.
   *
   * @param giftCardModel current GiftCard
   * @param orderModel current order
   */
  private void clearInactiveCartsForModifyOrder(final GiftCardModel giftCardModel, final AbstractOrderModel orderModel) {
    List<AbstractOrderModel> abstractOrderModelList = new ArrayList<>(giftCardModel.getOrder());
    if (CollectionUtils.isEmpty(abstractOrderModelList)) {
      return;
    }
    abstractOrderModelList.add(orderModel);
    giftCardModel.setOrder(abstractOrderModelList);
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
    }catch (final Exception exception){
      BlLogger.logMessage(LOGGER, Level.ERROR, "Error while fetching gift card code {} from backend", giftCardCode, exception);
    }
    return null;
  }

    public BlGiftCardDao getGiftCardDao() {
        return giftCardDao;
    }

    public void setGiftCardDao(BlGiftCardDao giftCardDao) {
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