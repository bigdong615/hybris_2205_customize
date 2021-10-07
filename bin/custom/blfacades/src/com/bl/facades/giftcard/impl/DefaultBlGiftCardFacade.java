package com.bl.facades.giftcard.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.gitfcard.BlGiftCardService;
import com.bl.facades.cart.BlCartFacade;
import com.bl.facades.giftcard.BlGiftCardFacade;
import com.bl.logging.BlLogger;
import com.google.common.collect.Lists;

import de.hybris.platform.commercefacades.order.CheckoutFacade;
import de.hybris.platform.commerceservices.strategies.CheckoutCustomerStrategy;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.servicelayer.model.ModelService;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.Validate;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * It is a default implementation of {@link BlGiftCardFacade}.
 *
 * @author Neeraj Singh
 */
public class DefaultBlGiftCardFacade implements BlGiftCardFacade {

  private static final Logger LOGGER = Logger.getLogger(DefaultBlGiftCardFacade.class);
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
  public void removeGiftCardforModifyOrder(final String giftCardCode, final AbstractOrderModel orderModel) {
    giftCardService.removeGiftCardForModifyOrder(giftCardCode, orderModel);
  }
  
  /**
   * {@inheritDoc}
   */
  @Override
  public boolean applyGiftCard(final String giftCardCode) {
    return giftCardService.applyGiftCard(giftCardCode);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean applyGiftCardForModifyOrder(final String giftCardCode, final AbstractOrderModel orderModel) {
    return giftCardService.applyGiftCardForModifyOrder(giftCardCode, orderModel );
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

  /**
   * {@inheritDoc}
   */
  @Override
  public void removeAppliedGiftCardFromCartOrShippingPage(final CartModel cartModel,
      final List<GiftCardModel> giftCardModelList) {
    for (GiftCardModel giftCardModel : giftCardModelList) {
      try {
        giftCardService.removeGiftCard(giftCardModel.getCode(), cartModel);
      } catch (final Exception exception) {
        BlLogger.logFormatMessageInfo(LOGGER, Level.ERROR,
            "Error occurred while removing applied gift card code: {} from cart: {} for the customer: {}",
            giftCardModel.getCode(), cartModel.getCode(), cartModel.getUser().getUid(),
            exception);
      }
    }
  }
  
  /**
	 * {@inheritDoc}
	 */
	@Override
	public boolean isGcAlreadyApplied(final String gcCode, final AbstractOrderModel orderModel)
	{
		Validate.notNull(orderModel, "Order Must Not Be Null", StringUtils.EMPTY);
		Validate.notBlank(gcCode, "GC Code must not be blank", StringUtils.EMPTY);

		final ArrayList<GiftCardModel> appliedGcList = Lists
				.newArrayList(CollectionUtils.emptyIfNull(orderModel.getTempModifiedOrderAppliedGcList()));
		if (CollectionUtils.isNotEmpty(appliedGcList))
		{
			return appliedGcList.stream().anyMatch(giftCard -> giftCard.getCode().equalsIgnoreCase(gcCode));
		}
		return Boolean.FALSE;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public double getGcRemainingBalanace(final GiftCardModel giftCard)
	{
		getGiftCardService().clearUncommittedMovements(giftCard);
		return getGiftCardService().calculateGiftCardBalance(giftCard);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public BigDecimal isModifiedAmountIsFullyPaid(final AbstractOrderModel orderModel, final BigDecimal amountToPay)
	{
		Validate.notNull(orderModel, "Order Must Not Be Null", StringUtils.EMPTY);
		BigDecimal remainingBalance = amountToPay;
		final ArrayList<GiftCardModel> appliedGC = Lists
				.newArrayList(CollectionUtils.emptyIfNull(orderModel.getTempModifiedOrderAppliedGcList()));
		int transactionId = 0;
		for (final GiftCardModel giftCardModel : appliedGC)
		{
			remainingBalance = calculateGC(orderModel, remainingBalance, giftCardModel, transactionId);
			++transactionId;
		}
		return remainingBalance;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean applyGiftCardForModifiedOrderPayment(final String gcCode, final AbstractOrderModel orderModel,
			final BigDecimal amountToPay)
	{
		Validate.notNull(orderModel, "Order Must Not Be Null", StringUtils.EMPTY);
		Validate.notNull(amountToPay, "Po Amount Must not be null", StringUtils.EMPTY);
		Validate.notBlank(gcCode, "GC Code must not be blank", StringUtils.EMPTY);
		try
		{
			BigDecimal remainingBalance = amountToPay;
			final GiftCardModel giftCard = getGiftCardService().getGiftCard(gcCode);
			if (Objects.nonNull(giftCard))
			{
				final ArrayList<GiftCardModel> appliedGC = Lists
						.newArrayList(CollectionUtils.emptyIfNull(orderModel.getTempModifiedOrderAppliedGcList()));
				appliedGC.add(giftCard);
				int transactionId = 0;
				for (final GiftCardModel giftCardModel : appliedGC)
				{
					remainingBalance = calculateGC(orderModel, remainingBalance, giftCardModel, transactionId);
					++transactionId;
				}
				orderModel.setTempModifiedOrderAppliedGcList(appliedGC);
				getModelService().save(orderModel);
				getModelService().refresh(orderModel);
				BlLogger.logFormatMessageInfo(LOGGER, Level.INFO, "Applied Gift Card : {} for Modified Order Payment :{}", gcCode, orderModel.getCode());
				return Boolean.TRUE;
			}
		}
		catch (final Exception exception)
		{
			BlLogger.logFormattedMessage(LOGGER, Level.ERROR, StringUtils.EMPTY, exception, "Error while applying Gift Card : {} for modified order : {}", 
					gcCode, orderModel.getCode());
		}
		return Boolean.FALSE;
	}

	/**
	 * Calculate Gift Card for Modifed Order Paymnet.
	 *
	 * @param orderModel the order model
	 * @param amountToPay the amount to pay
	 * @param giftCard the gift card
	 * @param transactionId the transaction id
	 * @return the big decimal
	 */
	private BigDecimal calculateGC(final AbstractOrderModel orderModel, final BigDecimal amountToPay, final GiftCardModel giftCard,
			final int transactionId)
	{
		BigDecimal remainingAmountToPay = amountToPay;
		getGiftCardService().clearUncommittedMovements(giftCard);
		final BigDecimal gCBalance = BigDecimal.valueOf(getGiftCardService().calculateGiftCardBalance(giftCard));
		if (gCBalance.compareTo(BigDecimal.valueOf(0)) > 0)
		{
			final GiftCardMovementModel movement = getModelService().create(GiftCardMovementModel.class);
			movement.setCommitted(Boolean.FALSE);
			if (gCBalance.compareTo(amountToPay) <= 0)
			{
				remainingAmountToPay = amountToPay.subtract(gCBalance);
				movement.setAmount((-1 * gCBalance.doubleValue()));
				movement.setBalanceAmount(movement.getAmount() + gCBalance.doubleValue()); // To set balance amount for every movement
			}
			else
			{
				remainingAmountToPay = BigDecimal.valueOf(0.0d);
				movement.setAmount((-1 * amountToPay.doubleValue()));
				movement.setBalanceAmount(movement.getAmount() + gCBalance.doubleValue()); // To set balance amount for every movement         
			}
			movement.setCurrency(orderModel.getCurrency());
			movement.setGiftCard(giftCard);
			movement.setTransactionId(orderModel.getCode() + "_" + UUID.randomUUID().toString() + "_" + transactionId);
			getModelService().save(movement);
			getModelService().refresh(giftCard);
			BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, 
					"Created Movment with Transaction ID : {} with Amount : {} and Balance Amount : {} for Gift Card : {} on Modified Order : {}", 
					movement.getTransactionId(), movement.getAmount(), movement.getBalanceAmount(), giftCard.getCode(), orderModel.getCode());
		}
		return remainingAmountToPay;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void removeGiftCardForModifiedOrder(final String giftCardCode, final AbstractOrderModel orderModel)
	{
		final List<GiftCardModel> giftCardModelList = Lists
				.newArrayList(CollectionUtils.emptyIfNull(orderModel.getTempModifiedOrderAppliedGcList()));
		final List<GiftCardModel> updatedGiftCardModelList = Lists.newArrayList();
		if (CollectionUtils.isNotEmpty(giftCardModelList))
		{
			final Optional<GiftCardModel> giftCardModelOptional = giftCardModelList.stream()
					.filter(giftCard -> giftCard.getCode().equalsIgnoreCase(giftCardCode)).findFirst();
			if (giftCardModelOptional.isPresent())
			{
				final GiftCardModel giftCard = giftCardModelOptional.get();
				getGiftCardService().clearUncommittedMovements(giftCard);
				for (final GiftCardModel gCModel : giftCardModelList)
				{
					if (!giftCard.getPk().toString().equals(gCModel.getPk().toString()))
					{
						updatedGiftCardModelList.add(gCModel);
					}
				}
				orderModel.setTempModifiedOrderAppliedGcList(updatedGiftCardModelList);
				getModelService().save(orderModel);
				getModelService().refresh(orderModel);
				BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, 
						"Removed Gift card : {} from modified order : {}", giftCardCode, orderModel.getCode());
			}
		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void commitAppliedGiftCard(final AbstractOrderModel orderModel)
	{
		final List<GiftCardModel> giftCards = orderModel.getTempModifiedOrderAppliedGcList();
		final List<GiftCardModel> committedGiftCards = Lists.newArrayList();
		if (CollectionUtils.isNotEmpty(giftCards))
		{
			for (final GiftCardModel giftCard : giftCards)
			{
				getModelService().refresh(giftCard);
				final List<GiftCardMovementModel> movements = giftCard.getMovements();
				for (final GiftCardMovementModel giftCardMovementModel : movements)
				{
					if (Boolean.FALSE.equals(giftCardMovementModel.getCommitted()))
					{
						giftCardMovementModel.setCommitted(Boolean.TRUE);
						giftCardMovementModel.setOrder(((OrderModel) orderModel));
						giftCardMovementModel.setRedeemDate(new Date());
						getModelService().save(giftCardMovementModel);
						getModelService().refresh(giftCard);
						BlLogger.logFormatMessageInfo(LOGGER, Level.DEBUG, 
								"Committing Movement : {} on Gift card : {} from modified order : {}", 
								giftCardMovementModel.getTransactionId(), giftCard.getCode(), orderModel.getCode());
					}
				}
				committedGiftCards.add(giftCard);
			}
			orderModel.setModifiedOrderAppliedGcList(committedGiftCards);
			orderModel.setTempModifiedOrderAppliedGcList(Lists.newArrayList());
			getModelService().save(orderModel);
			getModelService().refresh(orderModel);
		}
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

