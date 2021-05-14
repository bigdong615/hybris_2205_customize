package com.bl.core.services.gitfcard.impl;

import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.dao.BLGiftCardDao;
import com.bl.core.services.gitfcard.BlGiftCardService;
import com.bl.core.services.order.impl.BLCommerceCartCalculationStrategy;
import com.bl.facades.giftcard.data.BLGiftCardData;
import de.hybris.platform.commercefacades.product.PriceDataFactory;
import de.hybris.platform.commercefacades.product.data.PriceData;
import de.hybris.platform.commercefacades.product.data.PriceDataType;
import de.hybris.platform.commerceservices.service.data.CommerceCartParameter;
import de.hybris.platform.core.model.order.AbstractOrderModel;
import de.hybris.platform.core.model.order.CartModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.order.CartService;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.i18n.CommonI18NService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.user.UserService;
import de.hybris.platform.site.BaseSiteService;
import de.hybris.platform.store.services.BaseStoreService;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;
import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;


/**
 * @author Admin
 *
 */
public class DefaultBlGiftCardService implements BlGiftCardService
{
    private static final Logger LOG = LoggerFactory.getLogger(DefaultBlGiftCardService.class);

    @Resource
    private BLGiftCardDao giftCardDao;

    @Resource
    private CartService cartService;

    @Resource
    private ModelService modelService;

    @Resource
    BaseStoreService baseStoreService;

    @Resource
    BaseSiteService baseSiteService;

    @Resource(name = "blCheckoutCartCalculationStrategy")
    private BLCommerceCartCalculationStrategy blCommerceCartCalculationStrategy;

    @Resource(name = "priceDataFactory")
    private PriceDataFactory priceDataFactory;

    @Resource
    private CommonI18NService commonI18NService;

    @Resource
    private UserService userService;

    @Resource
    private EventService eventService;

    public static final String REDIRECT_PREFIX = "redirect:";
    protected static final String REDIRECT_URL_ORDER_CONFIRMATION = REDIRECT_PREFIX + "/checkout/orderConfirmation/";
    public static final int GIFT_CARD_CODE_LENGTH = 8;


    public void calculateGiftCard(final AbstractOrderModel order, double totalplustax)
    {
        final List<GiftCardModel> giftCards = order.getGiftCard();
        if (!CollectionUtils.isEmpty(giftCards))
        {
            order.setGrandTotal(totalplustax);

            int tid = 0;
            double giftCardAmount = 0;
            for (final GiftCardModel giftCardModel : giftCards) {
                // refresh the uncommited giftcards before recreate movements
                clearUncommitedMovements(giftCardModel);

                final double balance = calculateGiftCardBalance(giftCardModel);

                    // balance is less than total
                    if (balance > 0 && balance <= totalplustax && totalplustax > 0) {
                        // create movement of balance amount and reduce the amount of total
                        totalplustax -= balance;

                        final GiftCardMovementModel movement = modelService.create(GiftCardMovementModel.class);
                        movement.setCommited(Boolean.FALSE);
                        movement.setAmount((-1 * balance));
                        movement.setCurrency(order.getCurrency());
                        movement.setGiftCard(giftCardModel);
                        movement.setTransactionId(order.getCode() + "_" + ++tid);
                        modelService.save(movement);

                        giftCardAmount += balance;
                    }
                    // user full amount
                    else if (balance > totalplustax && totalplustax > 0) {
                        final GiftCardMovementModel movement = modelService.create(GiftCardMovementModel.class);
                        movement.setCommited(Boolean.FALSE);
                        movement.setAmount((-1 * totalplustax));
                        movement.setCurrency(order.getCurrency());
                        movement.setGiftCard(giftCardModel);
                        movement.setTransactionId(order.getCode() + "_" + ++tid);
                        modelService.save(movement);

                        giftCardAmount += totalplustax;
                        totalplustax = 0;
                    }

                    modelService.refresh(giftCardModel);
            }

            order.setGiftCardAmount(giftCardAmount);
            order.setTotalPrice(totalplustax);

        }
        else
            {
            order.setGiftCardAmount((double) 0);
            order.setGrandTotal((double) 0);
            order.setTotalPrice(totalplustax);
        }
        modelService.save(order);
        modelService.refresh(order);
    }


    @Override
    public boolean applyGiftCard(final String giftCardCode)
    {
        if (StringUtils.isEmpty(giftCardCode))
        {
            return false;
        }

        final CartModel cart = cartService.getSessionCart();
        final GiftCardModel giftCard = giftCardDao.getGiftCard(giftCardCode);

        if (giftCard != null)
        {
            this.clearUncommitedMovements(giftCard);
            clearInactiveCarts(giftCard, cart);

            if (giftCard.getActive() != null)
            {
                return validateGiftCardAndApply(giftCard,cart);
            }
        }

        return false;
    }

    private boolean applyGiftCardToCart(CartModel cart, GiftCardModel giftCard) {
        final List<GiftCardModel> auxGiftCards = new ArrayList<>(cart.getGiftCard());
        auxGiftCards.add(giftCard);
        cart.setGiftCard(auxGiftCards);
        cart.setCalculated(Boolean.FALSE);

        List<String> gcCodes;
        if(null != cart.getAppliedCouponCodes()) {
            gcCodes = new ArrayList<>(cart.getAppliedCouponCodes());
        } else {
            gcCodes = new ArrayList<>();
        }
        Iterator gcIterator = auxGiftCards.iterator();
        while (gcIterator.hasNext()) {
            gcCodes.add(((GiftCardModel) gcIterator.next()).getCode());
        }
        cart.setAppliedCouponCodes(gcCodes);
        modelService.save(cart);

        final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
        commerceCartParameter.setCart(cart);
        commerceCartParameter.setBaseSite(cart.getSite());
        commerceCartParameter.setEnableHooks(true);
        commerceCartParameter.setRecalculate(true);
        blCommerceCartCalculationStrategy.calculateCart(commerceCartParameter);

        modelService.refresh(cart);

        return true;
    }

    private boolean validateGiftCardAndApply(final GiftCardModel giftCard, final CartModel cart) {

        if (!Boolean.FALSE.equals(giftCard.getActive()) && giftCard.getCurrency() != null && giftCard.getCurrency() != commonI18NService.getCurrentCurrency())
        {
           return false;
        }
            //User validation
            final String registered = "REGISTERED";
            final CustomerModel currentCustomer = (CustomerModel) userService.getCurrentUser();

        if(Boolean.FALSE.equals(isRegisteredUserAndGiftCardUserNotSame(giftCard, registered, currentCustomer))) {
            return false;
        }

        if(Boolean.FALSE.equals(isGiftCardNotEligibleToApply(giftCard, cart))) {
                 return false;
            }

        // apply gift card
            return applyGiftCardToCart(cart, giftCard);
        }

    private boolean isRegisteredUserAndGiftCardUserNotSame(GiftCardModel giftCard, String registered, CustomerModel currentCustomer) {
        if (currentCustomer.getType() != null && currentCustomer.getType().getCode().equalsIgnoreCase(registered))//registered customer
        {
            if (null != giftCard.getCustomer() && !giftCard.getCustomer().getUid().equals(currentCustomer.getUid())) {
                return false;
            }
        }else {
            return false;
        }
        return true;
    }

    private boolean isGiftCardNotEligibleToApply(GiftCardModel giftCard, CartModel cart) {
        if (isGiftCardApplied(giftCard, cart)) {
            // this gift card is applied already do not apply twice
            return false;
        }

        if (isOrderFullyPaid(cart)) {
            // order is fully paid do not apply this gift card
            return false;
        }

        if (calculateGiftCardBalance(giftCard) <= 0) {
            return false;
        }
        return true;
    }

    public boolean isGiftCardApplied(final GiftCardModel giftCardModel, final CartModel cart)
    {
        // check if gift card is already applied
        final List<GiftCardModel> giftCards = cart.getGiftCard();

        // if cart has no gift card applied
        if (CollectionUtils.isEmpty(giftCards))
        {
            return false;
        }

        for (final GiftCardModel giftcard : giftCards) {
            if (giftcard.getCode().equals(giftCardModel.getCode())) {
                return true;
            }
        }

        return false;
    }

    public boolean isOrderFullyPaid(final CartModel cart)
    {
        return cart.getTotalPrice() <= 0;
    }

    public double calculateGiftCardBalance(final GiftCardModel giftCardModel)
    {
        double balance = 0;
        final List<GiftCardMovementModel> movements = giftCardModel.getMovements();
        if (!CollectionUtils.isEmpty(movements))
        {
            for (final GiftCardMovementModel giftCardMovementModel : movements) {
                balance += giftCardMovementModel.getAmount().doubleValue();
            }
        }

        return balance;
    }

    public void clearUncommitedMovements(final GiftCardModel giftCardModel)
    {
        final List<GiftCardMovementModel> movements = giftCardModel.getMovements();
        if (!CollectionUtils.isEmpty(movements))
        {
            for (final GiftCardMovementModel giftCardMovementModel : movements) {
                if (Boolean.FALSE.equals(giftCardMovementModel.getCommited())) {
                    modelService.remove(giftCardMovementModel);
                }
            }
        }
            modelService.refresh(giftCardModel);
    }

    /**@Override
    public List<BLGiftCardData> popupGiftcard(final String giftCardCode) {

        final List<BLGiftCardData> blGiftCardDataList = new ArrayList<>();
        boolean containGiftcard = false;
        BLGiftCardData giftData = null;
        final PriceDataType priceType = PriceDataType.BUY;
        List<GiftCardModel> giftCards = null;
        final CartModel cart = cartService.getSessionCart();

        if (cart != null && !CollectionUtils.isEmpty(cart.getGiftCard()))
        {
            giftCards = cart.getGiftCard();

            for (GiftCardModel giftCard : giftCards) {
                giftData = new BLGiftCardData();

                final double balance = this.calculateGiftCardBalance(giftCard);

                giftData.setCode(giftCard.getCode());
                giftData.setAmount(giftCard.getAmount());
                giftData.setBalanceamount(balance);


                final PriceData amount = priceDataFactory.create(priceType,
                        BigDecimal.valueOf(giftCard.getAmount().doubleValue()),
                        giftCard.getCurrency().getIsocode());

                giftData.setAmountc(amount);

                final PriceData bamount = priceDataFactory.create(priceType, BigDecimal.valueOf(balance),
                        giftCard.getCurrency().getIsocode());


                giftData.setBalanceamountc(bamount);

                blGiftCardDataList.add(giftData);

            }
        }


        if (!StringUtils.isEmpty(giftCardCode) && !CollectionUtils.isEmpty(giftCards))
            {
                for (GiftCardModel giftCard : giftCards) {
                    if (giftCard.getCode().equals(giftCardCode)) {
                        containGiftcard = true;
                        break;
                    }
                }
            }

            if (!containGiftcard)
            {
                giftData = new BLGiftCardData();
                final GiftCardModel giftCard = giftCardDao.getGiftCard(giftCardCode);

                if (giftCard != null)
                {
                    final double balance = this.calculateGiftCardBalance(giftCard);

                    giftData.setCode(giftCard.getCode());
                    giftData.setAmount(giftCard.getAmount());
                    giftData.setBalanceamount(balance);


                    final PriceData amount = priceDataFactory.create(priceType, BigDecimal.valueOf(giftCard.getAmount().doubleValue()),
                            giftCard.getCurrency().getIsocode());

                    giftData.setAmountc(amount);

                    final PriceData bamount = priceDataFactory.create(priceType, BigDecimal.valueOf(balance),
                            giftCard.getCurrency().getIsocode());


                    giftData.setBalanceamountc(bamount);
                    blGiftCardDataList.add(giftData);
                }

            }

            return blGiftCardDataList;
    }
    */

    /*
     * {@inheritDoc}
    */
    @Override
    public void removeGiftcard(final String giftCardCode)
    {
        List<GiftCardModel> giftList = new ArrayList<>();
        final GiftCardModel giftCard = giftCardDao.getGiftCard(giftCardCode);
        final CartModel cart = cartService.getSessionCart();

        if (giftCard != null)
        {
            final List<GiftCardModel> giftCardList = cart.getGiftCard();
            this.clearUncommitedMovements(giftCard);
            Collection<String> gcCodes = new ArrayList<>(cart.getAppliedCouponCodes());

            for (GiftCardModel giftCardModel : giftCardList) {

                GiftCardModel gift = null;

                if (!giftCardModel.getCode().equals(giftCardCode)) {
                    gift = giftCardModel;
                    giftList.add(gift);
                }else{
                    gcCodes.removeIf(gcCode -> gcCode.equals(giftCardModel.getCode()));
                }
            }

            cart.setGiftCard(giftList);

            cart.setAppliedCouponCodes(gcCodes);
        }

        cart.setCalculated(Boolean.FALSE);
        modelService.save(cart);

        final CommerceCartParameter commerceCartParameter = new CommerceCartParameter();
        commerceCartParameter.setCart(cart);
        commerceCartParameter.setBaseSite(cart.getSite());
        commerceCartParameter.setEnableHooks(true);
        commerceCartParameter.setRecalculate(true);
        blCommerceCartCalculationStrategy.calculateCart(commerceCartParameter);

        modelService.refresh(cart);

    }

    /*
     * (non-Javadoc)
     *
     *
     */
    @Override
    public boolean ValidateGiftplaceOrder(final OrderModel order)
    {

        if (order.getGiftCard().isEmpty())
        {
            return false;
        }
        else{

            return order.getGiftCardAmount() == order.getGrandTotal();
        }


    }

    /**
     * Method to clear inactiveCarts from gift card.
     *
     * @param giftCardModel
     * @param currentCart
     */
    private void clearInactiveCarts(final GiftCardModel giftCardModel, final CartModel currentCart)
    {
        final List<AbstractOrderModel> abstractOrderModels = giftCardModel.getOrder();
        if (CollectionUtils.isEmpty(abstractOrderModels))
        {
            return;
        }
        final List<AbstractOrderModel> filteredList = abstractOrderModels.stream()
                .filter(abstractOrder -> StringUtils.equals(abstractOrder.getCode(), currentCart.getCode())
                        || abstractOrder instanceof OrderModel)
                .collect(Collectors.toList());
        giftCardModel.setOrder(filteredList);
        modelService.save(giftCardModel);
        modelService.refresh(giftCardModel);
    }

    /**
     * @return the modelService
     */
    public ModelService getModelService()
    {
        return modelService;
    }


    /**
     * @param modelService
     *           the modelService to set
     */
    public void setModelService(final ModelService modelService)
    {
        this.modelService = modelService;
    }

    /**
     * @return the eventService
     */
    public EventService getEventService()
    {
        return eventService;
    }


    /**
     * @param eventService
     *           the eventService to set
     */
    public void setEventService(final EventService eventService)
    {
        this.eventService = eventService;
    }

}