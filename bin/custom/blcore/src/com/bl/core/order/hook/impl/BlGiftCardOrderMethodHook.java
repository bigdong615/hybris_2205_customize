package com.bl.core.order.hook.impl;

import com.bl.constants.BlCancelRefundLoggingConstants;
import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.gitfcard.BlGiftCardService;
import de.hybris.platform.commerceservices.order.hook.CommercePlaceOrderMethodHook;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import de.hybris.platform.util.DiscountValue;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import de.hybris.platform.core.enums.OrderStatus;

/**
 * It is a custom implementation of OOTB class {@link CommercePlaceOrderMethodHook} to do the adjustments in case of gift card is applied on the order.
 * @author Neeraj Singh
 */
public class BlGiftCardOrderMethodHook implements CommercePlaceOrderMethodHook {

  private EventService eventService;
  private ModelService modelService;

  @Resource(name = "giftCardService")
  private BlGiftCardService giftCardService;

  private DefaultBlESPEventService defaultBlESPEventService;

  /**
   * {@inheritDoc}
   */
  @Override
  public void afterPlaceOrder(final CommerceCheckoutParameter commerceCheckoutParameter,
      final CommerceOrderResult commerceOrderResult) {
    final OrderModel order = commerceOrderResult.getOrder();
    getModelService().refresh(order);
    ServicesUtil.validateParameterNotNullStandardMessage("order", order);

    createGiftCardForGCOrder(order);

    double discountValue = 0.00D;
    if (CollectionUtils.isNotEmpty(order.getGlobalDiscountValues())) {
      for (final DiscountValue orderDiscounts : order.getGlobalDiscountValues()) {
        discountValue += orderDiscounts.getValue();
      }
    }

    // add tax to total only if order is NET
    double totalplustax = order.getTotalPrice().doubleValue();
    if (Boolean.TRUE.equals(order.getNet())) {
      //commented code can be used while implementing place order using gift card. If not required remove it later on.
      			/*for (final AbstractOrderEntryModel abstractOrderEntryModel : order.getEntries())
      			{
      				final Double oraclePrice = (abstractOrderEntryModel.getTotalPrice() / abstractOrderEntryModel.getQuantity());
      				BigDecimal rounded = new BigDecimal(oraclePrice);
      				rounded = rounded.setScale(2, RoundingMode.HALF_UP);
      				abstractOrderEntryModel.setOraclePrice(rounded.doubleValue());
      				getModelService().save(abstractOrderEntryModel);
      			}
    } else {
      			try
      			{
      				final double total = order.getTotalPrice().doubleValue();
      				final double totalTax = order.getTotalTax().doubleValue();
      				//Calculating discount tax SUS-1210
      				final double discountTax = (discountValue * order.getTaxRate()) / 100.0;
      				final double tax = totalTax - discountTax;
      				double taxRate = tax / (total - tax);
      				BigDecimal bd = new BigDecimal(taxRate);
      				bd = bd.setScale(2, RoundingMode.HALF_UP);
      				taxRate = bd.doubleValue() * 100;
      				               order.setTaxRate(taxRate);
      				order.setTotalTax(tax);

      				for (final AbstractOrderEntryModel abstractOrderEntryModel : order.getEntries())
      				{
      					final Double oraclePrice = (abstractOrderEntryModel.getTotalPrice() / abstractOrderEntryModel.getQuantity())
      							/ (1 + (order.getTaxRate().doubleValue() / 100));
      					BigDecimal rounded = new BigDecimal(oraclePrice);
      					rounded = rounded.setScale(2, RoundingMode.HALF_UP);
      					abstractOrderEntryModel.setOraclePrice(rounded.doubleValue());
      					getModelService().save(abstractOrderEntryModel);
      				}
      			}
      			catch (final Exception e)
      			{
                     LOG.error("Error calculating taxrate", e);
      			}
     }*/
    }
    if(CollectionUtils.isNotEmpty(order.getGiftCard())) {
      giftCardService.calculateGiftCard(order, order.getGrandTotal());

      final List<GiftCardModel> giftCards = order.getGiftCard();
      if (giftCards != null) {
        for (final GiftCardModel giftCard : giftCards) {
          getModelService().refresh(giftCard);
          final List<GiftCardMovementModel> movements = giftCard.getMovements();
          for (final GiftCardMovementModel giftCardMovementModel : movements) {
            if (Boolean.FALSE.equals(giftCardMovementModel.getCommitted())) {
              giftCardMovementModel.setCommitted(Boolean.TRUE);
              giftCardMovementModel.setOrder(order);
              giftCardMovementModel.setRedeemDate(new Date());
              getModelService().save(giftCardMovementModel);
            }
          }
        }
      }
    }
  }
  /**
   * Create gift card for gift card purchase order
   * @param OrderModel
   */
  private void createGiftCardForGCOrder(final OrderModel order) {
    final Optional<AbstractOrderEntryModel> optionalEntry = order.getEntries().stream().findFirst();
    if(order.isGiftCardOrder() && optionalEntry.isPresent()){
      AbstractOrderEntryModel entry = optionalEntry.get();
      final GiftCardModel giftCardPurchase = modelService.create(GiftCardModel.class);
      giftCardPurchase.setActive(Boolean.TRUE);
      giftCardPurchase.setEmail(Boolean.TRUE);
      giftCardPurchase.setIsPurchased(Boolean.TRUE);
      giftCardPurchase.setAmount(order.getGiftCardCost());
      giftCardPurchase.setCustomerEmail(StringUtils.isNotBlank(entry.getRecipientEmail())? entry.getRecipientEmail() : ((CustomerModel)order.getUser()).getUid());
      giftCardPurchase.setName(StringUtils.isNotBlank(entry.getRecipientName())? entry.getRecipientName(): order.getUser().getName());
      giftCardPurchase.setMessage(entry.getRecipientMessage());
      giftCardPurchase.setCurrency(order.getCurrency());
      giftCardPurchase.setCustomer((CustomerModel) order.getUser());
      giftCardPurchase.setCode(giftCardService.getUniqueGiftCodeGenertaor());
      giftCardPurchase.setOrder(Collections.singletonList(order));
      modelService.save(giftCardPurchase);
      getModelService().refresh(giftCardPurchase);
      order.setStatus(OrderStatus.RECEIVED_IN_VERIFICATION);
      modelService.save(order);
      getModelService().refresh(order);

      // To call OrderConfirmation ESP Event
      getDefaultBlESPEventService().sendOrderConfirmation(order);
    }
  }


  /**
   * {@inheritDoc}
   */
  @Override
  public void beforePlaceOrder(final CommerceCheckoutParameter commerceCheckoutParameter) {
    // not implemented
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void beforeSubmitOrder(final CommerceCheckoutParameter commerceCheckoutParameter,
      final CommerceOrderResult commerceOrderResult) {
    // not implemented
  }

  protected EventService getEventService() {
    return eventService;
  }

  public void setEventService(final EventService eventService) {
    this.eventService = eventService;
  }

  public ModelService getModelService() {
    return modelService;
  }

  public void setModelService(final ModelService modelService) {
    this.modelService = modelService;
  }

  public DefaultBlESPEventService getDefaultBlESPEventService() {
    return defaultBlESPEventService;
  }

  public void setDefaultBlESPEventService(
      DefaultBlESPEventService defaultBlESPEventService) {
    this.defaultBlESPEventService = defaultBlESPEventService;
  }
}

