package com.bl.core.order.hook.impl;

import com.bl.core.esp.service.impl.DefaultBlESPEventService;
import com.bl.core.model.GiftCardModel;
import com.bl.core.model.GiftCardMovementModel;
import com.bl.core.services.gitfcard.BlGiftCardService;
import de.hybris.platform.commerceservices.order.hook.CommercePlaceOrderMethodHook;
import de.hybris.platform.commerceservices.service.data.CommerceCheckoutParameter;
import de.hybris.platform.commerceservices.service.data.CommerceOrderResult;
import de.hybris.platform.core.enums.OrderStatus;
import de.hybris.platform.core.model.order.AbstractOrderEntryModel;
import de.hybris.platform.core.model.order.OrderModel;
import de.hybris.platform.core.model.user.CustomerModel;
import de.hybris.platform.servicelayer.event.EventService;
import de.hybris.platform.servicelayer.model.ModelService;
import de.hybris.platform.servicelayer.util.ServicesUtil;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import javax.annotation.Resource;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;

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

    final CustomerModel customerModel = (CustomerModel) order.getUser();
    setOrderCountForCustomer(customerModel);
    setAverageOrderValue(customerModel);
    setOrderValuePriorToShippedStatus(order, customerModel);

    // add tax to total only if order is NET
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
   * It updates the orderValuePriorToShippedStatus attribute on customer model whenever a new order is created
   * @param order the order
   * @param customerModel the customer
   */
  private void setOrderValuePriorToShippedStatus(final OrderModel order, final CustomerModel customerModel) {
    final Double priceOfProducts = order.getEntries().stream().mapToDouble(AbstractOrderEntryModel::getTotalPrice).sum();
    customerModel.setOrderValuePriorToShippedStatus(customerModel.getOrderValuePriorToShippedStatus() + priceOfProducts);
    getModelService().save(customerModel);
  }

  /**
   * It updates the average order value on customer model whenever a new order is created
   * @param customerModel the customer
   */
  private void setAverageOrderValue(final CustomerModel customerModel) {
    final Collection<OrderModel> orders = customerModel.getOrders();
    final Double averageOrderValue = orders.stream().mapToDouble(OrderModel::getTotalPrice).sum();
    customerModel.setAverageOrderValue(averageOrderValue/orders.size());
    getModelService().save(customerModel);
  }

  /**
   * It updates the order count on customer model whenever a new order is created
   * @param customerModel the customer
   */
  private void setOrderCountForCustomer(final CustomerModel customerModel) {
    customerModel.setOrderCount(customerModel.getOrderCount() + 1);
    getModelService().save(customerModel);
  }

  /**
   * Create gift card for gift card purchase order
   * @param order the order model
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

